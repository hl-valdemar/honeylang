const std = @import("std");
const mem = @import("std").mem;

const SourceCode = @import("../source/source.zig").SourceCode;
const TokenList = @import("../lexer/token.zig").TokenList;
const Ast = @import("../parser/ast.zig").Ast;
const NodeIndex = @import("../parser/ast.zig").NodeIndex;
const SymbolIndex = @import("../semantic/symbols.zig").SymbolIndex;
const SymbolTable = @import("../semantic/symbols.zig").SymbolTable;
const TypeId = @import("../semantic/types.zig").TypeId;
const ComptimeErrorList = @import("error.zig").ComptimeErrorList;

pub const LiteralIndex = u32;

pub const EvalState = enum {
    unevaluated,
    evaluating, // for cycle detection
    evaluated,
};

// explicit error set to avoid inference issues with recursion
pub const EvalError = error{
    OutOfMemory,
    CircularDependency,
    UnsupportedExpression,
    DivisionByZero,
    Overflow,
};

pub const ComptimeResult = struct {
    allocator: mem.Allocator,

    // parallel to symbol table (eval state for each symbol)
    eval_states: std.ArrayList(EvalState),

    // evaluated literal strings (index by symbol index via eval_literal_indices)
    eval_literal_indices: std.ArrayList(LiteralIndex),
    eval_literals: std.ArrayList([]const u8),

    // arena owns the allocated literal strings
    arena: std.heap.ArenaAllocator,

    errors: ComptimeErrorList,

    const NULL_LITERAL: LiteralIndex = std.math.maxInt(LiteralIndex);

    pub fn init(allocator: mem.Allocator, symbol_count: usize) !ComptimeResult {
        const capacity = 100;
        var result = ComptimeResult{
            .allocator = allocator,
            .eval_states = try std.ArrayList(EvalState).initCapacity(allocator, capacity),
            .eval_literal_indices = try std.ArrayList(LiteralIndex).initCapacity(allocator, capacity),
            .eval_literals = try std.ArrayList([]const u8).initCapacity(allocator, capacity),
            .arena = std.heap.ArenaAllocator.init(allocator),
            .errors = try ComptimeErrorList.init(allocator),
        };

        // initialize parallel arrays
        try result.eval_states.appendNTimes(allocator, .unevaluated, symbol_count);
        try result.eval_literal_indices.appendNTimes(allocator, NULL_LITERAL, symbol_count);

        return result;
    }

    pub fn deinit(self: *ComptimeResult) void {
        self.eval_states.deinit(self.allocator);
        self.eval_literal_indices.deinit(self.allocator);
        self.eval_literals.deinit(self.allocator);
        self.arena.deinit(self.allocator);
        self.errors.deinit(self.allocator);
    }

    pub fn getEvalLiteral(self: *const ComptimeResult, idx: SymbolIndex) ?[]const u8 {
        const literal_idx = self.eval_literal_indices.items[idx];
        if (literal_idx == NULL_LITERAL) return null;
        return self.eval_literals.items[literal_idx];
    }
};

pub fn evaluate(
    allocator: mem.Allocator,
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    symbols: *const SymbolTable,
) !ComptimeResult {
    var ctx = ComptimeContext.init(allocator, ast, tokens, src, symbols);
    return ctx.evaluate();
}

pub const ComptimeContext = struct {
    allocator: mem.Allocator,
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    symbols: *const SymbolTable,
    result: ComptimeResult,

    pub fn init(
        allocator: mem.Allocator,
        ast: *const Ast,
        tokens: *const TokenList,
        src: *const SourceCode,
        symbols: *const SymbolTable,
    ) ComptimeContext {
        return .{
            .allocator = allocator,
            .ast = ast,
            .tokens = tokens,
            .src = src,
            .symbols = symbols,
            .result = ComptimeResult.init(allocator, symbols.count()) catch unreachable,
        };
    }

    pub fn deinit(self: *ComptimeContext) void {
        self.result.deinit();
    }

    pub fn evaluate(self: *ComptimeContext) !ComptimeResult {
        // evaluate all constant symbols
        const count = self.symbols.count();
        for (0..count) |i| {
            const idx: SymbolIndex = @intCast(i);

            // skip functions for now
            if (self.symbols.getKind(idx) != .constant) continue;

            // skip unresolved types (they'll trap anyway)
            if (self.symbols.getTypeId(idx) == .unresolved) continue;

            self.evaluateSymbol(idx) catch |err| {
                // record error but continue evaluating other symbols
                try self.result.errors.add(.{
                    .kind = switch (err) {
                        error.CircularDependency => .circular_dependency,
                        error.DivisionByZero => .division_by_zero,
                        error.Overflow => .overflow,
                        else => .evaluation_failed,
                    },
                    .symbol_idx = idx,
                });
            };
        }

        return self.result;
    }

    fn evaluateSymbol(self: *ComptimeContext, sym_idx: SymbolIndex) EvalError!void {
        const state = self.result.eval_states.items[sym_idx];

        switch (state) {
            .evaluated => return,
            .evaluating => {
                // circular dependency
                try self.result.errors.add(.{
                    .kind = .circular_dependency,
                    .symbol_idx = sym_idx,
                });
                return;
            },
            .unevaluated => {},
        }

        // mark as evaluating (for cycle detection)
        self.result.eval_states.items[sym_idx] = .evaluating;

        // get the value expression
        const value_node = self.symbols.getValueNode(sym_idx);
        const type_id = self.symbols.getTypeId(sym_idx);

        // evaluate expression
        const literal = try self.evaluateExpr(value_node, type_id);

        // store result
        if (literal) |lit| {
            const literal_idx: LiteralIndex = @intCast(self.result.eval_literals.items.len);
            try self.result.eval_literals.append(self.allocator, lit);
            self.result.eval_literal_indices.items[sym_idx] = literal_idx;
        }

        self.result.eval_states.items[sym_idx] = .evaluated;
    }

    fn evaluateExpr(self: *ComptimeContext, node_idx: NodeIndex, type_id: TypeId) EvalError!?[]const u8 {
        const kind = self.ast.getKind(node_idx);

        return switch (kind) {
            .literal => self.evaluateLiteral(node_idx, type_id),
            .identifier => self.evaluateIdentifier(node_idx, type_id),
            .unary_op => self.evaluateUnaryOp(node_idx, type_id),
            .binary_op => self.evaluateBinaryOp(node_idx, type_id),
            else => null, // unsupported expression type
        };
    }

    fn evaluateLiteral(self: *ComptimeContext, node_idx: NodeIndex, type_id: TypeId) EvalError!?[]const u8 {
        _ = type_id;

        const lit = self.ast.getLiteral(node_idx);
        const token = self.tokens.items[lit.token_idx];
        const value = self.src.getSlice(token.start, token.start + token.len);

        // just pass through the string
        return value;
    }

    fn evaluateIdentifier(self: *ComptimeContext, node_idx: NodeIndex, type_id: TypeId) EvalError!?[]const u8 {
        _ = type_id;

        const ident = self.ast.getIdentifier(node_idx);
        const token = self.tokens.items[ident.token_idx];
        const name = self.src.getSlice(token.start, token.start + token.len);

        // look up symbol
        const sym_idx = self.symbols.lookup(name) orelse return null;

        // recursively evaluate if necessary
        try self.evaluateSymbol(sym_idx);

        // return evaluated literal
        return self.result.getEvalLiteral(sym_idx);
    }

    fn evaluateUnaryOp(self: *ComptimeContext, node_idx: NodeIndex, type_id: TypeId) EvalError!?[]const u8 {
        const unary_op = self.ast.getUnaryOp(node_idx);
        const operand = try self.evaluateExpr(unary_op.operand, type_id) orelse return null;

        // TODO: implement negation, logical not
        _ = operand;
        _ = unary_op.op;

        return null; // TODO: implement
    }

    fn evaluateBinaryOp(self: *ComptimeContext, node_idx: NodeIndex, type_id: TypeId) EvalError!?[]const u8 {
        const binary_op = self.ast.getBinaryOp(node_idx);

        const left = try self.evaluateExpr(binary_op.left, type_id) orelse return null;
        const right = try self.evaluateExpr(binary_op.right, type_id) orelse return null;

        // TODO: use arbitrary precision arithmetic
        // for now, placeholder that just concatenates for debugging
        _ = left;
        _ = right;
        _ = binary_op.op;

        return null; // TODO: implement
    }
};
