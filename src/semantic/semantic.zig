const std = @import("std");
const mem = std.mem;

const Ast = @import("../parser/ast.zig").Ast;
const NodeIndex = @import("../parser/ast.zig").NodeIndex;
const TokenList = @import("../lexer/token.zig").TokenList;
const SourceCode = @import("../source/source.zig").SourceCode;

const SymbolTable = @import("symbols.zig").SymbolTable;
const SymbolIndex = @import("symbols.zig").SymbolIndex;
const TypeState = @import("types.zig").TypeState;
const TypeId = @import("types.zig").TypeId;
const ErrorList = @import("error.zig").ErrorList;

pub const SemanticResult = struct {
    symbols: SymbolTable,
    errors: ErrorList,
};

pub fn analyze(
    allocator: mem.Allocator,
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
) !SemanticResult {
    var ctx = try SemanticContext.init(allocator, ast, tokens, src);
    return ctx.analyze();
}

pub const SemanticContext = struct {
    allocator: mem.Allocator,
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    symbols: SymbolTable,
    errors: ErrorList,

    pub fn init(
        allocator: mem.Allocator,
        ast: *const Ast,
        tokens: *const TokenList,
        src: *const SourceCode,
    ) !SemanticContext {
        return .{
            .allocator = allocator,
            .ast = ast,
            .tokens = tokens,
            .src = src,
            .symbols = try SymbolTable.init(allocator),
            .errors = try ErrorList.init(allocator),
        };
    }

    pub fn analyze(self: *SemanticContext) !SemanticResult {
        // 1. collect all symbols
        try self.collectSymbols();

        // 2. infer types from anchors
        try self.inferTypes();

        // 3. finalize (pending → unresolved)
        self.finalizeTypes();

        return .{
            .symbols = self.symbols,
            .errors = self.errors,
        };
    }

    fn collectSymbols(self: *SemanticContext) !void {
        const program = self.ast.getProgram(self.ast.root);
        const decls = self.ast.getExtra(program.declarations);

        for (decls) |decl_idx| {
            try self.collectDeclaration(decl_idx);
        }
    }

    fn collectDeclaration(self: *SemanticContext, node_idx: NodeIndex) !void {
        const kind = self.ast.getKind(node_idx);

        switch (kind) {
            .const_decl => try self.collectConstDecl(node_idx),
            .func_decl => {},
            .err => {}, // skip parse errors
            else => {}, // ignore
        }
    }

    fn collectConstDecl(self: *SemanticContext, node_idx: NodeIndex) !void {
        const decl = self.ast.getConstDecl(node_idx);

        // get name from identifier node
        const name_ident = self.ast.getIdentifier(decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        // determine initial type state
        var type_state = TypeState.pending;
        var type_id = TypeId.unresolved;

        if (decl.type_id) |type_idx| {
            // handle explicit type annotation
            const type_ident = self.ast.getIdentifier(type_idx);
            const type_token = self.tokens.items[type_ident.token_idx];
            const type_name = self.src.getSlice(type_token.start, type_token.start + type_token.len);

            if (resolveTypeName(type_name)) |tid| {
                type_id = tid;
                type_state = .resolved;
            } else {
                // unknown type name: record error, leave as pending
                try self.errors.add(.{
                    .kind = .unknown_type,
                    .start = type_token.start,
                    .end = type_token.start + type_token.len,
                });
            }
        }

        // register symbol
        const result = try self.symbols.register(
            name,
            name_token.start,
            .constant,
            type_state,
            type_id,
            decl.value,
        );

        if (result == null) {
            // duplicate symbol
            try self.errors.add(.{
                .kind = .duplicate_symbol,
                .start = name_token.start,
                .end = name_token.start + name_token.len,
            });
        }
    }

    fn collectFuncDecl(self: *SemanticContext, node_idx: NodeIndex) !void {
        const decl = self.ast.getFuncDecl(node_idx);

        // get name
        const name_ident = self.ast.getIdentifier(decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        // functions always have a resolved type (the function type itself)
        // for now, we just mark it as resolved with a placeholder
        // TODO: proper function type representation

        const result = try self.symbols.register(
            name,
            name_token.start,
            .function,
            .resolved,
            .void, // placeholder - functions need proper type representation
            node_idx,
        );

        if (result == null) {
            try self.errors.add(.{
                .kind = .duplicate_symbol,
                .start = name_token.start,
                .end = name_token.start + name_token.len,
            });
        }
    }

    fn resolveTypeName(name: []const u8) ?TypeId {
        const type_map = std.StaticStringMap(TypeId).initComptime(.{
            .{ "void", .void },
            .{ "bool", .bool },
            .{ "u8", .u8 },
            .{ "u16", .u16 },
            .{ "u32", .u32 },
            .{ "u64", .u64 },
            .{ "i8", .i8 },
            .{ "i16", .i16 },
            .{ "i32", .i32 },
            .{ "i64", .i64 },
            .{ "f16", .f16 },
            .{ "f32", .f32 },
            .{ "f64", .f64 },
        });
        return type_map.get(name);
    }

    fn inferTypes(self: *SemanticContext) !void {
        // iterate until no more types can be inferred
        var changed = true;
        while (changed) {
            changed = false;

            const count = self.symbols.count();
            for (0..count) |i| {
                const idx: SymbolIndex = @intCast(i);

                // skip already resolved symbols
                if (self.symbols.getTypeState(idx) == .resolved) continue;

                const value_node = self.symbols.getValueNode(idx);
                if (self.tryInferType(value_node)) |type_id| {
                    self.symbols.resolve(idx, type_id);
                    changed = true;
                }
            }
        }
    }

    /// Attempt to infer the type of an expression node.
    /// Returns null if type cannot be determined yet.
    fn tryInferType(self: *SemanticContext, node_idx: NodeIndex) ?TypeId {
        const kind = self.ast.getKind(node_idx);

        return switch (kind) {
            .literal => self.inferLiteralType(node_idx),
            .identifier => self.inferIdentifierType(node_idx),
            .binary_op => self.inferBinaryOpType(node_idx),
            .unary_op => self.inferUnaryOpType(node_idx),
            else => null,
        };
    }

    fn inferLiteralType(self: *SemanticContext, node_idx: NodeIndex) ?TypeId {
        const lit = self.ast.getLiteral(node_idx);
        const token = self.tokens.items[lit.token_idx];

        // boolean literals always have type bool
        if (token.kind == .boolean) {
            return .bool;
        }

        // numeric literals cannot be inferred without context
        // (they need to be anchored by an explicitly typed value)
        return null;
    }

    fn inferIdentifierType(self: *SemanticContext, node_idx: NodeIndex) ?TypeId {
        const ident = self.ast.getIdentifier(node_idx);
        const token = self.tokens.items[ident.token_idx];
        const name = self.src.getSlice(token.start, token.start + token.len);

        if (self.symbols.lookup(name)) |sym_idx| {
            if (self.symbols.getTypeState(sym_idx) == .resolved) {
                return self.symbols.getTypeId(sym_idx);
            }
        }

        return null;
    }

    fn inferUnaryOpType(self: *SemanticContext, node_idx: NodeIndex) ?TypeId {
        const unary_op = self.ast.getUnaryOp(node_idx);
        return self.tryInferType(unary_op.operand);
    }

    fn inferBinaryOpType(self: *SemanticContext, node_idx: NodeIndex) ?TypeId {
        const binary_op = self.ast.getBinaryOp(node_idx);

        // try to get types from both operands
        const left_type = self.tryInferType(binary_op.left);
        const right_type = self.tryInferType(binary_op.right);

        // find anchor type (first resolved type we find)
        const anchor_type = left_type orelse right_type;

        if (anchor_type) |t| {
            // propagate type back to any pending operands
            self.propagateType(binary_op.left, t);
            self.propagateType(binary_op.right, t);
            return t;
        }

        return null;
    }

    fn propagateType(self: *SemanticContext, node_idx: NodeIndex, type_id: TypeId) void {
        const kind = self.ast.getKind(node_idx);

        switch (kind) {
            .identifier => {
                // look up the symbol and resolve if pending
                const ident = self.ast.getIdentifier(node_idx);
                const token = self.tokens.items[ident.token_idx];
                const name = self.src.getSlice(token.start, token.start + token.len);

                if (self.symbols.lookup(name)) |sym_idx| {
                    if (self.symbols.getTypeState(sym_idx) == .pending) {
                        self.symbols.resolve(sym_idx, type_id);
                    }
                }
            },
            .unary_op => {
                const unary_op = self.ast.getUnaryOp(node_idx);
                self.propagateType(unary_op.operand, type_id);
            },
            .binary_op => {
                const binary_op = self.ast.getBinaryOp(node_idx);
                self.propagateType(binary_op.left, type_id);
                self.propagateType(binary_op.right, type_id);
            },
            else => {},
        }
    }

    fn finalizeTypes(self: *SemanticContext) void {
        // any symbol still pending → unresolved (will trap at runtime)
        const count = self.symbols.count();
        for (0..count) |i| {
            const idx: SymbolIndex = @intCast(i);
            if (self.symbols.getTypeState(idx) == .pending) {
                self.symbols.resolve(idx, .unresolved);
            }
        }
    }
};
