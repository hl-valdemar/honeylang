const std = @import("std");
const mem = std.mem;

const Ast = @import("../parser/ast.zig").Ast;
const NodeIndex = @import("../parser/ast.zig").NodeIndex;
const BinaryOp = @import("../parser/ast.zig").BinaryOp;
const TokenList = @import("../lexer/token.zig").TokenList;
const SourceCode = @import("../source/source.zig").SourceCode;

const SymbolTable = @import("symbols.zig").SymbolTable;
const SymbolIndex = @import("symbols.zig").SymbolIndex;
const Scope = @import("symbols.zig").Scope;
const LocalSymbol = @import("symbols.zig").LocalSymbol;
const TypeState = @import("types.zig").TypeState;
const TypeId = @import("types.zig").TypeId;
const SemanticError = @import("error.zig").SemanticError;
const ErrorList = @import("error.zig").ErrorList;

pub const error_printer = @import("error_printer.zig");

pub fn analyze(
    allocator: mem.Allocator,
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
) !SemanticResult {
    var ctx = try SemanticContext.init(allocator, ast, tokens, src);
    return ctx.analyze();
}

pub const TypeError = error{
    OutOfMemory,
};

pub const SemanticResult = struct {
    symbols: SymbolTable,
    errors: ErrorList,
};

pub const SemanticContext = struct {
    allocator: mem.Allocator,

    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,

    symbols: SymbolTable,
    scopes: std.ArrayList(Scope),
    errors: ErrorList,

    current_ret_type: TypeId = .void,

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
            .scopes = try std.ArrayList(Scope).initCapacity(allocator, 4),
            .errors = try ErrorList.init(allocator),
        };
    }

    pub fn analyze(self: *SemanticContext) !SemanticResult {
        // 1. collect all symbols
        try self.collectSymbols();

        // 2. infer types from anchors
        try self.inferTypes();

        // 3. check type compatibility
        try self.checkTypes();

        // 4. finalize (pending → unresolved)
        self.finalizeTypes();

        // cleanup
        for (self.scopes.items) |*scope| {
            scope.deinit();
        }
        self.scopes.deinit(self.allocator);

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
            .var_decl => try self.collectVarDecl(node_idx),
            .func_decl => try self.collectFuncDecl(node_idx),
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
            false, // immutable
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

    fn collectVarDecl(self: *SemanticContext, node_idx: NodeIndex) !void {
        const decl = self.ast.getVarDecl(node_idx);

        // get name
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
            .variable,
            type_state,
            type_id,
            decl.value,
            decl.is_mutable,
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
            false, // mutability doesn't apply to functions
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

    fn checkTypes(self: *SemanticContext) !void {
        const program = self.ast.getProgram(self.ast.root);
        const decls = self.ast.getExtra(program.declarations);

        for (decls) |decl_idx| {
            try self.checkDeclaration(decl_idx);
        }
    }

    fn checkDeclaration(self: *SemanticContext, node_idx: NodeIndex) !void {
        const kind = self.ast.getKind(node_idx);

        switch (kind) {
            .const_decl => try self.checkConstDecl(node_idx),
            .func_decl => try self.checkFuncDecl(node_idx),
            .var_decl => {
                _ = try self.checkVarDecl(node_idx);
            },
            else => {}, // ignore
        }
    }

    fn checkConstDecl(self: *SemanticContext, node_idx: NodeIndex) !void {
        const decl = self.ast.getConstDecl(node_idx);
        const loc = self.ast.getLocation(node_idx);

        // get the declared type from the symbol table
        const name_ident = self.ast.getIdentifier(decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        const sym_idx = self.symbols.lookup(name) orelse return;
        const declared_type = self.symbols.getTypeId(sym_idx);

        // infer type of value expression (no context)
        const expr_type = try self.checkExpression(decl.value, .unresolved);

        // verify that the expression type matches the declared type
        if (declared_type != .unresolved) {
            if (expr_type) |et| {
                if (!typesCompatible(declared_type, et)) {
                    try self.errors.add(.{
                        .kind = .type_mismatch,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
            } else {
                // expression type is unresolved (e.g., literals without anchors)
                // check if the expression structure is compatible with declared type
                if (!self.isExprCompatibleWithType(decl.value, declared_type)) {
                    try self.errors.add(.{
                        .kind = .type_mismatch,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
            }
        }
    }

    fn checkFuncDecl(self: *SemanticContext, node_idx: NodeIndex) !void {
        const decl = self.ast.getFuncDecl(node_idx);

        // get expected return type
        var expected_return_type = TypeId.void;
        if (decl.return_type) |ret_type_idx| {
            const ret_ident = self.ast.getIdentifier(ret_type_idx);
            const ret_token = self.tokens.items[ret_ident.token_idx];
            const ret_name = self.src.getSlice(ret_token.start, ret_token.start + ret_token.len);

            if (resolveTypeName(ret_name)) |tid| {
                expected_return_type = tid;
            }
        }

        // store for return statement check
        const previous_ret_type = self.current_ret_type;
        self.current_ret_type = expected_return_type;
        defer self.current_ret_type = previous_ret_type;

        // push scope for func body
        try self.pushScope();
        defer self.popScope();

        // register params as locals
        const params = self.ast.getExtra(decl.params);
        var i: usize = 0;
        while (i < params.len) : (i += 2) {
            const param_name_idx = params[i];
            const param_type_idx = params[i + 1];

            // get param name
            const param_ident = self.ast.getIdentifier(param_name_idx);
            const param_token = self.tokens.items[param_ident.token_idx];
            const param_name = self.src.getSlice(param_token.start, param_token.start + param_token.len);

            // get param type
            const type_ident = self.ast.getIdentifier(param_type_idx);
            const type_token = self.tokens.items[type_ident.token_idx];
            const type_name = self.src.getSlice(type_token.start, type_token.start + type_token.len);

            const param_type = resolveTypeName(type_name) orelse .unresolved;

            // params are immutable
            try self.declareLocal(param_name, param_type, false);
        }

        // check function body
        try self.checkBlock(decl.body);
    }

    fn checkVarDecl(self: *SemanticContext, node_idx: NodeIndex) !TypeId {
        const decl = self.ast.getVarDecl(node_idx);

        // get declared type if explicit
        var expected_type = TypeId.unresolved;

        if (decl.type_id) |type_idx| {
            const type_ident = self.ast.getIdentifier(type_idx);
            const type_token = self.tokens.items[type_ident.token_idx];
            const type_name = self.src.getSlice(type_token.start, type_token.start + type_token.len);

            if (resolveTypeName(type_name)) |tid| {
                expected_type = tid;
            }
        }

        // check value expression
        const value_type = try self.checkExpression(decl.value, .unresolved);

        // if we have both explicit type and inferred type, verify compatibility
        if (expected_type != .unresolved and value_type != null and value_type.? != .unresolved) {
            if (!typesCompatible(expected_type, value_type.?)) {
                const loc = self.ast.getLocation(node_idx);
                try self.errors.add(.{
                    .kind = .type_mismatch,
                    .start = loc.start,
                    .end = loc.end,
                });
            }
        }

        // return best known type
        if (expected_type != .unresolved) return expected_type;
        if (value_type) |vt| return vt;
        return .unresolved;
    }

    fn checkBlock(self: *SemanticContext, node_idx: NodeIndex) TypeError!void {
        const block = self.ast.getBlock(node_idx);

        const statements = self.ast.getExtra(block.statements);
        for (statements) |stmt_idx| {
            try self.checkStatement(stmt_idx);
        }

        const deferred = self.ast.getExtra(block.deferred);
        for (deferred) |stmt_idx| {
            try self.checkStatement(stmt_idx);
        }
    }

    fn checkStatement(self: *SemanticContext, node_idx: NodeIndex) !void {
        const kind = self.ast.getKind(node_idx);

        switch (kind) {
            .var_decl => {
                _ = try self.checkLocalVarDecl(node_idx);
            },
            .assignment => try self.checkAssignment(node_idx),
            .return_stmt => try self.checkReturn(node_idx),
            .if_stmt => try self.checkIfStmt(node_idx),
            .call_expr => {
                // expression statement, just check the expression
                _ = try self.checkExpression(node_idx, .unresolved);
            },
            .block => {
                // nested block, push a new scope
                try self.pushScope();
                defer self.popScope();
                try self.checkBlock(node_idx);
            },
            else => {},
        }
    }

    fn checkLocalVarDecl(self: *SemanticContext, node_idx: NodeIndex) !void {
        const decl = self.ast.getVarDecl(node_idx);
        const type_id = try self.checkVarDecl(node_idx);

        // get name
        const name_ident = self.ast.getIdentifier(decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        // register in local scope
        try self.declareLocal(name, type_id, decl.is_mutable);
    }

    fn checkAssignment(self: *SemanticContext, node_idx: NodeIndex) !void {
        const assign = self.ast.getAssignment(node_idx);
        const loc = self.ast.getLocation(node_idx);

        // get target name
        const target_ident = self.ast.getIdentifier(assign.target);
        const target_token = self.tokens.items[target_ident.token_idx];
        const target_name = self.src.getSlice(target_token.start, target_token.start + target_token.len);

        // look up target / check locals first, then globals
        var target_type = TypeId.unresolved;
        var is_mutable = false;

        if (self.lookupLocal(target_name)) |local| {
            target_type = local.type_id;
            is_mutable = local.is_mutable;
        } else if (self.symbols.lookup(target_name)) |sym_idx| {
            target_type = self.symbols.getTypeId(sym_idx);
            is_mutable = self.symbols.isMutable(sym_idx);
        } else {
            try self.errors.add(.{
                .kind = .undefined_symbol,
                .start = loc.start,
                .end = loc.end,
            });
            return;
        }

        // check mutability
        if (!is_mutable) {
            try self.errors.add(.{
                .kind = .assignment_to_immutable,
                .start = loc.start,
                .end = loc.end,
            });
        }

        // check value type
        const value_type = try self.checkExpression(assign.value, target_type);
        if (value_type) |vt| {
            if (!typesCompatible(target_type, vt)) {
                try self.errors.add(.{
                    .kind = .type_mismatch,
                    .start = loc.start,
                    .end = loc.end,
                });
            }
        }
    }

    fn checkReturn(self: *SemanticContext, node_idx: NodeIndex) !void {
        const ret = self.ast.getReturn(node_idx);
        const loc = self.ast.getLocation(node_idx);

        const expr_type = try self.checkExpression(ret.expr, self.current_ret_type);

        if (expr_type) |et| {
            if (!typesCompatible(self.current_ret_type, et)) {
                try self.errors.add(.{
                    .kind = .return_type_mismatch,
                    .start = loc.start,
                    .end = loc.end,
                });
            }
        }
    }

    fn checkIfStmt(self: *SemanticContext, node_idx: NodeIndex) !void {
        const if_stmt = self.ast.getIf(node_idx);

        // check main guard is boolean
        try self.checkGuardIsBool(if_stmt.if_guard);

        // check main block (new scope)
        try self.pushScope();
        try self.checkBlock(if_stmt.if_block);
        self.popScope();

        // check else-if branches
        for (0..if_stmt.elseIfCount()) |i| {
            const pair = if_stmt.getElseIf(self.ast, i) orelse continue;

            try self.checkGuardIsBool(pair.guard);

            try self.pushScope();
            try self.checkBlock(pair.block);
            self.popScope();
        }

        // check else block
        if (if_stmt.else_block) |else_blk| {
            try self.pushScope();
            try self.checkBlock(else_blk);
            self.popScope();
        }
    }

    fn checkGuardIsBool(self: *SemanticContext, guard_idx: NodeIndex) !void {
        const guard_type = try self.checkExpression(guard_idx, .bool);
        const loc = self.ast.getLocation(guard_idx);

        if (guard_type) |gt| {
            if (gt != .bool and gt != .unresolved) {
                try self.errors.add(.{
                    .kind = .condition_not_bool,
                    .start = loc.start,
                    .end = loc.end,
                });
            }
        }
    }

    /// Check an expression and return its inferred type.
    /// Returns null if type cannot be determined.
    fn checkExpression(self: *SemanticContext, node_idx: NodeIndex, context_type: TypeId) TypeError!?TypeId {
        const kind = self.ast.getKind(node_idx);

        return switch (kind) {
            .literal => try self.checkLiteral(node_idx, context_type),
            .identifier => try self.checkIdentifier(node_idx),
            .unary_op => try self.checkUnaryOp(node_idx),
            .binary_op => try self.checkBinaryOp(node_idx),
            // .call_expr => try self.checkCallExpression(node_idx, context_type),
            else => null,
        };
    }

    fn checkLiteral(self: *SemanticContext, node_idx: NodeIndex, context_type: TypeId) !?TypeId {
        const lit = self.ast.getLiteral(node_idx);
        const token = self.tokens.items[lit.token_idx];

        if (token.kind == .boolean) {
            return .bool;
        }

        // for numeric literals, use context type if available
        if (context_type.isNumeric()) {
            return context_type;
        }

        // no context, type remains unresolved
        return null;
    }

    fn checkIdentifier(self: *SemanticContext, node_idx: NodeIndex) !?TypeId {
        const ident = self.ast.getIdentifier(node_idx);
        const token = self.tokens.items[ident.token_idx];
        const name = self.src.getSlice(token.start, token.start + token.len);

        // 1. check locals
        if (self.lookupLocal(name)) |local| {
            return local.type_id;
        }

        // 2. check globals
        if (self.symbols.lookup(name)) |sym_idx| {
            return self.symbols.getTypeId(sym_idx);
        }

        // undefined symbol
        try self.errors.add(.{
            .kind = .undefined_symbol,
            .start = token.start,
            .end = token.start + token.len,
        });

        return null;
    }

    fn checkUnaryOp(self: *SemanticContext, node_idx: NodeIndex) !?TypeId {
        const unary_op = self.ast.getUnaryOp(node_idx);
        const loc = self.ast.getLocation(node_idx);

        switch (unary_op.op) {
            .not => {
                // logical not requires bool
                const operand_type = try self.checkExpression(unary_op.operand, .bool);

                if (operand_type) |t| {
                    if (t != .bool and t != .unresolved) {
                        try self.errors.add(.{
                            .kind = .logical_op_requires_bool,
                            .start = loc.start,
                            .end = loc.end,
                        });
                    }
                }

                return .bool;
            },
            .negate => {
                // infer type of operand without context
                const operand_type = try self.checkExpression(unary_op.operand, .unresolved);

                // negation requires numeric type
                if (operand_type) |t| {
                    if (t == .unresolved) {
                        // operand is unresolved, negation is valid
                        // BUT can't determine result type yet
                        return null;
                    }

                    if (!t.isNumeric()) {
                        try self.errors.add(.{
                            .kind = .arithmetic_op_requires_numeric,
                            .start = loc.start,
                            .end = loc.end,
                        });
                        return null;
                    }

                    // can only negate signed types and floats
                    if (t.isInteger() and !t.isSignedInteger()) {
                        try self.errors.add(.{
                            .kind = .cannot_negate_unsigned,
                            .start = loc.start,
                            .end = loc.end,
                        });
                        return null;
                    }

                    return t;
                }

                // operand type unknown (bare literal), result must be signed
                return null;
            },
        }
    }

    fn checkBinaryOp(self: *SemanticContext, node_idx: NodeIndex) !?TypeId {
        const binary_op = self.ast.getBinaryOp(node_idx);
        const loc = self.ast.getLocation(node_idx);

        if (binary_op.isArithmetic()) {
            // infer operand types (no context)
            const left_type = try self.checkExpression(binary_op.left, .unresolved);
            const right_type = try self.checkExpression(binary_op.right, .unresolved);

            const operand_type = left_type orelse right_type;

            if (operand_type) |t| {
                if (!t.isNumeric() and t != .unresolved) {
                    try self.errors.add(.{
                        .kind = .arithmetic_op_requires_numeric,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
                if (left_type != null and right_type != null) {
                    if (!typesCompatible(left_type.?, right_type.?)) {
                        try self.errors.add(.{
                            .kind = .type_mismatch,
                            .start = loc.start,
                            .end = loc.end,
                        });
                    }
                }
                return t;
            }

            // both operands unresolved (e.g. literals), result is numeric but unknown
            return null;
        } else if (binary_op.isComparison()) {
            // infer operand types (no context)
            const left_type = try self.checkExpression(binary_op.left, .unresolved);
            const right_type = try self.checkExpression(binary_op.right, .unresolved);

            // if both sides have resolved types, they must match
            if (left_type != null and right_type != null) {
                if (!typesCompatible(left_type.?, right_type.?)) {
                    try self.errors.add(.{
                        .kind = .type_mismatch,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
            }

            // comparisons always produce bool
            return .bool;
        } else if (binary_op.isLogical()) {
            // infer operand types (no context)
            const left_type = try self.checkExpression(binary_op.left, .unresolved);
            const right_type = try self.checkExpression(binary_op.right, .unresolved);

            // validate right operand.
            // must be explicitly bool, unresolved (runtime check), or structurally compatible (literal true/false)
            const left_ok = if (left_type) |t|
                (t == .bool or t == .unresolved)
            else
                self.isExprCompatibleWithType(binary_op.left, .bool);

            if (!left_ok) {
                try self.errors.add(.{
                    .kind = .logical_op_requires_bool,
                    .start = loc.start,
                    .end = loc.end,
                });
            }

            // validate right operand
            const right_ok = if (right_type) |t|
                (t == .bool or t == .unresolved)
            else
                self.isExprCompatibleWithType(binary_op.right, .bool);

            if (!right_ok) {
                try self.errors.add(.{
                    .kind = .logical_op_requires_bool,
                    .start = loc.start,
                    .end = loc.end,
                });
            }

            // logical ops always produce bool
            return .bool;
        }

        return null;
    }

    // fn checkCallExpression(self: *SemanticContext, node_idx: NodeIndex, context_type: TypeId) !?TypeId {}

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

    fn typesCompatible(left: TypeId, right: TypeId) bool {
        // unresolved types are compatible with everything (errors caught at runtime)
        if (left == .unresolved or right == .unresolved) {
            return true;
        }

        // same type is always compatible
        if (left == right) {
            return true;
        }

        // for now, require exact type
        // NOTE: promotion rules become relevant when implementing pointers (i.e., promotion to const)
        return false;
    }

    /// Check if an unresolved expression can become the target type.
    /// This handles structural constraints like "negation requires signed".
    fn isExprCompatibleWithType(self: *SemanticContext, node_idx: NodeIndex, target_type: TypeId) bool {
        const kind = self.ast.getKind(node_idx);

        return switch (kind) {
            .literal => {
                const lit = self.ast.getLiteral(node_idx);
                const token = self.tokens.items[lit.token_idx];
                if (token.kind == .boolean) {
                    return target_type == .bool;
                }
                // numeric literals can become any numeric type
                return target_type.isNumeric();
            },
            .unary_op => {
                const unary_op = self.ast.getUnaryOp(node_idx);
                return switch (unary_op.op) {
                    .not => target_type == .bool,
                    .negate => target_type.isSignedInteger() or target_type.isFloat(),
                };
            },
            .binary_op => {
                const binary_op = self.ast.getBinaryOp(node_idx);
                if (binary_op.isArithmetic()) {
                    return target_type.isNumeric();
                } else {
                    // in case of comparison and logical ops
                    return target_type == .bool;
                }
            },
            .identifier => true, // already resolved via symbol table
            else => true,
        };
    }

    fn pushScope(self: *SemanticContext) !void {
        try self.scopes.append(self.allocator, Scope.init(self.allocator));
    }

    fn popScope(self: *SemanticContext) void {
        var scope = self.scopes.pop();
        if (scope != null) scope.?.deinit();
    }

    fn declareLocal(self: *SemanticContext, name: []const u8, type_id: TypeId, is_mutable: bool) !void {
        if (self.scopes.items.len == 0) return; // no scope active
        var current = &self.scopes.items[self.scopes.items.len - 1];
        try current.locals.put(name, .{
            .type_id = type_id,
            .is_mutable = is_mutable,
        });
    }

    fn lookupLocal(self: *SemanticContext, name: []const u8) ?LocalSymbol {
        // search from innermost to outermost
        var i = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.scopes.items[i].locals.get(name)) |sym| {
                return sym;
            }
        }
        return null;
    }
};
