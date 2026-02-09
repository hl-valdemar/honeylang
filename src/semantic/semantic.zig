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
const PrimitiveType = @import("types.zig").PrimitiveType;
const TypeRegistry = @import("types.zig").TypeRegistry;
const SemanticError = @import("error.zig").SemanticError;
const ErrorList = @import("error.zig").ErrorList;

pub const error_printer = @import("error-printer.zig");

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
    types: TypeRegistry,
    node_types: std.AutoHashMapUnmanaged(NodeIndex, TypeId),
    skip_nodes: std.AutoHashMapUnmanaged(NodeIndex, void),
    errors: ErrorList,
};

pub const SemanticContext = struct {
    allocator: mem.Allocator,

    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,

    symbols: SymbolTable,
    types: TypeRegistry,
    node_types: std.AutoHashMapUnmanaged(NodeIndex, TypeId),
    skip_nodes: std.AutoHashMapUnmanaged(NodeIndex, void),
    scopes: std.ArrayList(Scope),
    errors: ErrorList,

    current_ret_type: TypeId = TypeId.void,

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
            .types = try TypeRegistry.init(allocator),
            .node_types = .{},
            .skip_nodes = .{},
            .scopes = try std.ArrayList(Scope).initCapacity(allocator, 4),
            .errors = try ErrorList.init(allocator),
        };
    }

    pub fn analyze(self: *SemanticContext) !SemanticResult {
        // 1. collect all symbols
        try self.collectSymbols();

        // 2. infer types from anchors
        self.inferTypes();

        // 3. check type compatibility
        try self.checkTypes();

        // 4. propagate resolved types back through value expressions
        self.resolveFromContext();

        // 5. finalize (pending → unresolved)
        self.finalizeTypes();

        // 6. check for unused and unresolved symbols
        try self.checkUnusedSymbols();

        // cleanup
        for (self.scopes.items) |*scope| {
            scope.deinit();
        }
        self.scopes.deinit(self.allocator);

        // don't deinit returned data
        return .{
            .symbols = self.symbols,
            .types = self.types,
            .node_types = self.node_types,
            .skip_nodes = self.skip_nodes,
            .errors = self.errors,
        };
    }

    fn checkUnusedSymbols(self: *SemanticContext) !void {
        const count = self.symbols.count();
        for (0..count) |i| {
            const idx: SymbolIndex = @intCast(i);
            const kind = self.symbols.getKind(idx);

            // main is always considered referenced (entry point)
            if (kind == .function) {
                const name = self.symbols.getName(idx, self.src);
                if (std.mem.eql(u8, name, "main")) continue;
            }

            if (!self.symbols.isReferenced(idx)) {
                const start = self.symbols.name_starts.items[idx];
                const len = self.symbols.name_lengths.items[idx];
                const err_kind: @import("error.zig").SemanticErrorKind = switch (kind) {
                    .constant => .unused_constant,
                    .variable => .unused_variable,
                    .function => .unused_function,
                    .type_ => .unused_type,
                };
                try self.errors.add(.{
                    .kind = err_kind,
                    .start = start,
                    .end = start + len,
                });

                // mark for skipping in codegen
                const value_node = self.symbols.getValueNode(idx);
                try self.skip_nodes.put(self.allocator, value_node, {});
            } else {
                // referenced but type unresolved — error
                const type_id = self.symbols.getTypeId(idx);
                if (type_id == .unresolved and kind != .function and kind != .type_) {
                    const start = self.symbols.name_starts.items[idx];
                    const len = self.symbols.name_lengths.items[idx];
                    try self.errors.add(.{
                        .kind = .unresolved_type,
                        .start = start,
                        .end = start + len,
                    });
                }
            }
        }
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
            .struct_decl => try self.collectStructDecl(node_idx),
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
        var type_id: TypeId = .unresolved;

        if (decl.type_id) |type_idx| {
            // handle explicit type annotation
            const type_ident = self.ast.getIdentifier(type_idx);
            const type_token = self.tokens.items[type_ident.token_idx];
            const type_name = self.src.getSlice(type_token.start, type_token.start + type_token.len);

            if (self.resolveTypeName(type_name)) |tid| {
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
        var type_id: TypeId = .unresolved;

        if (decl.type_id) |type_idx| {
            // handle explicit type annotation
            const type_ident = self.ast.getIdentifier(type_idx);
            const type_token = self.tokens.items[type_ident.token_idx];
            const type_name = self.src.getSlice(type_token.start, type_token.start + type_token.len);

            if (self.resolveTypeName(type_name)) |tid| {
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

        // get return type
        const ret_ident = self.ast.getIdentifier(decl.return_type);
        const ret_token = self.tokens.items[ret_ident.token_idx];
        const ret_name = self.src.getSlice(ret_token.start, ret_token.start + ret_token.len);
        const return_type = self.resolveTypeName(ret_name) orelse .unresolved;

        // collect parameter types
        const params = self.ast.getExtra(decl.params);
        const param_count = params.len / 2;

        var param_types = try std.ArrayList(TypeId).initCapacity(self.allocator, param_count);
        defer param_types.deinit(self.allocator);

        var i: usize = 0;
        while (i < params.len) : (i += 2) {
            const param_type_idx = params[i + 1]; // type is second in pair
            const type_ident = self.ast.getIdentifier(param_type_idx);
            const type_token = self.tokens.items[type_ident.token_idx];
            const type_name = self.src.getSlice(type_token.start, type_token.start + type_token.len);

            const param_type = self.resolveTypeName(type_name) orelse .unresolved;
            try param_types.append(self.allocator, param_type);
        }

        // register function type
        const func_type = try self.types.addFunctionType(
            param_types.items,
            return_type,
            decl.call_conv,
        );

        // register symbol
        const result = try self.symbols.register(
            name,
            name_token.start,
            .function,
            .resolved,
            func_type,
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

    fn collectStructDecl(self: *SemanticContext, node_idx: NodeIndex) !void {
        const decl = self.ast.getStructDecl(node_idx);

        // get name
        const name_ident = self.ast.getIdentifier(decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        // parse field pairs from extra_data
        const field_data = self.ast.getExtra(decl.fields);
        const field_count = field_data.len / 2;

        var field_names = try std.ArrayList([]const u8).initCapacity(self.allocator, field_count);
        defer field_names.deinit(self.allocator);
        var field_types = try std.ArrayList(TypeId).initCapacity(self.allocator, field_count);
        defer field_types.deinit(self.allocator);

        var seen_fields = std.StringHashMap(void).init(self.allocator);
        defer seen_fields.deinit();

        var i: usize = 0;
        while (i < field_data.len) : (i += 2) {
            const field_name_idx = field_data[i];
            const field_type_idx = field_data[i + 1];

            // get field name
            const field_ident = self.ast.getIdentifier(field_name_idx);
            const field_token = self.tokens.items[field_ident.token_idx];
            const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);

            // check for duplicate fields
            if (seen_fields.contains(field_name)) {
                try self.errors.add(.{
                    .kind = .duplicate_field,
                    .start = field_token.start,
                    .end = field_token.start + field_token.len,
                });
            } else {
                try seen_fields.put(field_name, {});
            }

            // resolve field type
            const type_ident = self.ast.getIdentifier(field_type_idx);
            const type_token = self.tokens.items[type_ident.token_idx];
            const type_name = self.src.getSlice(type_token.start, type_token.start + type_token.len);

            const field_type = self.resolveTypeName(type_name) orelse blk: {
                try self.errors.add(.{
                    .kind = .unknown_type,
                    .start = type_token.start,
                    .end = type_token.start + type_token.len,
                });
                break :blk TypeId.unresolved;
            };

            try field_names.append(self.allocator, field_name);
            try field_types.append(self.allocator, field_type);
        }

        // register struct type
        const struct_type = try self.types.addStructType(
            name,
            field_names.items,
            field_types.items,
            decl.call_conv,
        );

        // register symbol as type
        const result = try self.symbols.register(
            name,
            name_token.start,
            .type_,
            .resolved,
            struct_type,
            node_idx,
            false,
        );

        if (result == null) {
            try self.errors.add(.{
                .kind = .duplicate_symbol,
                .start = name_token.start,
                .end = name_token.start + name_token.len,
            });
        }
    }

    fn resolveTypeName(self: *const SemanticContext, name: []const u8) ?TypeId {
        // try primitives first
        if (resolvePrimitiveTypeName(name)) |tid| return tid;

        // then check symbol table for struct types
        if (self.symbols.lookup(name)) |sym_idx| {
            if (self.symbols.getKind(sym_idx) == .type_) {
                return self.symbols.getTypeId(sym_idx);
            }
        }

        return null;
    }

    fn resolvePrimitiveTypeName(name: []const u8) ?TypeId {
        const type_map = std.StaticStringMap(TypeId).initComptime(.{
            .{ "void", TypeId.void },
            .{ "bool", TypeId.bool },
            .{ "u8", TypeId.u8 },
            .{ "u16", TypeId.u16 },
            .{ "u32", TypeId.u32 },
            .{ "u64", TypeId.u64 },
            .{ "i8", TypeId.i8 },
            .{ "i16", TypeId.i16 },
            .{ "i32", TypeId.i32 },
            .{ "i64", TypeId.i64 },
            .{ "f16", TypeId.f16 },
            .{ "f32", TypeId.f32 },
            .{ "f64", TypeId.f64 },
        });
        return type_map.get(name);
    }

    fn inferTypes(self: *SemanticContext) void {
        // sub-phase 1: infer types from function contexts (return types, etc.)
        self.inferTypesFromFuncs();

        // sub-phase 2: iterate until no more types can be inferred
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

    fn inferTypesFromFuncs(self: *SemanticContext) void {
        for (0..self.symbols.count()) |i| {
            const idx: SymbolIndex = @intCast(i);
            if (self.symbols.getKind(idx) != .function) continue;

            const func_type_id = self.symbols.getTypeId(idx);
            const return_type = self.types.getReturnType(func_type_id) orelse continue;
            if (return_type.isUnresolved()) continue;

            const func_node = self.symbols.getValueNode(idx);
            const decl = self.ast.getFuncDecl(func_node);

            // external functions have no body
            if (decl.body) |body| {
                self.propagateTypeToReturns(body, return_type);
            }
        }
    }

    fn propagateTypeToReturns(self: *SemanticContext, body_node: NodeIndex, return_type: TypeId) void {
        const block = self.ast.getBlock(body_node);
        const statements = self.ast.getExtra(block.statements);
        for (statements) |stmt| {
            switch (self.ast.getKind(stmt)) {
                .return_stmt => {
                    const ret = self.ast.getReturn(stmt);
                    self.propagateType(ret.expr, return_type);
                },
                .block => self.propagateTypeToReturns(stmt, return_type),
                .if_stmt => {
                    const if_stmt = self.ast.getIf(stmt);
                    self.propagateTypeToReturns(if_stmt.if_block, return_type);
                    for (0..if_stmt.elseIfCount()) |i| {
                        if (if_stmt.getElseIf(self.ast, i)) |pair| {
                            self.propagateTypeToReturns(pair.block, return_type);
                        }
                    }
                    if (if_stmt.else_block) |else_blk| {
                        self.propagateTypeToReturns(else_blk, return_type);
                    }
                },
                else => continue,
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
        if (token.kind == .bool) {
            return TypeId.bool;
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

    /// Propagate resolved types backward through global value expressions.
    /// When a global was resolved via context (e.g. return type), this pass
    /// walks its value expression and resolves any .pending symbols it references.
    fn resolveFromContext(self: *SemanticContext) void {
        var changed = true;
        while (changed) {
            changed = false;

            // count pending symbols before propagation
            var pending_before: usize = 0;
            for (self.symbols.type_states.items) |ts| {
                if (ts == .pending) pending_before += 1;
            }

            // propagate resolved types through value expressions
            for (0..self.symbols.count()) |i| {
                const idx: SymbolIndex = @intCast(i);
                const kind = self.symbols.getKind(idx);
                if (kind == .function or kind == .type_) continue;

                if (self.symbols.getTypeState(idx) == .resolved) {
                    const value_node = self.symbols.getValueNode(idx);
                    if (value_node != 0) {
                        self.propagateType(value_node, self.symbols.getTypeId(idx));
                    }
                }
            }

            // count pending symbols after propagation
            var pending_after: usize = 0;
            for (self.symbols.type_states.items) |ts| {
                if (ts == .pending) pending_after += 1;
            }

            if (pending_after < pending_before) {
                changed = true;
            }
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
            .struct_decl => {}, // already handled in collectStructDecl
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

        // infer type of value expression, passing declared type as context
        const expr_type = try self.checkExpression(decl.value, declared_type);

        // verify that the expression type matches the declared type
        if (!declared_type.isUnresolved()) {
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

        // get function's type from symtable
        const name_ident = self.ast.getIdentifier(decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        const sym_idx = self.symbols.lookup(name) orelse return;
        const func_type_id = self.symbols.getTypeId(sym_idx);

        // get return type from function type and store for codegen
        const expected_return_type = self.types.getReturnType(func_type_id) orelse TypeId.void;
        try self.node_types.put(self.allocator, decl.return_type, expected_return_type);

        // store param types in node_types for codegen
        const params = self.ast.getExtra(decl.params);
        const param_types = self.types.getParamTypes(func_type_id) orelse &[_]TypeId{};

        {
            var i: usize = 0;
            var param_idx: usize = 0;
            while (i < params.len) : ({
                i += 2;
                param_idx += 1;
            }) {
                const param_name_idx = params[i];
                const param_type: TypeId = if (param_idx < param_types.len) param_types[param_idx] else .unresolved;
                try self.node_types.put(self.allocator, param_name_idx, param_type);
            }
        }

        // external functions (no body) — only allowed for foreign calling conventions
        if (decl.body == null) {
            if (decl.call_conv == .honey) {
                const loc = self.ast.getLocation(node_idx);
                try self.errors.add(.{
                    .kind = .missing_function_body,
                    .start = loc.start,
                    .end = loc.end,
                });
            }
            return;
        }

        // store for return statement check
        const previous_ret_type = self.current_ret_type;
        self.current_ret_type = expected_return_type;
        defer self.current_ret_type = previous_ret_type;

        // push scope for func body
        try self.pushScope();
        defer self.popScope();

        // register params as locals
        {
            var i: usize = 0;
            var param_idx: usize = 0;
            while (i < params.len) : ({
                i += 2;
                param_idx += 1;
            }) {
                const param_name_idx = params[i];

                // get param name
                const param_ident = self.ast.getIdentifier(param_name_idx);
                const param_token = self.tokens.items[param_ident.token_idx];
                const param_name = self.src.getSlice(param_token.start, param_token.start + param_token.len);

                // get param type from function signature
                const param_type: TypeId = if (param_idx < param_types.len) param_types[param_idx] else .unresolved;

                // params are immutable
                try self.declareLocal(param_name, param_type, false, param_name_idx);
            }
        }

        // check function body
        try self.checkBlock(decl.body.?);
    }

    fn checkVarDecl(self: *SemanticContext, node_idx: NodeIndex) !TypeId {
        const decl = self.ast.getVarDecl(node_idx);

        // get declared type if explicit
        var expected_type: TypeId = .unresolved;

        if (decl.type_id) |type_idx| {
            const type_ident = self.ast.getIdentifier(type_idx);
            const type_token = self.tokens.items[type_ident.token_idx];
            const type_name = self.src.getSlice(type_token.start, type_token.start + type_token.len);

            if (self.resolveTypeName(type_name)) |tid| {
                expected_type = tid;
            }
        }

        // check value expression, passing declared type as context so literals can resolve
        const value_type = try self.checkExpression(decl.value, expected_type);

        // verify that the expression type matches the declared type
        if (!expected_type.isUnresolved()) {
            if (value_type != null and !value_type.?.isUnresolved()) {
                if (!typesCompatible(expected_type, value_type.?)) {
                    const loc = self.ast.getLocation(node_idx);
                    try self.errors.add(.{
                        .kind = .type_mismatch,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
            } else if (value_type == null) {
                // expression type is unresolved (e.g., literals without anchors)
                if (!self.isExprCompatibleWithType(decl.value, expected_type)) {
                    const loc = self.ast.getLocation(node_idx);
                    try self.errors.add(.{
                        .kind = .type_mismatch,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
            }
        }

        // return best known type
        if (!expected_type.isUnresolved()) return expected_type;
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

        // store resolved type for codegen
        try self.node_types.put(self.allocator, node_idx, type_id);

        // get name
        const name_ident = self.ast.getIdentifier(decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        // register in local scope
        try self.declareLocal(name, type_id, decl.is_mutable, node_idx);
    }

    fn checkAssignment(self: *SemanticContext, node_idx: NodeIndex) !void {
        const assign = self.ast.getAssignment(node_idx);
        const loc = self.ast.getLocation(node_idx);

        // get target name
        const target_ident = self.ast.getIdentifier(assign.target);
        const target_token = self.tokens.items[target_ident.token_idx];
        const target_name = self.src.getSlice(target_token.start, target_token.start + target_token.len);

        // look up target / check locals first, then globals
        var target_type: TypeId = .unresolved;
        var is_mutable = false;

        if (self.lookupLocalPtr(target_name)) |local| {
            target_type = local.type_id;
            is_mutable = local.is_mutable;
            local.referenced = true;
        } else if (self.symbols.lookup(target_name)) |sym_idx| {
            target_type = self.symbols.getTypeId(sym_idx);
            is_mutable = self.symbols.isMutable(sym_idx);
            self.symbols.markReferenced(sym_idx);
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
        const guard_type = try self.checkExpression(guard_idx, TypeId.bool);
        const loc = self.ast.getLocation(guard_idx);

        if (guard_type) |gt| {
            if (!gt.isBool() and !gt.isUnresolved()) {
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

        const result: ?TypeId = switch (kind) {
            .literal => try self.checkLiteral(node_idx, context_type),
            .void_literal => TypeId.void,
            .identifier => try self.checkIdentifier(node_idx, context_type),
            .unary_op => try self.checkUnaryOp(node_idx, context_type),
            .binary_op => try self.checkBinaryOp(node_idx, context_type),
            .call_expr => try self.checkCallExpression(node_idx),
            .field_access => try self.checkFieldAccess(node_idx),
            else => null,
        };

        // Store resolved type for codegen
        if (result) |type_id| {
            try self.node_types.put(self.allocator, node_idx, type_id);
        }

        return result;
    }

    fn checkLiteral(self: *SemanticContext, node_idx: NodeIndex, context_type: TypeId) !?TypeId {
        const lit = self.ast.getLiteral(node_idx);
        const token = self.tokens.items[lit.token_idx];

        if (token.kind == .bool) {
            return .bool;
        }

        // for numeric literals, use context type if available
        if (context_type.isNumeric()) {
            return context_type;
        }

        // no context, type remains unresolved
        return null;
    }

    fn checkIdentifier(self: *SemanticContext, node_idx: NodeIndex, context_type: TypeId) !?TypeId {
        const ident = self.ast.getIdentifier(node_idx);
        const token = self.tokens.items[ident.token_idx];
        const name = self.src.getSlice(token.start, token.start + token.len);

        // 1. check locals
        if (self.lookupLocalPtr(name)) |local| {
            local.referenced = true;
            // propagate type from context if local is still unresolved
            if (local.type_id == .unresolved and !context_type.isUnresolved()) {
                local.type_id = context_type;
            }
            return local.type_id;
        }

        // 2. check globals
        if (self.symbols.lookup(name)) |sym_idx| {
            self.symbols.markReferenced(sym_idx);
            // propagate type from context if global is still pending
            if (self.symbols.getTypeState(sym_idx) == .pending and !context_type.isUnresolved()) {
                self.symbols.resolve(sym_idx, context_type);
            }
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

    fn checkUnaryOp(self: *SemanticContext, node_idx: NodeIndex, context_type: TypeId) !?TypeId {
        const unary_op = self.ast.getUnaryOp(node_idx);
        const loc = self.ast.getLocation(node_idx);

        switch (unary_op.op) {
            .not => {
                // logical not requires bool
                const operand_type = try self.checkExpression(unary_op.operand, TypeId.bool);

                if (operand_type) |t| {
                    if (!t.isBool() and !t.isUnresolved()) {
                        try self.errors.add(.{
                            .kind = .logical_op_requires_bool,
                            .start = loc.start,
                            .end = loc.end,
                        });
                    }
                }

                return TypeId.bool;
            },
            .negate => {
                // pass numeric context through to operand
                const operand_type = try self.checkExpression(unary_op.operand, context_type);

                // negation requires numeric type
                if (operand_type) |t| {
                    if (t.isUnresolved()) {
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

    fn checkBinaryOp(self: *SemanticContext, node_idx: NodeIndex, context_type: TypeId) !?TypeId {
        const binary_op = self.ast.getBinaryOp(node_idx);
        const loc = self.ast.getLocation(node_idx);

        if (binary_op.isArithmetic()) {
            // infer operand types, threading context for numeric propagation
            const left_type = try self.checkExpression(binary_op.left, context_type);
            const right_type = try self.checkExpression(binary_op.right, context_type);

            const operand_type = left_type orelse right_type;

            if (operand_type) |t| {
                if (!t.isNumeric() and !t.isUnresolved()) {
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
            return TypeId.bool;
        } else if (binary_op.isLogical()) {
            // infer operand types (no context)
            const left_type = try self.checkExpression(binary_op.left, .unresolved);
            const right_type = try self.checkExpression(binary_op.right, .unresolved);

            // validate left operand.
            // must be explicitly bool, unresolved (runtime check), or structurally compatible (literal true/false)
            const left_ok = if (left_type) |t|
                (t.isBool() or t.isUnresolved())
            else
                self.isExprCompatibleWithType(binary_op.left, TypeId.bool);

            if (!left_ok) {
                try self.errors.add(.{
                    .kind = .logical_op_requires_bool,
                    .start = loc.start,
                    .end = loc.end,
                });
            }

            // validate right operand
            const right_ok = if (right_type) |t|
                (t.isBool() or t.isUnresolved())
            else
                self.isExprCompatibleWithType(binary_op.right, TypeId.bool);

            if (!right_ok) {
                try self.errors.add(.{
                    .kind = .logical_op_requires_bool,
                    .start = loc.start,
                    .end = loc.end,
                });
            }

            // logical ops always produce bool
            return TypeId.bool;
        }

        return null;
    }

    fn checkCallExpression(self: *SemanticContext, node_idx: NodeIndex) !?TypeId {
        const call = self.ast.getCallExpr(node_idx);
        const loc = self.ast.getLocation(node_idx);

        // get function name
        const func_ident = self.ast.getIdentifier(call.func);
        const func_token = self.tokens.items[func_ident.token_idx];
        const func_name = self.src.getSlice(func_token.start, func_token.start + func_token.len);

        // look up function symbol
        const sym_idx = self.symbols.lookup(func_name) orelse {
            try self.errors.add(.{
                .kind = .undefined_symbol,
                .start = loc.start,
                .end = loc.end,
            });
            return null;
        };

        self.symbols.markReferenced(sym_idx);

        // verify it's a function
        if (self.symbols.getKind(sym_idx) != .function) {
            try self.errors.add(.{
                .kind = .not_callable,
                .start = loc.start,
                .end = loc.end,
            });
            return null;
        }

        // get function type from registry
        const func_type_id = self.symbols.getTypeId(sym_idx);
        const param_types = self.types.getParamTypes(func_type_id) orelse &[_]TypeId{};
        const return_type = self.types.getReturnType(func_type_id) orelse TypeId.void;

        // get args
        const args = self.ast.getExtra(call.args);

        // check args count
        if (args.len != param_types.len) {
            try self.errors.add(.{
                .kind = .argument_count_mismatch,
                .start = loc.start,
                .end = loc.end,
            });
            // still check types for arguments we have
        }

        // check each arg type
        const check_count = @min(args.len, param_types.len);
        for (0..check_count) |i| {
            const arg_idx = args[i];
            const expected_type = param_types[i];

            // check argument type (with expected type for context)
            const arg_type = try self.checkExpression(arg_idx, expected_type);
            if (arg_type) |at| {
                if (!typesCompatible(expected_type, at)) {
                    const arg_loc = self.ast.getLocation(arg_idx);
                    try self.errors.add(.{
                        .kind = .argument_type_mismatch,
                        .start = arg_loc.start,
                        .end = arg_loc.end,
                    });
                }
            }
        }

        return return_type;
    }

    fn checkFieldAccess(self: *SemanticContext, node_idx: NodeIndex) !?TypeId {
        const access = self.ast.getFieldAccess(node_idx);
        const loc = self.ast.getLocation(node_idx);

        // type-check the object expression
        const object_type = try self.checkExpression(access.object, .unresolved);

        if (object_type) |ot| {
            if (ot.isStruct()) {
                const struct_type = self.types.getStructType(ot) orelse return null;

                // get field name
                const field_ident = self.ast.getIdentifier(access.field);
                const field_token = self.tokens.items[field_ident.token_idx];
                const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);

                // look up field
                for (struct_type.fields) |field| {
                    if (mem.eql(u8, field.name, field_name)) {
                        return field.type_id;
                    }
                }

                // field not found
                try self.errors.add(.{
                    .kind = .no_such_field,
                    .start = field_token.start,
                    .end = field_token.start + field_token.len,
                });
                return null;
            } else if (!ot.isUnresolved()) {
                // dot access on non-struct
                try self.errors.add(.{
                    .kind = .field_access_on_non_struct,
                    .start = loc.start,
                    .end = loc.end,
                });
                return null;
            }
        }

        return null;
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

    fn typesCompatible(left: TypeId, right: TypeId) bool {
        // unresolved types are compatible with everything (errors caught at runtime)
        if (left.isUnresolved() or right.isUnresolved()) {
            return true;
        }

        // use structural equality
        return left.eql(right);
    }

    /// Check if an unresolved expression can become the target type.
    /// This handles structural constraints like "negation requires signed".
    fn isExprCompatibleWithType(self: *SemanticContext, node_idx: NodeIndex, target_type: TypeId) bool {
        const kind = self.ast.getKind(node_idx);

        return switch (kind) {
            .literal => {
                const lit = self.ast.getLiteral(node_idx);
                const token = self.tokens.items[lit.token_idx];
                if (token.kind == .bool) {
                    return target_type.isBool();
                }
                // numeric literals can become any numeric type
                return target_type.isNumeric();
            },
            .unary_op => {
                const unary_op = self.ast.getUnaryOp(node_idx);
                return switch (unary_op.op) {
                    .not => target_type.isBool(),
                    .negate => target_type.isSignedInteger() or target_type.isFloat(),
                };
            },
            .binary_op => {
                const binary_op = self.ast.getBinaryOp(node_idx);
                if (binary_op.isArithmetic()) {
                    return target_type.isNumeric();
                } else {
                    // in case of comparison and logical ops
                    return target_type.isBool();
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
        var scope = self.scopes.pop() orelse return;
        // check for unused/unresolved locals before destroying the scope
        var it = scope.locals.iterator();
        while (it.next()) |entry| {
            const local = entry.value_ptr;
            if (local.node_idx == 0) continue; // skip params without node info
            if (!local.referenced) {
                const loc = self.ast.getLocation(local.node_idx);
                self.errors.add(.{
                    .kind = .unused_variable,
                    .start = loc.start,
                    .end = loc.end,
                }) catch {};
                self.skip_nodes.put(self.allocator, local.node_idx, {}) catch {};
            } else if (local.type_id == .unresolved) {
                const loc = self.ast.getLocation(local.node_idx);
                self.errors.add(.{
                    .kind = .unresolved_type,
                    .start = loc.start,
                    .end = loc.end,
                }) catch {};
            }
        }
        scope.deinit();
    }

    fn declareLocal(self: *SemanticContext, name: []const u8, type_id: TypeId, is_mutable: bool, node_idx: NodeIndex) !void {
        if (self.scopes.items.len == 0) return; // no scope active
        var current = &self.scopes.items[self.scopes.items.len - 1];
        try current.locals.put(name, .{
            .type_id = type_id,
            .is_mutable = is_mutable,
            .node_idx = node_idx,
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

    fn lookupLocalPtr(self: *SemanticContext, name: []const u8) ?*LocalSymbol {
        // search from innermost to outermost, return mutable pointer
        var i = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.scopes.items[i].locals.getPtr(name)) |sym| {
                return sym;
            }
        }
        return null;
    }
};
