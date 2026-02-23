const std = @import("std");
const mem = std.mem;

const Ast = @import("../parser/ast.zig").Ast;
const NodeIndex = @import("../parser/ast.zig").NodeIndex;
const BinaryOp = @import("../parser/ast.zig").BinaryOp;
const TokenList = @import("../lexer/token.zig").TokenList;
const SourceCode = @import("../source/source.zig").SourceCode;
const SourceIndex = @import("../source/source.zig").SourceIndex;
const tupleFieldName = @import("../utils/tuple.zig").fieldName;

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
const ResolvedImports = @import("../imports/imports.zig").ResolvedImports;

pub const error_printer = @import("error-printer.zig");

pub fn analyze(
    allocator: mem.Allocator,
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    resolved_imports: ?*const ResolvedImports,
    ptr_size: u32,
) !SemanticResult {
    var ctx = try SemanticContext.init(allocator, ast, tokens, src, resolved_imports, ptr_size);
    return ctx.analyze();
}

pub const TypeError = error{
    OutOfMemory,
};

pub const SemanticResult = struct {
    symbols: SymbolTable,
    types: TypeRegistry,
    node_types: std.AutoHashMapUnmanaged(NodeIndex, TypeId),
    import_node_types: std.AutoHashMapUnmanaged(usize, std.AutoHashMapUnmanaged(NodeIndex, TypeId)),
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
    import_node_types: std.AutoHashMapUnmanaged(usize, std.AutoHashMapUnmanaged(NodeIndex, TypeId)),
    skip_nodes: std.AutoHashMapUnmanaged(NodeIndex, void),
    scopes: std.ArrayList(Scope),
    errors: ErrorList,

    resolved_imports: ?*const ResolvedImports,
    current_import_map: ?*const std.AutoHashMapUnmanaged(NodeIndex, usize) = null,

    current_ret_type: TypeId = TypeId.void,
    current_namespace_prefix: []const u8 = "",
    loop_depth: u32 = 0,

    // Tracks which import we're currently collecting structs from (null = main file)
    current_collecting_import: ?ImportContext = null,
    // Saved context for import context switching during struct finalization
    saved_ast: ?*const Ast = null,
    saved_tokens: ?*const TokenList = null,
    saved_src: ?*const SourceCode = null,

    pub fn init(
        allocator: mem.Allocator,
        ast: *const Ast,
        tokens: *const TokenList,
        src: *const SourceCode,
        resolved_imports: ?*const ResolvedImports,
        ptr_size: u32,
    ) !SemanticContext {
        return .{
            .allocator = allocator,
            .ast = ast,
            .tokens = tokens,
            .src = src,
            .symbols = try SymbolTable.init(allocator),
            .types = try TypeRegistry.init(allocator, ptr_size),
            .node_types = .{},
            .import_node_types = .{},
            .skip_nodes = .{},
            .scopes = try std.ArrayList(Scope).initCapacity(allocator, 4),
            .errors = try ErrorList.init(allocator),
            .resolved_imports = resolved_imports,
            .current_import_map = if (resolved_imports) |ri| &ri.map else null,
        };
    }

    /// Add a semantic error with the current source file ID.
    fn addError(self: *SemanticContext, err: struct {
        kind: @import("error.zig").SemanticErrorKind,
        start: SourceIndex,
        end: SourceIndex,
    }) !void {
        try self.errors.add(.{
            .kind = err.kind,
            .start = err.start,
            .end = err.end,
            .src_id = self.src.id,
        });
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
            .import_node_types = self.import_node_types,
            .skip_nodes = self.skip_nodes,
            .errors = self.errors,
        };
    }

    fn checkUnusedSymbols(self: *SemanticContext) !void {
        const count = self.symbols.count();
        for (0..count) |i| {
            const idx: SymbolIndex = @intCast(i);

            // skip imported symbols (unused checking handled in their own file)
            if (self.symbols.getIsImported(idx)) continue;

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
                    .@"type" => .unused_type,
                    .namespace => .unused_namespace,
                };
                try self.addError(.{
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
                if (type_id == .unresolved and kind != .function and kind != .@"type" and kind != .namespace) {
                    const start = self.symbols.name_starts.items[idx];
                    const len = self.symbols.name_lengths.items[idx];
                    try self.addError(.{
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

        // Phase 1: Forward-declare all struct and opaque types (including those inside namespaces and imports)
        for (decls) |decl_idx| {
            if (self.ast.getKind(decl_idx) == .struct_decl) {
                try self.forwardDeclareStruct(decl_idx);
            } else if (self.ast.getKind(decl_idx) == .opaque_decl) {
                try self.collectOpaqueDecl(decl_idx, "");
            } else if (self.ast.getKind(decl_idx) == .namespace_decl) {
                try self.forwardDeclareStructsInNamespace(decl_idx, "");
            } else if (self.ast.getKind(decl_idx) == .import_decl or self.ast.getKind(decl_idx) == .c_include_decl or self.ast.getKind(decl_idx) == .c_import_block) {
                try self.forwardDeclareImportStructs(decl_idx);
            }
        }

        // Phase 2: Finalize struct types in dependency order (including those from imports)
        try self.finalizeStructTypes(decls);

        // Phase 3: Collect all non-struct declarations
        for (decls) |decl_idx| {
            const kind = self.ast.getKind(decl_idx);
            if (kind == .namespace_decl) {
                try self.collectNamespaceDecl(decl_idx, "");
            } else if (kind == .import_decl or kind == .c_include_decl or kind == .c_import_block) {
                try self.processImportDecl(decl_idx);
            } else if (kind != .struct_decl and kind != .opaque_decl) {
                try self.collectDeclaration(decl_idx);
            }
        }
    }

    fn forwardDeclareStruct(self: *SemanticContext, node_idx: NodeIndex) !void {
        const decl = self.ast.getStructDecl(node_idx);
        const name_ident = self.ast.getIdentifier(decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        // reserve a slot in the type registry
        const type_id = try self.types.reserveStructType(name);

        // register symbol so resolveTypeName can find it
        const result = try self.symbols.register(
            name,
            name_token.start,
            .@"type",
            .resolved,
            type_id,
            node_idx,
            false,
        );

        if (result == null) {
            try self.addError(.{
                .kind = .duplicate_symbol,
                .start = name_token.start,
                .end = name_token.start + name_token.len,
            });
        }
    }

    fn collectOpaqueDecl(self: *SemanticContext, node_idx: NodeIndex, prefix: []const u8) !void {
        const decl = self.ast.getOpaqueDecl(node_idx);
        const name_ident = self.ast.getIdentifier(decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const raw_name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        // build qualified name with namespace prefix
        const name = if (prefix.len > 0)
            try std.fmt.allocPrint(self.types.arena.allocator(), "{s}.{s}", .{ prefix, raw_name })
        else
            raw_name;

        // register opaque type
        const type_id = try self.types.addOpaqueType(name);

        // register symbol
        const result = try self.symbols.register(
            name,
            name_token.start,
            .@"type",
            .resolved,
            type_id,
            node_idx,
            false,
        );

        if (result == null) {
            try self.addError(.{
                .kind = .duplicate_symbol,
                .start = name_token.start,
                .end = name_token.start + name_token.len,
            });
        }
    }

    /// Forward-declare structs found inside namespace declarations.
    fn forwardDeclareStructsInNamespace(self: *SemanticContext, node_idx: NodeIndex, prefix: []const u8) !void {
        const ns_decl = self.ast.getNamespaceDecl(node_idx);
        const ns_name_ident = self.ast.getIdentifier(ns_decl.name);
        const ns_name_token = self.tokens.items[ns_name_ident.token_idx];
        const ns_name = self.src.getSlice(ns_name_token.start, ns_name_token.start + ns_name_token.len);

        // build qualified prefix
        const arena_alloc = self.types.arena.allocator();
        const qualified_prefix = if (prefix.len > 0)
            try std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ prefix, ns_name })
        else
            try arena_alloc.dupe(u8, ns_name);

        const members = self.ast.getExtra(ns_decl.declarations);
        for (members) |member_idx| {
            const inner_idx = self.unwrapPubDecl(member_idx);
            const inner_kind = self.ast.getKind(inner_idx);
            if (inner_kind == .struct_decl) {
                try self.forwardDeclareStructWithPrefix(inner_idx, qualified_prefix);
            } else if (inner_kind == .namespace_decl) {
                try self.forwardDeclareStructsInNamespace(inner_idx, qualified_prefix);
            }
        }
    }

    /// Forward-declare a struct with a qualified name prefix.
    fn forwardDeclareStructWithPrefix(self: *SemanticContext, node_idx: NodeIndex, prefix: []const u8) !void {
        const decl = self.ast.getStructDecl(node_idx);
        const name_ident = self.ast.getIdentifier(decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const short_name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        const arena_alloc = self.types.arena.allocator();
        const qualified_name = try std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ prefix, short_name });

        const type_id = try self.types.reserveStructType(qualified_name);

        const result = try self.symbols.register(
            qualified_name,
            name_token.start,
            .@"type",
            .resolved,
            type_id,
            node_idx,
            false,
        );

        if (result == null) {
            try self.addError(.{
                .kind = .duplicate_symbol,
                .start = name_token.start,
                .end = name_token.start + name_token.len,
            });
        }
    }

    /// Forward-declare structs from an imported file.
    fn forwardDeclareImportStructs(self: *SemanticContext, node_idx: NodeIndex) !void {
        const ri = self.resolved_imports orelse return;
        const import_map = self.current_import_map orelse return;
        const import_idx = import_map.get(node_idx) orelse return;
        const resolved = &ri.imports.items[import_idx];

        const saved_ast = self.ast;
        const saved_tokens = self.tokens;
        const saved_src = self.src;
        const saved_import_map = self.current_import_map;
        self.ast = &resolved.ast;
        self.tokens = &resolved.tokens;
        self.src = &resolved.src;
        self.current_import_map = &resolved.sub_import_map;
        defer {
            self.ast = saved_ast;
            self.tokens = saved_tokens;
            self.src = saved_src;
            self.current_import_map = saved_import_map;
        }

        const ns_name = resolved.namespace_name;

        // Walk imported file's declarations
        const sym_count_before = self.symbols.count();
        const program = self.ast.getProgram(self.ast.root);
        const decls = self.ast.getExtra(program.declarations);
        for (decls) |decl_idx| {
            const inner_idx = self.unwrapPubDecl(decl_idx);
            const inner_kind = self.ast.getKind(inner_idx);
            if (inner_kind == .struct_decl) {
                try self.forwardDeclareStructWithPrefix(inner_idx, ns_name);
            } else if (inner_kind == .opaque_decl) {
                try self.collectOpaqueDecl(inner_idx, ns_name);
            } else if (inner_kind == .namespace_decl) {
                try self.forwardDeclareStructsInNamespace(inner_idx, ns_name);
            } else if (inner_kind == .import_decl or inner_kind == .c_include_decl or inner_kind == .c_import_block) {
                try self.forwardDeclareImportStructs(inner_idx);
            }
        }

        // Mark all newly registered symbols as imported (their value_nodes reference the imported AST)
        for (sym_count_before..self.symbols.count()) |si| {
            self.symbols.markImported(@intCast(si));
        }
    }

    /// Process an import declaration: collect all declarations as namespace members.
    fn processImportDecl(self: *SemanticContext, node_idx: NodeIndex) !void {
        const ri = self.resolved_imports orelse return;
        const import_map = self.current_import_map orelse return;
        const import_idx = import_map.get(node_idx) orelse return;
        const resolved = &ri.imports.items[import_idx];

        const saved_ast = self.ast;
        const saved_tokens = self.tokens;
        const saved_src = self.src;
        const saved_import_map = self.current_import_map;
        self.ast = &resolved.ast;
        self.tokens = &resolved.tokens;
        self.src = &resolved.src;
        self.current_import_map = &resolved.sub_import_map;
        defer {
            self.ast = saved_ast;
            self.tokens = saved_tokens;
            self.src = saved_src;
            self.current_import_map = saved_import_map;
        }

        const ns_name = resolved.namespace_name;
        const arena_alloc = self.types.arena.allocator();

        const saved_prefix = self.current_namespace_prefix;
        self.current_namespace_prefix = ns_name;
        defer self.current_namespace_prefix = saved_prefix;

        const program = self.ast.getProgram(self.ast.root);
        const decls = self.ast.getExtra(program.declarations);

        var ns_members = try std.ArrayList(@import("types.zig").NamespaceMember).initCapacity(self.allocator, decls.len);
        defer ns_members.deinit(self.allocator);

        const sym_count_before = self.symbols.count();

        for (decls) |decl_idx| {
            const is_pub = self.ast.getKind(decl_idx) == .pub_decl;
            const inner_idx = self.unwrapPubDecl(decl_idx);
            const inner_kind = self.ast.getKind(inner_idx);

            if (inner_kind == .import_decl or inner_kind == .c_include_decl or inner_kind == .c_import_block) {
                // Recursively process nested imports
                try self.processImportDecl(inner_idx);
                // Register the nested import's namespace as a member of this namespace
                const nested_ri = self.current_import_map orelse continue;
                const nested_import_idx = nested_ri.get(inner_idx) orelse continue;
                const nested_resolved = &ri.imports.items[nested_import_idx];
                const nested_ns_name = nested_resolved.namespace_name;
                const nested_qualified = try std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ ns_name, nested_ns_name });
                _ = nested_qualified;
                if (self.symbols.lookup(nested_ns_name)) |sym_idx| {
                    try ns_members.append(self.allocator, .{
                        .name = nested_ns_name,
                        .symbol_idx = sym_idx,
                        .is_pub = is_pub,
                    });
                }
                continue;
            }

            const member_short_name = self.getMemberName(inner_idx) orelse continue;
            const qualified_name = try std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ ns_name, member_short_name });

            if (inner_kind == .namespace_decl) {
                try self.collectNamespaceDecl(inner_idx, ns_name);
                if (self.symbols.lookup(qualified_name)) |sym_idx| {
                    try ns_members.append(self.allocator, .{
                        .name = member_short_name,
                        .symbol_idx = sym_idx,
                        .is_pub = is_pub,
                    });
                }
            } else if (inner_kind == .struct_decl) {
                if (self.symbols.lookup(qualified_name)) |sym_idx| {
                    try ns_members.append(self.allocator, .{
                        .name = member_short_name,
                        .symbol_idx = sym_idx,
                        .is_pub = is_pub,
                    });
                }
            } else {
                const sym_idx = try self.collectMemberDecl(inner_idx, qualified_name);
                if (sym_idx) |si| {
                    // Set extern_name for imported bodyless c func declarations
                    // so codegen emits the unqualified C symbol name
                    if (inner_kind == .func_decl) {
                        const func_ast = self.ast.getFuncDecl(inner_idx);
                        if (func_ast.body == null) {
                            self.symbols.setExternName(si, member_short_name);
                        }
                    }

                    try ns_members.append(self.allocator, .{
                        .name = member_short_name,
                        .symbol_idx = si,
                        .is_pub = is_pub,
                    });
                }
            }
        }

        // Mark all member symbols as imported (their value_nodes reference the imported AST)
        for (sym_count_before..self.symbols.count()) |si| {
            self.symbols.markImported(@intCast(si));
        }

        // Register namespace type and symbol using the main file's AST location
        const ns_type_id = try self.types.addNamespaceType(ns_name, ns_members.items);

        // Use the import_decl node's location from the main file for the symbol
        const main_loc = saved_ast.getLocation(node_idx);

        const register_result = try self.symbols.register(
            ns_name,
            main_loc.start,
            .namespace,
            .resolved,
            ns_type_id,
            node_idx,
            false,
        );

        // Mark namespace symbol as imported too (name is derived from filename, not in main source)
        if (register_result) |ns_sym| {
            self.symbols.markImported(ns_sym);
        }

        if (register_result == null) {
            try self.addError(.{
                .kind = .duplicate_symbol,
                .start = main_loc.start,
                .end = main_loc.end,
            });
        }
    }

    /// Unwrap a pub_decl to get the inner declaration node.
    fn unwrapPubDecl(self: *SemanticContext, node_idx: NodeIndex) NodeIndex {
        if (self.ast.getKind(node_idx) == .pub_decl) {
            const pub_decl = self.ast.getPubDecl(node_idx);
            return pub_decl.inner;
        }
        return node_idx;
    }

    /// Collect a namespace declaration: register all members with qualified names.
    fn collectNamespaceDecl(self: *SemanticContext, node_idx: NodeIndex, prefix: []const u8) !void {
        const ns_decl = self.ast.getNamespaceDecl(node_idx);
        const ns_name_ident = self.ast.getIdentifier(ns_decl.name);
        const ns_name_token = self.tokens.items[ns_name_ident.token_idx];
        const ns_name = self.src.getSlice(ns_name_token.start, ns_name_token.start + ns_name_token.len);

        const arena_alloc = self.types.arena.allocator();
        const qualified_prefix = if (prefix.len > 0)
            try std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ prefix, ns_name })
        else
            try arena_alloc.dupe(u8, ns_name);

        const saved_prefix = self.current_namespace_prefix;
        self.current_namespace_prefix = qualified_prefix;
        defer self.current_namespace_prefix = saved_prefix;

        const members = self.ast.getExtra(ns_decl.declarations);

        // Build namespace member list and register each member
        var ns_members = try std.ArrayList(@import("types.zig").NamespaceMember).initCapacity(self.allocator, members.len);
        defer ns_members.deinit(self.allocator);

        for (members) |member_idx| {
            const is_pub = self.ast.getKind(member_idx) == .pub_decl;
            const inner_idx = self.unwrapPubDecl(member_idx);
            const inner_kind = self.ast.getKind(inner_idx);

            // Get member's short name
            const member_short_name = self.getMemberName(inner_idx) orelse continue;

            // Build qualified name
            const qualified_name = try std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ qualified_prefix, member_short_name });

            if (inner_kind == .namespace_decl) {
                // Nested namespace: recurse
                try self.collectNamespaceDecl(inner_idx, qualified_prefix);
                // Look up the nested namespace symbol we just registered
                if (self.symbols.lookup(qualified_name)) |sym_idx| {
                    try ns_members.append(self.allocator, .{
                        .name = member_short_name,
                        .symbol_idx = sym_idx,
                        .is_pub = is_pub,
                    });
                }
            } else if (inner_kind == .struct_decl) {
                // Already forward-declared with qualified name
                if (self.symbols.lookup(qualified_name)) |sym_idx| {
                    try ns_members.append(self.allocator, .{
                        .name = member_short_name,
                        .symbol_idx = sym_idx,
                        .is_pub = is_pub,
                    });
                }
            } else {
                // Register the member with its qualified name
                const sym_idx = try self.collectMemberDecl(inner_idx, qualified_name);
                if (sym_idx) |si| {
                    try ns_members.append(self.allocator, .{
                        .name = member_short_name,
                        .symbol_idx = si,
                        .is_pub = is_pub,
                    });
                }
            }
        }

        // Register namespace type
        const ns_type_id = try self.types.addNamespaceType(qualified_prefix, ns_members.items);

        // Register namespace symbol
        const result = try self.symbols.register(
            qualified_prefix,
            ns_name_token.start,
            .namespace,
            .resolved,
            ns_type_id,
            node_idx,
            false,
        );

        if (result == null) {
            try self.addError(.{
                .kind = .duplicate_symbol,
                .start = ns_name_token.start,
                .end = ns_name_token.start + ns_name_token.len,
            });
        }
    }

    /// Get the short name of a declaration node.
    fn getMemberName(self: *SemanticContext, node_idx: NodeIndex) ?[]const u8 {
        const kind = self.ast.getKind(node_idx);
        const name_node = switch (kind) {
            .const_decl => self.ast.getConstDecl(node_idx).name,
            .func_decl => self.ast.getFuncDecl(node_idx).name,
            .var_decl => self.ast.getVarDecl(node_idx).name,
            .struct_decl => self.ast.getStructDecl(node_idx).name,
            .namespace_decl => self.ast.getNamespaceDecl(node_idx).name,
            else => return null,
        };
        const ident = self.ast.getIdentifier(name_node);
        const token = self.tokens.items[ident.token_idx];
        return self.src.getSlice(token.start, token.start + token.len);
    }

    /// Register a member declaration (const, var, func) with a specific qualified name.
    fn collectMemberDecl(self: *SemanticContext, node_idx: NodeIndex, qualified_name: []const u8) !?SymbolIndex {
        const kind = self.ast.getKind(node_idx);
        switch (kind) {
            .const_decl => return try self.collectConstDeclWithName(node_idx, qualified_name),
            .func_decl => return try self.collectFuncDeclWithName(node_idx, qualified_name),
            .var_decl => return try self.collectVarDeclWithName(node_idx, qualified_name),
            else => return null,
        }
    }

    fn collectConstDeclWithName(self: *SemanticContext, node_idx: NodeIndex, name: []const u8) !?SymbolIndex {
        const decl = self.ast.getConstDecl(node_idx);
        const name_ident = self.ast.getIdentifier(decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];

        var type_state = TypeState.pending;
        var type_id: TypeId = .unresolved;

        if (decl.type_id) |type_idx| {
            if (self.resolveTypeNode(type_idx)) |tid| {
                type_id = tid;
                type_state = .resolved;
            } else {
                const inferred = self.resolveInferredArrayType(type_idx, decl.value);
                if (!inferred.isUnresolved()) {
                    type_id = inferred;
                    type_state = .resolved;
                } else {
                    const loc = self.ast.getLocation(type_idx);
                    try self.addError(.{ .kind = .unknown_type, .start = loc.start, .end = loc.end });
                }
            }
        }

        const result = try self.symbols.register(name, name_token.start, .constant, type_state, type_id, decl.value, false);
        if (result == null) {
            try self.addError(.{ .kind = .duplicate_symbol, .start = name_token.start, .end = name_token.start + name_token.len });
            return null;
        }
        return result;
    }

    fn collectFuncDeclWithName(self: *SemanticContext, node_idx: NodeIndex, name: []const u8) !?SymbolIndex {
        const decl = self.ast.getFuncDecl(node_idx);
        const name_ident = self.ast.getIdentifier(decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];

        const return_type = self.resolveTypeNode(decl.return_type) orelse .unresolved;

        const params = self.ast.getExtra(decl.params);
        const param_count = params.len / 2;
        var param_types = try std.ArrayList(TypeId).initCapacity(self.allocator, param_count);
        defer param_types.deinit(self.allocator);

        var i: usize = 0;
        while (i < params.len) : (i += 2) {
            const param_type = self.resolveTypeNode(params[i + 1]) orelse .unresolved;
            try param_types.append(self.allocator, param_type);
        }

        const func_type = try self.types.addFunctionTypeEx(param_types.items, return_type, decl.call_conv, decl.is_variadic);

        const result = try self.symbols.register(name, name_token.start, .function, .resolved, func_type, node_idx, false);
        if (result == null) {
            try self.addError(.{ .kind = .duplicate_symbol, .start = name_token.start, .end = name_token.start + name_token.len });
            return null;
        }
        return result;
    }

    fn collectVarDeclWithName(self: *SemanticContext, node_idx: NodeIndex, name: []const u8) !?SymbolIndex {
        const decl = self.ast.getVarDecl(node_idx);
        const name_ident = self.ast.getIdentifier(decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];

        var type_state = TypeState.pending;
        var type_id: TypeId = .unresolved;

        if (decl.type_id) |type_idx| {
            if (self.resolveTypeNode(type_idx)) |tid| {
                type_id = tid;
                type_state = .resolved;
            } else {
                const inferred = self.resolveInferredArrayType(type_idx, decl.value);
                if (!inferred.isUnresolved()) {
                    type_id = inferred;
                    type_state = .resolved;
                } else {
                    const loc = self.ast.getLocation(type_idx);
                    try self.addError(.{ .kind = .unknown_type, .start = loc.start, .end = loc.end });
                }
            }
        }

        const result = try self.symbols.register(name, name_token.start, .variable, type_state, type_id, decl.value, decl.is_mutable);
        if (result == null) {
            try self.addError(.{ .kind = .duplicate_symbol, .start = name_token.start, .end = name_token.start + name_token.len });
            return null;
        }
        return result;
    }

    /// Context needed to access a struct node that may live in an imported file's AST.
    const ImportContext = struct {
        import_idx: usize,
    };

    /// Finalize struct types iteratively until all are done.
    /// Each pass finalizes structs whose field types are already finalized.
    fn finalizeStructTypes(self: *SemanticContext, decls: []const NodeIndex) !void {
        // collect all struct declaration nodes (including those inside namespaces and imports)
        var struct_nodes = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 4);
        defer struct_nodes.deinit(self.allocator);
        var lookup_names = try std.ArrayList([]const u8).initCapacity(self.allocator, 4);
        defer lookup_names.deinit(self.allocator);
        var import_contexts = try std.ArrayList(?ImportContext).initCapacity(self.allocator, 4);
        defer import_contexts.deinit(self.allocator);

        try self.collectAllStructDecls(decls, "", &struct_nodes, &lookup_names, &import_contexts);

        var finalized = try std.ArrayList(bool).initCapacity(self.allocator, struct_nodes.items.len);
        defer finalized.deinit(self.allocator);
        for (0..struct_nodes.items.len) |_| {
            try finalized.append(self.allocator, false);
        }

        var remaining = struct_nodes.items.len;
        var prev_remaining = remaining + 1;

        while (remaining > 0 and remaining < prev_remaining) {
            prev_remaining = remaining;

            for (struct_nodes.items, 0..) |decl_idx, si| {
                if (finalized.items[si]) continue;

                self.current_namespace_prefix = self.namespaceFromQualified(lookup_names.items[si]);
                self.switchToImportContext(import_contexts.items[si]);
                defer self.restoreFromImportContext(import_contexts.items[si]);
                if (self.canFinalizeStruct(decl_idx)) {
                    try self.finalizeStructByName(decl_idx, lookup_names.items[si]);
                    finalized.items[si] = true;
                    remaining -= 1;
                }
            }
        }

        // any remaining structs have unresolvable field types (cycles or unknown types)
        for (struct_nodes.items, 0..) |decl_idx, si| {
            if (!finalized.items[si]) {
                self.current_namespace_prefix = self.namespaceFromQualified(lookup_names.items[si]);
                self.switchToImportContext(import_contexts.items[si]);
                defer self.restoreFromImportContext(import_contexts.items[si]);
                try self.finalizeStructByName(decl_idx, lookup_names.items[si]);
            }
        }

        self.current_namespace_prefix = "";
    }

    /// Switch AST/tokens/src to an imported file's context. Saves the current context
    /// so it can be restored with restoreFromImportContext.
    fn switchToImportContext(self: *SemanticContext, ctx: ?ImportContext) void {
        const ri = self.resolved_imports orelse return;
        const ic = ctx orelse return;
        const resolved = &ri.imports.items[ic.import_idx];
        self.saved_ast = self.ast;
        self.saved_tokens = self.tokens;
        self.saved_src = self.src;
        self.ast = &resolved.ast;
        self.tokens = &resolved.tokens;
        self.src = &resolved.src;
    }

    fn restoreFromImportContext(self: *SemanticContext, ctx: ?ImportContext) void {
        if (ctx == null) return;
        self.ast = self.saved_ast.?;
        self.tokens = self.saved_tokens.?;
        self.src = self.saved_src.?;
    }

    /// Extract namespace prefix from a qualified name (e.g. "geometry.Vec2" -> "geometry").
    fn namespaceFromQualified(_: *SemanticContext, qualified: []const u8) []const u8 {
        // find last dot
        var i = qualified.len;
        while (i > 0) {
            i -= 1;
            if (qualified[i] == '.') return qualified[0..i];
        }
        return "";
    }

    /// Recursively collect all struct_decl nodes from declarations, including inside namespaces.
    /// Also tracks which import context each struct belongs to (null for main file structs).
    fn collectAllStructDecls(
        self: *SemanticContext,
        decls: []const NodeIndex,
        prefix: []const u8,
        struct_nodes: *std.ArrayList(NodeIndex),
        lookup_names: *std.ArrayList([]const u8),
        import_contexts: *std.ArrayList(?ImportContext),
    ) !void {
        const arena_alloc = self.types.arena.allocator();
        for (decls) |decl_idx| {
            const inner_idx = self.unwrapPubDecl(decl_idx);
            const kind = self.ast.getKind(inner_idx);
            if (kind == .struct_decl) {
                const short_name = self.getMemberName(inner_idx) orelse continue;
                const lookup_name = if (prefix.len > 0)
                    try std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ prefix, short_name })
                else
                    short_name;
                try struct_nodes.append(self.allocator, inner_idx);
                try lookup_names.append(self.allocator, lookup_name);
                try import_contexts.append(self.allocator, self.current_collecting_import);
            } else if (kind == .namespace_decl) {
                const ns_decl = self.ast.getNamespaceDecl(inner_idx);
                const ns_name = self.getMemberName(inner_idx) orelse continue;
                const ns_prefix = if (prefix.len > 0)
                    try std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ prefix, ns_name })
                else
                    try arena_alloc.dupe(u8, ns_name);
                const ns_members = self.ast.getExtra(ns_decl.declarations);
                try self.collectAllStructDecls(ns_members, ns_prefix, struct_nodes, lookup_names, import_contexts);
            } else if (kind == .import_decl or kind == .c_include_decl or kind == .c_import_block) {
                // Context-switch into the imported file to collect its structs
                const ri = self.resolved_imports orelse continue;
                const cur_map = self.current_import_map orelse continue;
                const import_idx = cur_map.get(inner_idx) orelse continue;
                const resolved = &ri.imports.items[import_idx];

                const saved_ast = self.ast;
                const saved_tokens = self.tokens;
                const saved_src = self.src;
                const saved_import_map = self.current_import_map;
                const saved_collecting_import = self.current_collecting_import;
                self.ast = &resolved.ast;
                self.tokens = &resolved.tokens;
                self.src = &resolved.src;
                self.current_import_map = &resolved.sub_import_map;
                self.current_collecting_import = .{ .import_idx = import_idx };

                const program_node = self.ast.getProgram(self.ast.root);
                const import_decls = self.ast.getExtra(program_node.declarations);
                self.collectAllStructDecls(import_decls, resolved.namespace_name, struct_nodes, lookup_names, import_contexts) catch {
                    self.ast = saved_ast;
                    self.tokens = saved_tokens;
                    self.src = saved_src;
                    self.current_import_map = saved_import_map;
                    self.current_collecting_import = saved_collecting_import;
                    continue;
                };

                self.ast = saved_ast;
                self.tokens = saved_tokens;
                self.src = saved_src;
                self.current_import_map = saved_import_map;
                self.current_collecting_import = saved_collecting_import;
            }
        }
    }

    /// Check if all field types of a struct are finalized (have known sizes).
    fn canFinalizeStruct(self: *SemanticContext, node_idx: NodeIndex) bool {
        const decl = self.ast.getStructDecl(node_idx);
        const field_data = self.ast.getExtra(decl.fields);

        var i: usize = 0;
        while (i < field_data.len) : (i += 2) {
            const field_type_idx = field_data[i + 1];
            if (self.resolveTypeNode(field_type_idx)) |tid| {
                if (tid.isStruct()) {
                    const st = self.types.struct_types.items[tid.struct_type];
                    if (!st.finalized) return false;
                }
                // pointers have known size regardless of pointee finalization
            }
        }

        return true;
    }

    /// Finalize a struct: resolve field types, compute layout, update registry.
    fn finalizeStructByName(self: *SemanticContext, node_idx: NodeIndex, lookup_name: []const u8) !void {
        const decl = self.ast.getStructDecl(node_idx);

        const sym_idx = self.symbols.lookup(lookup_name) orelse return;
        const type_id = self.symbols.getTypeId(sym_idx);

        // parse field pairs
        const field_data = self.ast.getExtra(decl.fields);
        const field_count = field_data.len / 2;

        var field_names = try std.ArrayList([]const u8).initCapacity(self.allocator, field_count);
        defer field_names.deinit(self.allocator);
        var field_types = try std.ArrayList(TypeId).initCapacity(self.allocator, field_count);
        defer field_types.deinit(self.allocator);

        var seen_fields = std.StringHashMap(void).init(self.allocator);
        defer seen_fields.deinit();

        var positional_idx: usize = 0;

        var i: usize = 0;
        while (i < field_data.len) : (i += 2) {
            const field_name_idx = field_data[i];
            const field_type_idx = field_data[i + 1];

            // Tuple fields use sentinel (maxInt) for name — use synthetic "0", "1", etc.
            const field_name = if (field_name_idx == std.math.maxInt(NodeIndex)) blk: {
                const name = tupleFieldName(self.allocator, positional_idx) catch "?";
                positional_idx += 1;
                break :blk name;
            } else blk: {
                const field_ident = self.ast.getIdentifier(field_name_idx);
                const field_token = self.tokens.items[field_ident.token_idx];
                break :blk self.src.getSlice(field_token.start, field_token.start + field_token.len);
            };

            if (field_name_idx != std.math.maxInt(NodeIndex)) {
                // only check duplicates for named fields (tuple fields are unique by construction)
                if (seen_fields.contains(field_name)) {
                    const loc = self.ast.getLocation(field_name_idx);
                    try self.addError(.{
                        .kind = .duplicate_field,
                        .start = loc.start,
                        .end = loc.end,
                    });
                } else {
                    try seen_fields.put(field_name, {});
                }
            }

            const field_type = self.resolveTypeNode(field_type_idx) orelse blk: {
                const field_loc = self.ast.getLocation(field_type_idx);
                try self.addError(.{
                    .kind = .unknown_type,
                    .start = field_loc.start,
                    .end = field_loc.end,
                });
                break :blk TypeId.unresolved;
            };

            try field_names.append(self.allocator, field_name);
            try field_types.append(self.allocator, field_type);
        }

        try self.types.finalizeStructType(
            type_id,
            field_names.items,
            field_types.items,
            decl.call_conv,
        );
    }

    fn collectDeclaration(self: *SemanticContext, node_idx: NodeIndex) !void {
        const kind = self.ast.getKind(node_idx);
        switch (kind) {
            .const_decl => try self.collectConstDecl(node_idx),
            .var_decl => try self.collectVarDecl(node_idx),
            .func_decl => try self.collectFuncDecl(node_idx),
            .struct_decl => {}, // handled by forwardDeclareStruct + finalizeStruct
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
            if (self.resolveTypeNode(type_idx)) |tid| {
                type_id = tid;
                type_state = .resolved;
            } else {
                const inferred = self.resolveInferredArrayType(type_idx, decl.value);
                if (!inferred.isUnresolved()) {
                    type_id = inferred;
                    type_state = .resolved;
                } else {
                    const loc = self.ast.getLocation(type_idx);
                    try self.addError(.{
                        .kind = .unknown_type,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
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
            try self.addError(.{
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
            if (self.resolveTypeNode(type_idx)) |tid| {
                type_id = tid;
                type_state = .resolved;
            } else {
                const inferred = self.resolveInferredArrayType(type_idx, decl.value);
                if (!inferred.isUnresolved()) {
                    type_id = inferred;
                    type_state = .resolved;
                } else {
                    const loc = self.ast.getLocation(type_idx);
                    try self.addError(.{
                        .kind = .unknown_type,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
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
            try self.addError(.{
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
        const return_type = self.resolveTypeNode(decl.return_type) orelse .unresolved;

        // collect parameter types
        const params = self.ast.getExtra(decl.params);
        const param_count = params.len / 2;

        var param_types = try std.ArrayList(TypeId).initCapacity(self.allocator, param_count);
        defer param_types.deinit(self.allocator);

        var i: usize = 0;
        while (i < params.len) : (i += 2) {
            const param_type_idx = params[i + 1]; // type is second in pair
            const param_type = self.resolveTypeNode(param_type_idx) orelse .unresolved;
            try param_types.append(self.allocator, param_type);
        }

        // register function type
        const func_type = try self.types.addFunctionTypeEx(
            param_types.items,
            return_type,
            decl.call_conv,
            decl.is_variadic,
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
            try self.addError(.{
                .kind = .duplicate_symbol,
                .start = name_token.start,
                .end = name_token.start + name_token.len,
            });
        }
    }

    /// Resolve a type AST node (identifier or pointer_type) to a TypeId.
    fn resolveTypeNode(self: *SemanticContext, node_idx: NodeIndex) ?TypeId {
        const kind = self.ast.getKind(node_idx);
        return switch (kind) {
            .identifier => {
                const ident = self.ast.getIdentifier(node_idx);
                const token = self.tokens.items[ident.token_idx];
                const name = self.src.getSlice(token.start, token.start + token.len);
                return self.resolveTypeName(name);
            },
            .pointer_type => {
                const ptr = self.ast.getPointerType(node_idx);
                const pointee = self.resolveTypeNode(ptr.pointee) orelse return null;
                return self.types.addPointerType(pointee, ptr.is_mutable, ptr.is_many_item) catch return null;
            },
            .array_type => {
                const arr = self.ast.getArrayType(node_idx);
                const element_type = self.resolveTypeNode(arr.element_type) orelse return null;
                const length = arr.length orelse return null; // [_]T needs context to resolve
                return self.types.addArrayTypeWithSentinel(element_type, length, arr.is_mutable, arr.sentinel) catch return null;
            },
            .slice_type => {
                const slc = self.ast.getSliceType(node_idx);
                const element_type = self.resolveTypeNode(slc.element_type) orelse return null;
                return self.types.addSliceTypeWithSentinel(element_type, slc.is_mutable, slc.sentinel) catch return null;
            },
            else => null,
        };
    }

    /// Resolve [_]T array type by inferring length from a value expression.
    /// Returns .unresolved if the type node is not an inferred array type.
    fn resolveInferredArrayType(self: *SemanticContext, type_idx: NodeIndex, value_idx: NodeIndex) TypeId {
        if (self.ast.getKind(type_idx) != .array_type) return .unresolved;
        const arr = self.ast.getArrayType(type_idx);
        if (arr.length != null) return .unresolved;
        const elem_tid = self.resolveTypeNode(arr.element_type) orelse return .unresolved;
        if (self.ast.getKind(value_idx) != .array_literal) return .unresolved;
        const lit = self.ast.getArrayLiteral(value_idx);
        const elements = self.ast.getExtra(lit.elements);
        const inferred_len: u32 = @intCast(elements.len);
        return self.types.addArrayType(elem_tid, inferred_len, arr.is_mutable) catch .unresolved;
    }

    fn resolveTypeName(self: *const SemanticContext, name: []const u8) ?TypeId {
        // try primitives first
        if (resolvePrimitiveTypeName(name)) |tid| return tid;

        // then check symbol table for struct types
        if (self.symbols.lookup(name)) |sym_idx| {
            if (self.symbols.getKind(sym_idx) == .@"type") {
                return self.symbols.getTypeId(sym_idx);
            }
        }

        // try with current namespace prefix
        if (self.current_namespace_prefix.len > 0) {
            var buf: [512]u8 = undefined;
            const qualified = std.fmt.bufPrint(&buf, "{s}.{s}", .{ self.current_namespace_prefix, name }) catch return null;
            if (self.symbols.lookup(qualified)) |sym_idx| {
                if (self.symbols.getKind(sym_idx) == .@"type") {
                    return self.symbols.getTypeId(sym_idx);
                }
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
            .{ "usize", TypeId.usize },
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

                // skip imported symbols (their value_nodes reference a different AST)
                if (self.symbols.getIsImported(idx)) continue;

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

            // skip imported symbols (their value_nodes reference a different AST)
            if (self.symbols.getIsImported(idx)) continue;

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
            .struct_literal => self.inferStructLiteral(node_idx),
            .identifier => self.inferIdentifierType(node_idx),
            .binary_op => self.inferBinaryOpType(node_idx),
            .unary_op => self.inferUnaryOpType(node_idx),
            .address_of => {
                const addr = self.ast.getAddressOf(node_idx);
                const operand_type = self.tryInferType(addr.operand) orelse return null;
                // default to immutable single-item pointer for inference
                return self.types.addPointerType(operand_type, false, false) catch return null;
            },
            .deref => {
                const deref = self.ast.getDeref(node_idx);
                const operand_type = self.tryInferType(deref.operand) orelse return null;
                if (self.types.getPointerType(operand_type)) |ptr_info| {
                    return ptr_info.pointee;
                }
                return null;
            },
            .array_literal => {
                const lit = self.ast.getArrayLiteral(node_idx);
                const elements = self.ast.getExtra(lit.elements);
                if (elements.len == 0) return null;
                // infer element type from first element
                const elem_type = self.tryInferType(elements[0]) orelse return null;
                const length: u32 = @intCast(elements.len);
                return self.types.addArrayType(elem_type, length, false) catch return null;
            },
            .array_index => {
                const idx_node = self.ast.getArrayIndex(node_idx);
                const obj_type = self.tryInferType(idx_node.object) orelse return null;
                if (self.types.getArrayType(obj_type)) |arr_info| {
                    return arr_info.element_type;
                }
                return null;
            },
            .array_slice => {
                const slice_node = self.ast.getArraySlice(node_idx);
                const obj_type = self.tryInferType(slice_node.object) orelse return null;
                if (obj_type.isArray()) {
                    if (self.types.getArrayType(obj_type)) |arr_info| {
                        return self.types.addSliceType(arr_info.element_type, arr_info.is_mutable) catch return null;
                    }
                } else if (obj_type.isSlice()) {
                    // slicing drops sentinel
                    if (self.types.getSliceType(obj_type)) |slice_info| {
                        return self.types.addSliceType(slice_info.element_type, slice_info.is_mutable) catch return null;
                    }
                    return obj_type;
                }
                return null;
            },
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

        // string literals are always [:0]u8 (null-terminated immutable slice)
        if (token.kind == .string_literal) {
            return self.types.addSliceTypeWithSentinel(.{ .primitive = .u8 }, false, 0) catch return null;
        }

        // numeric literals cannot be inferred without context
        // (they need to be anchored by an explicitly typed value)
        return null;
    }

    fn inferStructLiteral(self: *SemanticContext, node_idx: NodeIndex) ?TypeId {
        const lit = self.ast.getStructLiteral(node_idx);
        const ident = self.ast.getIdentifier(lit.type_name);
        const token = self.tokens.items[ident.token_idx];
        const name = self.src.getSlice(token.start, token.start + token.len);

        if (self.symbols.lookup(name)) |sym_idx| {
            if (self.symbols.getTypeState(sym_idx) == .resolved) {
                return self.symbols.getTypeId(sym_idx);
            }
        }

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

                // also propagate through local variables
                if (self.lookupLocalPtr(name)) |local| {
                    if (local.type_id.isUnresolved()) {
                        local.type_id = type_id;
                        // cascade through the local's value expression
                        if (local.node_idx != 0) {
                            const var_decl = self.ast.getVarDecl(local.node_idx);
                            self.propagateType(var_decl.value, type_id);
                        }
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
            .address_of => {
                // unwrap pointer type and propagate pointee to operand
                if (type_id.isPointer()) {
                    if (self.types.getPointerType(type_id)) |ptr_info| {
                        const addr = self.ast.getAddressOf(node_idx);
                        self.propagateType(addr.operand, ptr_info.pointee);
                    }
                }
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
                if (kind == .function or kind == .@"type" or kind == .namespace) continue;

                // skip imported symbols (their value_nodes reference a different AST)
                if (self.symbols.getIsImported(idx)) continue;

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
            .struct_decl => {}, // handled by two-pass struct collection
            .namespace_decl => try self.checkNamespaceDecl(node_idx, ""),
            .import_decl, .c_include_decl, .c_import_block => try self.checkImportDecl(node_idx),
            .pub_decl => {
                const inner = self.unwrapPubDecl(node_idx);
                try self.checkDeclaration(inner);
            },
            else => {}, // ignore
        }
    }

    /// Type-check declarations from an imported file.
    fn checkImportDecl(self: *SemanticContext, node_idx: NodeIndex) !void {
        const ri = self.resolved_imports orelse return;
        const cur_map = self.current_import_map orelse return;
        const import_idx = cur_map.get(node_idx) orelse return;
        const resolved = &ri.imports.items[import_idx];

        const saved_ast = self.ast;
        const saved_tokens = self.tokens;
        const saved_src = self.src;
        const saved_node_types = self.node_types;
        const saved_import_map = self.current_import_map;
        self.ast = &resolved.ast;
        self.tokens = &resolved.tokens;
        self.src = &resolved.src;
        self.current_import_map = &resolved.sub_import_map;

        // Use a separate node_types map for the imported file to avoid
        // NodeIndex collisions with the main file's AST.
        const existing = self.import_node_types.get(import_idx);
        self.node_types = if (existing) |e| e else .{};

        defer {
            // Save the import's node_types before restoring main's
            self.import_node_types.put(self.allocator, import_idx, self.node_types) catch {};
            self.ast = saved_ast;
            self.tokens = saved_tokens;
            self.src = saved_src;
            self.node_types = saved_node_types;
            self.current_import_map = saved_import_map;
        }

        const ns_name = resolved.namespace_name;

        const saved_prefix = self.current_namespace_prefix;
        self.current_namespace_prefix = ns_name;
        defer self.current_namespace_prefix = saved_prefix;

        const arena_alloc = self.types.arena.allocator();
        const program = self.ast.getProgram(self.ast.root);
        const decls = self.ast.getExtra(program.declarations);

        for (decls) |decl_idx| {
            const inner_idx = self.unwrapPubDecl(decl_idx);
            const kind = self.ast.getKind(inner_idx);
            switch (kind) {
                .func_decl => {
                    const short_name = self.getMemberName(inner_idx) orelse continue;
                    const qualified_name = try std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ ns_name, short_name });
                    const sym_idx = self.symbols.lookup(qualified_name) orelse continue;
                    try self.checkFuncDeclBody(inner_idx, sym_idx);
                },
                .const_decl => {
                    const short_name = self.getMemberName(inner_idx) orelse continue;
                    const qualified_name = try std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ ns_name, short_name });
                    try self.checkConstDeclWithName(inner_idx, qualified_name);
                },
                .var_decl => {
                    _ = try self.checkVarDecl(inner_idx);
                },
                .namespace_decl => try self.checkNamespaceDecl(inner_idx, ns_name),
                .import_decl, .c_include_decl, .c_import_block => try self.checkImportDecl(inner_idx),
                .struct_decl => {},
                else => {},
            }
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
                if (!self.typesCompatible(declared_type, et)) {
                    try self.addError(.{
                        .kind = .type_mismatch,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
            } else {
                // expression type is unresolved (e.g., literals without anchors)
                // check if the expression structure is compatible with declared type
                if (!self.isExprCompatibleWithType(decl.value, declared_type)) {
                    try self.addError(.{
                        .kind = .type_mismatch,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
            }
        }
    }

    /// Check all member declarations inside a namespace.
    fn checkNamespaceDecl(self: *SemanticContext, node_idx: NodeIndex, prefix: []const u8) !void {
        const ns_decl = self.ast.getNamespaceDecl(node_idx);
        const ns_name_ident = self.ast.getIdentifier(ns_decl.name);
        const ns_name_token = self.tokens.items[ns_name_ident.token_idx];
        const ns_name = self.src.getSlice(ns_name_token.start, ns_name_token.start + ns_name_token.len);

        const arena_alloc = self.types.arena.allocator();
        const qualified_prefix = if (prefix.len > 0)
            try std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ prefix, ns_name })
        else
            try arena_alloc.dupe(u8, ns_name);

        const saved_prefix = self.current_namespace_prefix;
        self.current_namespace_prefix = qualified_prefix;
        defer self.current_namespace_prefix = saved_prefix;

        const members = self.ast.getExtra(ns_decl.declarations);
        for (members) |member_idx| {
            const inner_idx = self.unwrapPubDecl(member_idx);
            const kind = self.ast.getKind(inner_idx);
            switch (kind) {
                .func_decl => {
                    const short_name = self.getMemberName(inner_idx) orelse continue;
                    const qualified_name = try std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ qualified_prefix, short_name });
                    const sym_idx = self.symbols.lookup(qualified_name) orelse continue;
                    try self.checkFuncDeclBody(inner_idx, sym_idx);
                },
                .const_decl => {
                    const short_name = self.getMemberName(inner_idx) orelse continue;
                    const qualified_name = try std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ qualified_prefix, short_name });
                    try self.checkConstDeclWithName(inner_idx, qualified_name);
                },
                .var_decl => {
                    _ = try self.checkVarDecl(inner_idx);
                },
                .namespace_decl => try self.checkNamespaceDecl(inner_idx, qualified_prefix),
                .struct_decl => {},
                else => {},
            }
        }
    }

    fn checkConstDeclWithName(self: *SemanticContext, node_idx: NodeIndex, name: []const u8) !void {
        const decl = self.ast.getConstDecl(node_idx);
        const loc = self.ast.getLocation(node_idx);

        const sym_idx = self.symbols.lookup(name) orelse return;
        const declared_type = self.symbols.getTypeId(sym_idx);

        const expr_type = try self.checkExpression(decl.value, declared_type);

        // Update symbol type if it was unresolved and the expression resolved it
        if (declared_type.isUnresolved()) {
            if (expr_type) |et| {
                if (!et.isUnresolved()) {
                    self.symbols.resolve(sym_idx, et);
                }
            }
        }

        if (!declared_type.isUnresolved()) {
            if (expr_type) |et| {
                if (!self.typesCompatible(declared_type, et)) {
                    try self.addError(.{
                        .kind = .type_mismatch,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
            } else {
                if (!self.isExprCompatibleWithType(decl.value, declared_type)) {
                    try self.addError(.{
                        .kind = .type_mismatch,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
            }
        }
    }

    fn checkFuncDecl(self: *SemanticContext, node_idx: NodeIndex) !void {
        const name_ident = self.ast.getIdentifier(self.ast.getFuncDecl(node_idx).name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        const sym_idx = self.symbols.lookup(name) orelse return;
        try self.checkFuncDeclBody(node_idx, sym_idx);
    }

    fn checkFuncDeclBody(self: *SemanticContext, node_idx: NodeIndex, sym_idx: SymbolIndex) !void {
        const decl = self.ast.getFuncDecl(node_idx);
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
                try self.addError(.{
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

        // check that non-void functions return on all code paths
        if (!expected_return_type.isVoid() and !expected_return_type.isUnresolved()) {
            if (!self.blockAlwaysReturns(decl.body.?)) {
                const loc = self.ast.getLocation(node_idx);
                try self.addError(.{
                    .kind = .missing_return,
                    .start = loc.start,
                    .end = loc.end,
                });
            }
        }
    }

    fn checkVarDecl(self: *SemanticContext, node_idx: NodeIndex) !TypeId {
        const decl = self.ast.getVarDecl(node_idx);

        // get declared type if explicit
        var expected_type: TypeId = .unresolved;

        if (decl.type_id) |type_idx| {
            if (self.resolveTypeNode(type_idx)) |tid| {
                expected_type = tid;
            } else {
                expected_type = self.resolveInferredArrayType(type_idx, decl.value);
            }
        }

        // check value expression, passing declared type as context so literals can resolve
        const value_type = try self.checkExpression(decl.value, expected_type);

        // verify that the expression type matches the declared type
        if (!expected_type.isUnresolved()) {
            if (value_type != null and !value_type.?.isUnresolved()) {
                if (!self.typesCompatible(expected_type, value_type.?)) {
                    const err_kind: @import("error.zig").SemanticErrorKind = if (expected_type.isPointer() and value_type.?.isPointer()) blk: {
                        const lp = self.types.getPointerType(expected_type) orelse break :blk .type_mismatch;
                        const rp = self.types.getPointerType(value_type.?) orelse break :blk .type_mismatch;
                        break :blk if (lp.is_mutable and !rp.is_mutable) .mutability_mismatch else .type_mismatch;
                    } else .type_mismatch;
                    const val_loc = self.ast.getLocation(decl.value);
                    try self.addError(.{
                        .kind = err_kind,
                        .start = val_loc.start,
                        .end = val_loc.end,
                    });
                }
            } else if (value_type == null) {
                // expression type is unresolved (e.g., literals without anchors)
                if (!self.isExprCompatibleWithType(decl.value, expected_type)) {
                    const loc = self.ast.getLocation(node_idx);
                    try self.addError(.{
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

        // post-pass: re-check var_decl value expressions for locals that were
        // resolved after their initial check (backward type inference).
        // iterate until fixed point to handle chains (x → &x → p).
        var changed = true;
        while (changed) {
            changed = false;
            for (statements) |stmt_idx| {
                if (self.ast.getKind(stmt_idx) == .var_decl) {
                    const stored = self.node_types.get(stmt_idx);
                    if (stored == null or !stored.?.isUnresolved()) continue;

                    const decl = self.ast.getVarDecl(stmt_idx);
                    const name_ident = self.ast.getIdentifier(decl.name);
                    const name_token = self.tokens.items[name_ident.token_idx];
                    const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

                    if (self.lookupLocalPtr(name)) |local| {
                        // re-check value expression with the local's current type as context
                        const ctx = local.type_id;
                        const new_type = try self.checkExpression(decl.value, ctx);
                        if (new_type) |nt| {
                            if (!nt.isUnresolved()) {
                                try self.node_types.put(self.allocator, stmt_idx, nt);
                                if (local.type_id.isUnresolved()) {
                                    local.type_id = nt;
                                }
                                changed = true;
                            }
                        }
                    }
                }
            }
        }

        const deferred = self.ast.getExtra(block.deferred);
        for (deferred) |stmt_idx| {
            try self.checkStatement(stmt_idx);
        }
    }

    /// Returns true if the block is guaranteed to return on all code paths.
    fn blockAlwaysReturns(self: *SemanticContext, node_idx: NodeIndex) bool {
        const block = self.ast.getBlock(node_idx);
        const statements = self.ast.getExtra(block.statements);
        if (statements.len == 0) return false;

        // check the last statement
        const last = statements[statements.len - 1];
        const kind = self.ast.getKind(last);

        return switch (kind) {
            .return_stmt => true,
            .if_stmt => self.ifAlwaysReturns(last),
            .block => self.blockAlwaysReturns(last),
            else => false,
        };
    }

    /// Returns true if an if-statement returns on all branches.
    fn ifAlwaysReturns(self: *SemanticContext, node_idx: NodeIndex) bool {
        const if_stmt = self.ast.getIf(node_idx);

        // must have an else branch to cover all paths
        const else_blk = if_stmt.else_block orelse return false;

        // check the if-block
        if (!self.blockAlwaysReturns(if_stmt.if_block)) return false;

        // check all else-if blocks
        for (0..if_stmt.elseIfCount()) |i| {
            const pair = if_stmt.getElseIf(self.ast, i) orelse return false;
            if (!self.blockAlwaysReturns(pair.block)) return false;
        }

        // check the else block
        return self.blockAlwaysReturns(else_blk);
    }

    fn checkStatement(self: *SemanticContext, node_idx: NodeIndex) TypeError!void {
        const kind = self.ast.getKind(node_idx);
        switch (kind) {
            .var_decl => {
                _ = try self.checkLocalVarDecl(node_idx);
            },
            .assignment => try self.checkAssignment(node_idx),
            .return_stmt => try self.checkReturn(node_idx),
            .if_stmt => try self.checkIfStmt(node_idx),
            .while_stmt => try self.checkWhileStmt(node_idx),
            .break_stmt => {
                if (self.loop_depth == 0) {
                    const loc = self.ast.getLocation(node_idx);
                    try self.addError(.{ .kind = .break_outside_loop, .start = loc.start, .end = loc.end });
                }
            },
            .continue_stmt => {
                if (self.loop_depth == 0) {
                    const loc = self.ast.getLocation(node_idx);
                    try self.addError(.{ .kind = .continue_outside_loop, .start = loc.start, .end = loc.end });
                }
            },
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
        const target_kind = self.ast.getKind(assign.target);

        var target_type: TypeId = .unresolved;
        var is_mutable = false;

        if (target_kind == .deref) {
            // pointer dereference assignment: p^ = 10
            const deref = self.ast.getDeref(assign.target);

            // type-check the deref to get target type
            if (try self.checkExpression(assign.target, .unresolved)) |dt| {
                target_type = dt;
            }

            // check the pointer is mutable
            const ptr_type = try self.checkExpression(deref.operand, .unresolved);
            if (ptr_type) |pt| {
                if (pt.isPointer()) {
                    if (self.types.getPointerType(pt)) |ptr_info| {
                        is_mutable = ptr_info.is_mutable;
                        if (!ptr_info.is_mutable) {
                            try self.addError(.{
                                .kind = .assign_through_immutable_ptr,
                                .start = loc.start,
                                .end = loc.end,
                            });
                        }
                    }
                }
            }
        } else if (target_kind == .field_access) {
            // field assignment: p.x = 10
            // type-check the field access expression (stores types for all intermediate nodes)
            if (try self.checkExpression(assign.target, .unresolved)) |ft| {
                target_type = ft;
            }

            // walk to root identifier to check mutability
            var current = assign.target;
            while (self.ast.getKind(current) == .field_access) {
                const fa = self.ast.getFieldAccess(current);
                current = fa.object;
            }
            if (self.ast.getKind(current) == .identifier) {
                const ident = self.ast.getIdentifier(current);
                const token = self.tokens.items[ident.token_idx];
                const name = self.src.getSlice(token.start, token.start + token.len);
                if (self.lookupLocalPtr(name)) |local| {
                    local.referenced = true;
                    // if root is a pointer, check pointer mutability (auto-deref)
                    if (local.type_id.isPointer()) {
                        if (self.types.getPointerType(local.type_id)) |ptr_info| {
                            is_mutable = ptr_info.is_mutable;
                            if (!is_mutable) {
                                try self.addError(.{
                                    .kind = .assign_through_immutable_ptr,
                                    .start = loc.start,
                                    .end = loc.end,
                                });
                                return;
                            }
                        }
                    } else {
                        is_mutable = local.is_mutable;
                    }
                } else if (self.symbols.lookup(name)) |sym_idx| {
                    self.symbols.markReferenced(sym_idx);
                    const sym_type = self.symbols.getTypeId(sym_idx);
                    if (sym_type.isPointer()) {
                        if (self.types.getPointerType(sym_type)) |ptr_info| {
                            is_mutable = ptr_info.is_mutable;
                            if (!is_mutable) {
                                try self.addError(.{
                                    .kind = .assign_through_immutable_ptr,
                                    .start = loc.start,
                                    .end = loc.end,
                                });
                                return;
                            }
                        }
                    } else {
                        is_mutable = self.symbols.isMutable(sym_idx);
                    }
                }
            }
        } else if (target_kind == .array_index) {
            // array element assignment: arr[i] = 10
            // Element mutability is controlled by the array type ([N]mut T),
            // not by whether the variable itself is mut. This is consistent
            // with pointer dereference: p^ = 10 checks pointer mutability,
            // not variable mutability.
            if (try self.checkExpression(assign.target, .unresolved)) |et| {
                target_type = et;
            }

            // mark the root identifier as referenced
            var current = assign.target;
            while (self.ast.getKind(current) == .array_index) {
                const ai = self.ast.getArrayIndex(current);
                current = ai.object;
            }
            while (self.ast.getKind(current) == .field_access) {
                const access = self.ast.getFieldAccess(current);
                current = access.object;
            }
            if (self.ast.getKind(current) == .identifier) {
                const ident = self.ast.getIdentifier(current);
                const token = self.tokens.items[ident.token_idx];
                const name = self.src.getSlice(token.start, token.start + token.len);
                if (self.lookupLocalPtr(name)) |local| {
                    local.referenced = true;
                } else if (self.symbols.lookup(name)) |sym_idx| {
                    self.symbols.markReferenced(sym_idx);
                }
            }

            // check array/slice element mutability
            const idx_node = self.ast.getArrayIndex(assign.target);
            if (self.node_types.get(idx_node.object)) |obj_type| {
                if (obj_type.isArray()) {
                    if (self.types.getArrayType(obj_type)) |arr_info| {
                        if (!arr_info.is_mutable) {
                            try self.addError(.{
                                .kind = .assign_to_immutable_element,
                                .start = loc.start,
                                .end = loc.end,
                            });
                            try self.node_types.put(self.allocator, node_idx, .unresolved);
                            return;
                        }
                    }
                } else if (obj_type.isSlice()) {
                    if (self.types.getSliceType(obj_type)) |slice_info| {
                        if (!slice_info.is_mutable) {
                            try self.addError(.{
                                .kind = .assign_to_immutable_element,
                                .start = loc.start,
                                .end = loc.end,
                            });
                            try self.node_types.put(self.allocator, node_idx, .unresolved);
                            return;
                        }
                    }
                }
            }

            // bypass variable mutability check — element mutability is sufficient
            is_mutable = true;
        } else {
            // identifier assignment: x = 10
            const target_ident = self.ast.getIdentifier(assign.target);
            const target_token = self.tokens.items[target_ident.token_idx];
            const target_name = self.src.getSlice(target_token.start, target_token.start + target_token.len);

            if (self.lookupLocalPtr(target_name)) |local| {
                target_type = local.type_id;
                is_mutable = local.is_mutable;
                local.referenced = true;
            } else if (self.symbols.lookup(target_name)) |sym_idx| {
                target_type = self.symbols.getTypeId(sym_idx);
                is_mutable = self.symbols.isMutable(sym_idx);
                self.symbols.markReferenced(sym_idx);
            } else {
                try self.addError(.{
                    .kind = .undefined_symbol,
                    .start = loc.start,
                    .end = loc.end,
                });
                return;
            }

            // store target type for codegen
            try self.node_types.put(self.allocator, assign.target, target_type);
        }

        // check mutability
        // for deref assignments with unresolved pointer type, skip — can't verify yet
        const skip_mutability_check = target_kind == .deref and target_type.isUnresolved();
        if (!is_mutable and !skip_mutability_check) {
            try self.addError(.{
                .kind = .assignment_to_immutable,
                .start = loc.start,
                .end = loc.end,
            });
            try self.node_types.put(self.allocator, node_idx, .unresolved);
            return;
        }

        // check value type
        const value_type = try self.checkExpression(assign.value, target_type);
        if (value_type) |vt| {
            if (!self.typesCompatible(target_type, vt)) {
                try self.addError(.{
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
            if (!self.typesCompatible(self.current_ret_type, et)) {
                try self.addError(.{
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

    fn checkWhileStmt(self: *SemanticContext, node_idx: NodeIndex) !void {
        const while_stmt = self.ast.getWhile(node_idx);

        // check condition is boolean
        try self.checkGuardIsBool(while_stmt.condition);

        // check body in new scope, with loop context
        self.loop_depth += 1;
        try self.pushScope();
        try self.checkBlock(while_stmt.body);
        self.popScope();
        self.loop_depth -= 1;

        // check continue expression if present
        if (while_stmt.cont_expr) |cont| {
            try self.checkStatement(cont);
        }
    }

    fn checkGuardIsBool(self: *SemanticContext, guard_idx: NodeIndex) !void {
        const guard_type = try self.checkExpression(guard_idx, TypeId.bool);
        const loc = self.ast.getLocation(guard_idx);

        if (guard_type) |gt| {
            if (!gt.isBool() and !gt.isUnresolved()) {
                try self.addError(.{
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
            .struct_literal => try self.checkStructLiteral(node_idx),
            .address_of => try self.checkAddressOf(node_idx, context_type),
            .deref => try self.checkDeref(node_idx, context_type),
            .array_literal => try self.checkArrayLiteral(node_idx, context_type),
            .array_index => try self.checkArrayIndex(node_idx),
            .array_slice => try self.checkArraySlice(node_idx),
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

        // string literals are always [:0]u8 (null-terminated immutable slice)
        if (token.kind == .string_literal) {
            return try self.types.addSliceTypeWithSentinel(.{ .primitive = .u8 }, false, 0);
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
                // cascade through the local's value expression (e.g. &x → resolve x)
                if (local.node_idx != 0) {
                    const var_decl = self.ast.getVarDecl(local.node_idx);
                    self.propagateType(var_decl.value, context_type);
                }
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
        try self.addError(.{
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
                        try self.addError(.{
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
                        try self.addError(.{
                            .kind = .arithmetic_op_requires_numeric,
                            .start = loc.start,
                            .end = loc.end,
                        });
                        return null;
                    }

                    // can only negate signed types and floats
                    if (t.isInteger() and !t.isSignedInteger()) {
                        try self.addError(.{
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

    fn checkAddressOf(self: *SemanticContext, node_idx: NodeIndex, context_type: TypeId) !?TypeId {
        const addr = self.ast.getAddressOf(node_idx);
        const loc = self.ast.getLocation(node_idx);
        const operand_kind = self.ast.getKind(addr.operand);

        // only variables and field accesses are addressable
        if (operand_kind != .identifier and operand_kind != .field_access) {
            try self.addError(.{
                .kind = .cannot_take_address,
                .start = loc.start,
                .end = loc.end,
            });
            return null;
        }

        // determine pointee context type from outer pointer context
        var pointee_context: TypeId = .unresolved;
        if (!context_type.isUnresolved() and context_type.isPointer()) {
            if (self.types.getPointerType(context_type)) |ptr_info| {
                pointee_context = ptr_info.pointee;
            }
        }

        // check operand type
        const operand_type = try self.checkExpression(addr.operand, pointee_context) orelse return null;

        // if operand is unresolved, the whole address-of is unresolved
        // (so context from the consumer can flow back through checkIdentifier later)
        if (operand_type.isUnresolved()) return .unresolved;

        // determine mutability from the target
        var is_mutable = false;
        if (operand_kind == .identifier) {
            const ident = self.ast.getIdentifier(addr.operand);
            const token = self.tokens.items[ident.token_idx];
            const name = self.src.getSlice(token.start, token.start + token.len);
            if (self.lookupLocalPtr(name)) |local| {
                is_mutable = local.is_mutable;
            } else if (self.symbols.lookup(name)) |sym_idx| {
                is_mutable = self.symbols.isMutable(sym_idx);
            }
        } else if (operand_kind == .field_access) {
            // walk to root identifier to check mutability
            var current = addr.operand;
            while (self.ast.getKind(current) == .field_access) {
                const access = self.ast.getFieldAccess(current);
                current = access.object;
            }
            if (self.ast.getKind(current) == .identifier) {
                const ident = self.ast.getIdentifier(current);
                const token = self.tokens.items[ident.token_idx];
                const name = self.src.getSlice(token.start, token.start + token.len);
                if (self.lookupLocalPtr(name)) |local| {
                    is_mutable = local.is_mutable;
                } else if (self.symbols.lookup(name)) |sym_idx| {
                    is_mutable = self.symbols.isMutable(sym_idx);
                }
            }
        }

        // produce many-item pointer if context expects one, single-item otherwise
        const is_many_item = if (self.types.getPointerType(context_type)) |ctx_ptr| ctx_ptr.is_many_item else false;
        return self.types.addPointerType(operand_type, is_mutable, is_many_item) catch return null;
    }

    fn checkDeref(self: *SemanticContext, node_idx: NodeIndex, context_type: TypeId) !?TypeId {
        const deref = self.ast.getDeref(node_idx);
        const loc = self.ast.getLocation(node_idx);
        _ = context_type;

        // check operand type
        const operand_type = try self.checkExpression(deref.operand, .unresolved) orelse return null;

        // if operand is unresolved, propagate unresolved (will be resolved later)
        if (operand_type.isUnresolved()) return .unresolved;

        // verify it's a pointer
        if (!operand_type.isPointer()) {
            try self.addError(.{
                .kind = .deref_non_pointer,
                .start = loc.start,
                .end = loc.end,
            });
            return null;
        }

        // return pointee type
        const ptr_info = self.types.getPointerType(operand_type) orelse return null;
        return ptr_info.pointee;
    }

    fn checkBinaryOp(self: *SemanticContext, node_idx: NodeIndex, context_type: TypeId) !?TypeId {
        const binary_op = self.ast.getBinaryOp(node_idx);
        const loc = self.ast.getLocation(node_idx);

        if (binary_op.isArithmetic()) {
            // infer operand types, threading context for numeric propagation
            const left_type = try self.checkExpression(binary_op.left, context_type);
            const right_type = try self.checkExpression(binary_op.right, context_type);

            const operand_type = left_type orelse right_type;

            // pointer arithmetic: allow for many-item pointers, reject for single-item
            const left_is_ptr = if (left_type) |t| t.isPointer() else false;
            const right_is_ptr = if (right_type) |t| t.isPointer() else false;
            if (left_is_ptr or right_is_ptr) {
                const left_is_many = if (left_type) |t| self.types.isManyItemPointer(t) else false;
                const right_is_many = if (right_type) |t| self.types.isManyItemPointer(t) else false;

                if (left_is_many and (binary_op.op == .add or binary_op.op == .sub)) {
                    // many-item pointer +/- integer: re-check offset as usize
                    _ = try self.checkExpression(binary_op.right, TypeId.usize);
                    return left_type;
                } else if (right_is_many and binary_op.op == .add) {
                    // integer + many-item pointer: re-check offset as usize
                    _ = try self.checkExpression(binary_op.left, TypeId.usize);
                    return right_type;
                }

                // single-item pointer arithmetic is not allowed
                try self.addError(.{
                    .kind = .pointer_arithmetic,
                    .start = loc.start,
                    .end = loc.end,
                });
                return operand_type;
            }

            if (operand_type) |t| {
                if (!t.isNumeric() and !t.isUnresolved()) {
                    try self.addError(.{
                        .kind = .arithmetic_op_requires_numeric,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
                if (left_type != null and right_type != null) {
                    if (!self.typesCompatible(left_type.?, right_type.?)) {
                        try self.addError(.{
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
            // infer operand types (no context initially)
            var left_type = try self.checkExpression(binary_op.left, .unresolved);
            var right_type = try self.checkExpression(binary_op.right, .unresolved);

            // propagate resolved type to unresolved literals (e.g. `x == 3` where x is f32)
            if (left_type != null and right_type == null) {
                right_type = try self.checkExpression(binary_op.right, left_type.?);
            } else if (right_type != null and left_type == null) {
                left_type = try self.checkExpression(binary_op.left, right_type.?);
            }

            // if both sides have resolved types, they must match
            if (left_type != null and right_type != null) {
                if (!self.typesCompatible(left_type.?, right_type.?)) {
                    try self.addError(.{
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
                try self.addError(.{
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
                try self.addError(.{
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
        const func_node_kind = self.ast.getKind(call.func);

        var func_type_id: TypeId = .unresolved;

        if (func_node_kind == .identifier) {
            // direct call: func_name(args...)
            const func_ident = self.ast.getIdentifier(call.func);
            const func_token = self.tokens.items[func_ident.token_idx];
            const func_name = self.src.getSlice(func_token.start, func_token.start + func_token.len);

            const sym_idx = self.symbols.lookup(func_name) orelse {
                try self.addError(.{
                    .kind = .undefined_symbol,
                    .start = loc.start,
                    .end = loc.end,
                });
                return null;
            };

            self.symbols.markReferenced(sym_idx);

            if (self.symbols.getKind(sym_idx) != .function) {
                try self.addError(.{
                    .kind = .not_callable,
                    .start = loc.start,
                    .end = loc.end,
                });
                return null;
            }

            func_type_id = self.symbols.getTypeId(sym_idx);
        } else if (func_node_kind == .field_access) {
            // namespace call: ns.func(args...)
            // checkFieldAccess handles namespace member resolution and pub checking
            const ft = try self.checkFieldAccess(call.func) orelse return null;

            if (!ft.isFunction()) {
                try self.addError(.{
                    .kind = .not_callable,
                    .start = loc.start,
                    .end = loc.end,
                });
                return null;
            }

            func_type_id = ft;
        } else {
            try self.addError(.{
                .kind = .not_callable,
                .start = loc.start,
                .end = loc.end,
            });
            return null;
        }

        // get function type from registry
        const func_type_info = self.types.getFunctionType(func_type_id);
        const param_types = self.types.getParamTypes(func_type_id) orelse &[_]TypeId{};
        const return_type = self.types.getReturnType(func_type_id) orelse TypeId.void;
        const is_variadic = if (func_type_info) |ft| ft.is_variadic else false;

        // get args
        const args = self.ast.getExtra(call.args);

        // check args count — variadic functions allow extra args
        if (is_variadic) {
            if (args.len < param_types.len) {
                try self.addError(.{
                    .kind = .argument_count_mismatch,
                    .start = loc.start,
                    .end = loc.end,
                });
            }
        } else {
            if (args.len != param_types.len) {
                try self.addError(.{
                    .kind = .argument_count_mismatch,
                    .start = loc.start,
                    .end = loc.end,
                });
            }
        }

        // check each arg type (fixed params get matched, variadic extras just get type-checked)
        for (0..args.len) |i| {
            const arg_idx = args[i];
            const expected_type: TypeId = if (i < param_types.len) param_types[i] else .unresolved;

            const arg_type = try self.checkExpression(arg_idx, expected_type);
            // skip compatibility check for variadic extra args (no expected type)
            if (i >= param_types.len) continue;
            if (arg_type) |at| {
                if (!self.typesCompatible(expected_type, at)) {
                    const arg_loc = self.ast.getLocation(arg_idx);
                    const err_kind: @import("error.zig").SemanticErrorKind = if (expected_type.isPointer() and at.isPointer()) blk: {
                        const lp = self.types.getPointerType(expected_type) orelse break :blk .argument_type_mismatch;
                        const rp = self.types.getPointerType(at) orelse break :blk .argument_type_mismatch;
                        break :blk if (lp.is_mutable and !rp.is_mutable) .mutability_mismatch else .argument_type_mismatch;
                    } else .argument_type_mismatch;
                    try self.addError(.{
                        .kind = err_kind,
                        .start = arg_loc.start,
                        .end = arg_loc.end,
                    });
                    // Mark as unresolved so codegen traps (width .w32 ≠ .ptr)
                    try self.node_types.put(self.allocator, arg_idx, .unresolved);
                }
            }
        }

        return return_type;
    }

    /// Get field name from a field access node — handles both identifier (.name) and literal (.0) fields.
    fn getFieldName(self: *SemanticContext, field_node: NodeIndex) []const u8 {
        const kind = self.ast.getKind(field_node);
        if (kind == .literal) {
            const lit = self.ast.getLiteral(field_node);
            const token = self.tokens.items[lit.token_idx];
            return self.src.getSlice(token.start, token.start + token.len);
        }
        const ident = self.ast.getIdentifier(field_node);
        const token = self.tokens.items[ident.token_idx];
        return self.src.getSlice(token.start, token.start + token.len);
    }

    fn checkFieldAccess(self: *SemanticContext, node_idx: NodeIndex) !?TypeId {
        const access = self.ast.getFieldAccess(node_idx);
        const loc = self.ast.getLocation(node_idx);

        // type-check the object expression
        const object_type = try self.checkExpression(access.object, .unresolved);

        if (object_type) |raw_ot| {
            // auto-deref: unwrap pointer layers to reach the underlying type
            var ot = raw_ot;
            while (ot.isPointer()) {
                const ptr_info = self.types.getPointerType(ot) orelse break;
                ot = ptr_info.pointee;
            }

            if (ot.isStruct()) {
                const struct_type = self.types.getStructType(ot) orelse return null;

                // get field name — identifier for named fields, literal for tuple .0 .1
                const field_name = self.getFieldName(access.field);

                // look up field
                for (struct_type.fields) |field| {
                    if (mem.eql(u8, field.name, field_name)) {
                        return field.type_id;
                    }
                }

                // field not found
                try self.addError(.{
                    .kind = .no_such_field,
                    .start = loc.start,
                    .end = loc.end,
                });
                return null;
            } else if (ot.isNamespace()) {
                const ns_type = self.types.getNamespaceType(ot) orelse return null;

                // get field name
                const field_ident = self.ast.getIdentifier(access.field);
                const field_token = self.tokens.items[field_ident.token_idx];
                const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);

                // look up member in namespace
                for (ns_type.members) |member| {
                    if (mem.eql(u8, member.name, field_name)) {
                        // check pub visibility
                        if (!member.is_pub) {
                            try self.addError(.{
                                .kind = .access_private_member,
                                .start = field_token.start,
                                .end = field_token.start + field_token.len,
                            });
                        }
                        // mark member symbol as referenced
                        self.symbols.markReferenced(member.symbol_idx);
                        return self.symbols.getTypeId(member.symbol_idx);
                    }
                }

                // member not found
                try self.addError(.{
                    .kind = .no_such_field,
                    .start = field_token.start,
                    .end = field_token.start + field_token.len,
                });
                return null;
            } else if (ot.isSlice()) {
                // slice built-in properties: .len
                const field_ident = self.ast.getIdentifier(access.field);
                const field_token = self.tokens.items[field_ident.token_idx];
                const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);

                if (mem.eql(u8, field_name, "len")) {
                    return TypeId.usize;
                }

                try self.addError(.{
                    .kind = .no_such_field,
                    .start = field_token.start,
                    .end = field_token.start + field_token.len,
                });
                return null;
            } else if (ot.isArray()) {
                // array built-in properties: .len
                const field_ident = self.ast.getIdentifier(access.field);
                const field_token = self.tokens.items[field_ident.token_idx];
                const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);

                if (mem.eql(u8, field_name, "len")) {
                    return TypeId.usize;
                }

                try self.addError(.{
                    .kind = .no_such_field,
                    .start = field_token.start,
                    .end = field_token.start + field_token.len,
                });
                return null;
            } else if (!ot.isUnresolved()) {
                // dot access on non-struct/non-namespace
                try self.addError(.{
                    .kind = .field_access_on_non_struct,
                    .start = loc.start,
                    .end = loc.end,
                });
                return null;
            }
        }

        return null;
    }

    fn checkStructLiteral(self: *SemanticContext, node_idx: NodeIndex) !?TypeId {
        const lit = self.ast.getStructLiteral(node_idx);
        const loc = self.ast.getLocation(node_idx);

        // anonymous tuple literal: { expr, expr, ... }
        if (lit.type_name == std.math.maxInt(NodeIndex)) {
            return try self.checkAnonymousTupleLiteral(node_idx);
        }

        // resolve type name to struct type — handle both identifier and field_access
        const type_id: TypeId = blk: {
            if (self.ast.getKind(lit.type_name) == .field_access) {
                // qualified name like geometry.Vec2 — checkFieldAccess handles
                // namespace lookup, pub checking, and marking as referenced
                const resolved = try self.checkFieldAccess(lit.type_name) orelse return null;
                break :blk resolved;
            } else {
                const type_ident = self.ast.getIdentifier(lit.type_name);
                const type_token = self.tokens.items[type_ident.token_idx];
                const type_name = self.src.getSlice(type_token.start, type_token.start + type_token.len);

                const sym_idx = self.symbols.lookup(type_name) orelse ns_blk: {
                    // Try with namespace prefix for imports (e.g. Color → rl.Color)
                    if (self.current_namespace_prefix.len > 0) {
                        var buf: [512]u8 = undefined;
                        const qualified = std.fmt.bufPrint(&buf, "{s}.{s}", .{ self.current_namespace_prefix, type_name }) catch break :ns_blk null;
                        break :ns_blk self.symbols.lookup(qualified);
                    }
                    break :ns_blk null;
                } orelse {
                    try self.addError(.{
                        .kind = .undefined_symbol,
                        .start = type_token.start,
                        .end = type_token.start + type_token.len,
                    });
                    return null;
                };

                self.symbols.markReferenced(sym_idx);

                if (self.symbols.getKind(sym_idx) != .@"type") {
                    try self.addError(.{
                        .kind = .unknown_type,
                        .start = type_token.start,
                        .end = type_token.start + type_token.len,
                    });
                    return null;
                }

                break :blk self.symbols.getTypeId(sym_idx);
            }
        };
        const struct_type = self.types.getStructType(type_id) orelse return null;

        // parse field pairs from extra_data
        const field_data = self.ast.getExtra(lit.fields);

        // track which fields have been set (for duplicate/missing detection)
        var seen_fields = std.StringHashMap(void).init(self.allocator);
        defer seen_fields.deinit();

        var positional_idx: usize = 0;

        var fi: usize = 0;
        while (fi < field_data.len) : (fi += 2) {
            const field_name_idx = field_data[fi];
            const field_value_idx = field_data[fi + 1];

            // get field name — sentinel means positional (tuple literal)
            const field_name = if (field_name_idx == std.math.maxInt(NodeIndex)) blk: {
                const name = tupleFieldName(self.allocator, positional_idx) catch "?";
                positional_idx += 1;
                break :blk name;
            } else blk: {
                const field_ident = self.ast.getIdentifier(field_name_idx);
                const field_token = self.tokens.items[field_ident.token_idx];
                break :blk self.src.getSlice(field_token.start, field_token.start + field_token.len);
            };

            // check for duplicate fields (skip duplicate check for positional, but still record)
            if (field_name_idx != std.math.maxInt(NodeIndex)) {
                if (seen_fields.contains(field_name)) {
                    const dup_loc = self.ast.getLocation(field_name_idx);
                    try self.addError(.{
                        .kind = .duplicate_literal_field,
                        .start = dup_loc.start,
                        .end = dup_loc.end,
                    });
                } else {
                    try seen_fields.put(field_name, {});
                }
            } else {
                try seen_fields.put(field_name, {});
            }

            // find field in struct definition
            var found = false;
            for (struct_type.fields) |field| {
                if (mem.eql(u8, field.name, field_name)) {
                    found = true;
                    // check value type against field type
                    const value_type = try self.checkExpression(field_value_idx, field.type_id);
                    if (value_type) |vt| {
                        if (!self.typesCompatible(field.type_id, vt)) {
                            const val_loc = self.ast.getLocation(field_value_idx);
                            try self.addError(.{
                                .kind = .type_mismatch,
                                .start = val_loc.start,
                                .end = val_loc.end,
                            });
                        }
                    }
                    break;
                }
            }

            if (!found) {
                const val_loc = self.ast.getLocation(field_value_idx);
                try self.addError(.{
                    .kind = .no_such_field,
                    .start = val_loc.start,
                    .end = val_loc.end,
                });
            }
        }

        // check all struct fields are initialized
        for (struct_type.fields) |field| {
            if (!seen_fields.contains(field.name)) {
                try self.addError(.{
                    .kind = .missing_field,
                    .start = loc.start,
                    .end = loc.end,
                });
                break; // one error is enough
            }
        }

        return type_id;
    }

    fn checkAnonymousTupleLiteral(self: *SemanticContext, node_idx: NodeIndex) !?TypeId {
        const lit = self.ast.getStructLiteral(node_idx);
        const field_data = self.ast.getExtra(lit.fields);
        const field_count = field_data.len / 2;

        // collect element types by type-checking each value
        var field_names = try std.ArrayList([]const u8).initCapacity(self.allocator, field_count);
        defer field_names.deinit(self.allocator);
        var field_types = try std.ArrayList(TypeId).initCapacity(self.allocator, field_count);
        defer field_types.deinit(self.allocator);

        var fi: usize = 0;
        var idx: usize = 0;
        while (fi < field_data.len) : (fi += 2) {
            const field_value_idx = field_data[fi + 1];
            // Let numeric literals stay unresolved — backward inference resolves them
            // from usage context (e.g. return type, variable type annotation)
            const value_type = try self.checkExpression(field_value_idx, .unresolved) orelse TypeId.unresolved;
            try field_names.append(self.allocator, tupleFieldName(self.allocator, idx) catch "?");
            try field_types.append(self.allocator, value_type);
            idx += 1;
        }

        // create anonymous struct type
        const anon_name = std.fmt.allocPrint(self.types.arena.allocator(), "__anon_tuple_{d}", .{self.types.struct_types.items.len}) catch return null;
        const type_id = try self.types.reserveStructType(anon_name);
        try self.types.finalizeStructType(type_id, field_names.items, field_types.items, .honey);

        return type_id;
    }

    fn checkArrayLiteral(self: *SemanticContext, node_idx: NodeIndex, context_type: TypeId) !?TypeId {
        const lit = self.ast.getArrayLiteral(node_idx);
        const loc = self.ast.getLocation(node_idx);
        const elements = self.ast.getExtra(lit.elements);

        // determine element type and expected length from context
        var element_type: TypeId = .unresolved;
        var expected_length: ?u32 = null;

        if (context_type.isArray()) {
            if (self.types.getArrayType(context_type)) |arr_info| {
                element_type = arr_info.element_type;
                expected_length = arr_info.length;
            }
        }

        // check length matches if context provides one
        if (expected_length) |exp_len| {
            if (elements.len != exp_len) {
                try self.addError(.{
                    .kind = .array_length_mismatch,
                    .start = loc.start,
                    .end = loc.end,
                });
            }
        }

        // type-check each element
        for (elements) |elem_idx| {
            const elem_type = try self.checkExpression(elem_idx, element_type);
            // if we don't have an element type yet, infer from first element
            if (element_type.isUnresolved()) {
                if (elem_type) |et| {
                    if (!et.isUnresolved()) {
                        element_type = et;
                    }
                }
            }
        }

        if (element_type.isUnresolved()) return null;

        const length: u32 = @intCast(elements.len);
        const is_mutable = if (context_type.isArray())
            if (self.types.getArrayType(context_type)) |info| info.is_mutable else false
        else
            false;
        return self.types.addArrayType(element_type, length, is_mutable) catch return null;
    }

    fn checkArrayIndex(self: *SemanticContext, node_idx: NodeIndex) !?TypeId {
        const idx_node = self.ast.getArrayIndex(node_idx);
        const loc = self.ast.getLocation(node_idx);

        // check the object being indexed
        const obj_type = try self.checkExpression(idx_node.object, .unresolved);

        // check the index expression — must be integer (context: usize)
        const index_type = try self.checkExpression(idx_node.index, TypeId.usize);
        if (index_type) |it| {
            if (!it.isInteger() and !it.isUnresolved()) {
                try self.addError(.{
                    .kind = .index_not_integer,
                    .start = loc.start,
                    .end = loc.end,
                });
            }
        }

        // object must be an array or slice type
        if (obj_type) |ot| {
            if (ot.isArray()) {
                if (self.types.getArrayType(ot)) |arr_info| {
                    return arr_info.element_type;
                }
            } else if (ot.isSlice()) {
                if (self.types.getSliceType(ot)) |slice_info| {
                    return slice_info.element_type;
                }
            } else if (!ot.isUnresolved()) {
                try self.addError(.{
                    .kind = .index_non_array,
                    .start = loc.start,
                    .end = loc.end,
                });
            }
        }

        return null;
    }

    fn checkArraySlice(self: *SemanticContext, node_idx: NodeIndex) !?TypeId {
        const slice_node = self.ast.getArraySlice(node_idx);
        const loc = self.ast.getLocation(node_idx);

        // check the object being sliced
        const obj_type = try self.checkExpression(slice_node.object, .unresolved);

        // check start and end expressions (if present) — must be integers (context: usize)
        if (slice_node.range_start) |start_idx| {
            const start_type = try self.checkExpression(start_idx, TypeId.usize);
            if (start_type) |st| {
                if (!st.isInteger() and !st.isUnresolved()) {
                    try self.addError(.{
                        .kind = .index_not_integer,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
            }
        }

        if (slice_node.range_end) |end_idx| {
            const end_type = try self.checkExpression(end_idx, TypeId.usize);
            if (end_type) |et| {
                if (!et.isInteger() and !et.isUnresolved()) {
                    try self.addError(.{
                        .kind = .index_not_integer,
                        .start = loc.start,
                        .end = loc.end,
                    });
                }
            }
        }

        // object must be an array or slice type — slicing produces a slice
        if (obj_type) |ot| {
            if (ot.isArray()) {
                if (self.types.getArrayType(ot)) |arr_info| {
                    return try self.types.addSliceType(arr_info.element_type, arr_info.is_mutable);
                }
            } else if (ot.isSlice()) {
                // slicing a slice drops any sentinel guarantee
                if (self.types.getSliceType(ot)) |slice_info| {
                    return try self.types.addSliceType(slice_info.element_type, slice_info.is_mutable);
                }
                return ot;
            } else if (!ot.isUnresolved()) {
                try self.addError(.{
                    .kind = .index_non_array,
                    .start = loc.start,
                    .end = loc.end,
                });
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

    fn typesCompatible(self: *const SemanticContext, left: TypeId, right: TypeId) bool {
        // unresolved types are compatible with everything (errors caught at runtime)
        if (left.isUnresolved() or right.isUnresolved()) {
            return true;
        }

        // pointer types: structural comparison, mut → immutable is allowed
        if (left.isPointer() and right.isPointer()) {
            const lp = self.types.getPointerType(left) orelse return false;
            const rp = self.types.getPointerType(right) orelse return false;
            // single-item → many-item is not allowed (loosening); reverse is fine (tightening)
            if (lp.is_many_item and !rp.is_many_item) return false;
            // mut T → T is allowed (mutable to immutable)
            // T → mut T is not allowed
            if (lp.is_mutable and !rp.is_mutable) {
                // expected mutable, got immutable — not OK
                return false;
            }
            return self.typesCompatible(lp.pointee, rp.pointee);
        }

        // array types: structural comparison, [N]mut T → [N]T is allowed (tightening)
        if (left.isArray() and right.isArray()) {
            const la = self.types.getArrayType(left) orelse return false;
            const ra = self.types.getArrayType(right) orelse return false;
            if (la.length != ra.length) return false;
            // expected mutable, got immutable — not OK
            if (la.is_mutable and !ra.is_mutable) return false;
            // sentinel must match (no implicit sentinel coercion for arrays)
            if (la.sentinel != ra.sentinel) return false;
            return self.typesCompatible(la.element_type, ra.element_type);
        }

        // slice types: structural comparison with mutability and sentinel coercion
        if (left.isSlice() and right.isSlice()) {
            const ls = self.types.getSliceType(left) orelse return false;
            const rs = self.types.getSliceType(right) orelse return false;
            // expected mutable, got immutable — not OK
            if (ls.is_mutable and !rs.is_mutable) return false;
            // sentinel coercion: [:0]T → []T is OK (dropping sentinel guarantee)
            // []T → [:0]T is NOT OK (can't guarantee sentinel exists)
            if (ls.sentinel != null and rs.sentinel == null) return false;
            return self.typesCompatible(ls.element_type, rs.element_type);
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

        // check all scopes for shadowing (current scope catches param/local conflicts)
        for (self.scopes.items) |scope| {
            if (scope.locals.contains(name)) {
                const loc = self.ast.getLocation(node_idx);
                try self.addError(.{
                    .kind = .variable_shadowing,
                    .start = loc.start,
                    .end = loc.end,
                });
                break;
            }
        }

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
