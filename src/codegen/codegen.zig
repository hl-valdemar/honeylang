const std = @import("std");
const mem = std.mem;

const ComptimeResult = @import("../comptime/comptime.zig").ComptimeResult;
const SymbolTable = @import("../semantic/symbols.zig").SymbolTable;
const SymbolIndex = @import("../semantic/symbols.zig").SymbolIndex;
const types_mod = @import("../semantic/types.zig");
const TypeId = types_mod.TypeId;
const PrimitiveType = types_mod.PrimitiveType;
const TypeRegistry = types_mod.TypeRegistry;
const Ast = @import("../parser/ast.zig").Ast;
const NodeIndex = @import("../parser/ast.zig").NodeIndex;
const NodeKind = @import("../parser/ast.zig").NodeKind;
const TokenList = @import("../lexer/token.zig").TokenList;
const SourceCode = @import("../source/source.zig").SourceCode;

const mir = @import("mir.zig");
const MIRModule = mir.MIRModule;
const MIRFunction = mir.MIRFunction;
const MInst = mir.MInst;
const VReg = mir.VReg;
const Width = mir.Width;
const BinOp = mir.BinOp;
const CmpOp = mir.CmpOp;
const GlobalIndex = mir.GlobalIndex;

const llvm = @import("llvm.zig");

pub const linker = @import("linker.zig");

/// Local variable storage for a function (struct of arrays).
pub const LocalVars = struct {
    offsets: std.ArrayListUnmanaged(i16),
    widths: std.ArrayListUnmanaged(Width),
    name_map: std.StringHashMapUnmanaged(u16), // name -> index
    next_offset: i16 = -8, // first local at fp-8

    pub fn init() LocalVars {
        return .{
            .offsets = .{},
            .widths = .{},
            .name_map = .{},
            .next_offset = -8,
        };
    }

    pub fn deinit(self: *LocalVars, allocator: mem.Allocator) void {
        self.offsets.deinit(allocator);
        self.widths.deinit(allocator);
        self.name_map.deinit(allocator);
    }

    pub fn add(
        self: *LocalVars,
        allocator: mem.Allocator,
        name: []const u8,
        width: Width,
    ) !i16 {
        const idx: u16 = @intCast(self.offsets.items.len);
        const offset = self.next_offset;

        try self.offsets.append(allocator, offset);
        try self.widths.append(allocator, width);
        try self.name_map.put(allocator, name, idx);

        // Advance offset for next local (8-byte slots for simplicity)
        self.next_offset -= 8;

        return offset;
    }

    pub const LocalInfo = struct {
        offset: i16,
        width: Width,
    };

    pub fn lookup(self: *const LocalVars, name: []const u8) ?LocalInfo {
        const idx = self.name_map.get(name) orelse return null;
        return .{
            .offset = self.offsets.items[idx],
            .width = self.widths.items[idx],
        };
    }

    /// Get frame size aligned to 16 bytes.
    pub fn frameSize(self: *const LocalVars) u16 {
        if (self.offsets.items.len == 0) return 0;
        // next_offset is negative, e.g., -24 means we used 16 bytes
        const used: u16 = @intCast(-self.next_offset - 8 + 8); // bytes used
        // Round up to 16-byte alignment
        return (used + 15) & ~@as(u16, 15);
    }

    pub fn reset(self: *LocalVars) void {
        self.offsets.clearRetainingCapacity();
        self.widths.clearRetainingCapacity();
        self.name_map.clearRetainingCapacity();
        self.next_offset = -8;
    }
};

pub const Arch = enum {
    aarch64,
    x86_64,
};

pub const Os = enum {
    darwin,
    linux,
};

pub const Target = struct {
    arch: Arch,
    os: Os,

    /// Get the LLVM target triple for this target.
    pub fn getLLVMTriple(self: Target) []const u8 {
        return switch (self.os) {
            .darwin => switch (self.arch) {
                .aarch64 => "arm64-apple-darwin",
                .x86_64 => "x86_64-apple-darwin",
            },
            .linux => switch (self.arch) {
                .aarch64 => "aarch64-unknown-linux-gnu",
                .x86_64 => "x86_64-unknown-linux-gnu",
            },
        };
    }
};

pub fn generate(
    allocator: mem.Allocator,
    target: Target,
    comptime_result: *const ComptimeResult,
    symbols: *const SymbolTable,
    types: *const TypeRegistry,
    node_types: *const std.AutoHashMapUnmanaged(NodeIndex, TypeId),
    skip_nodes: *const std.AutoHashMapUnmanaged(NodeIndex, void),
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
) !CodeGenResult {
    // generate MIR (architecture-independent)
    var ctx = CodeGenContext.init(
        allocator,
        comptime_result,
        symbols,
        types,
        node_types,
        skip_nodes,
        ast,
        tokens,
        src,
    );

    try ctx.generate();

    // lower MIR to LLVM IR
    const output = try llvm.lower(allocator, &ctx.mir, types, target);

    return .{
        .output = output,
        .mir = ctx.mir,
    };
}

pub const CodeGenContext = struct {
    allocator: mem.Allocator,
    comptime_result: *const ComptimeResult,
    symbols: *const SymbolTable,
    types: *const TypeRegistry,
    node_types: *const std.AutoHashMapUnmanaged(NodeIndex, TypeId),
    skip_nodes: *const std.AutoHashMapUnmanaged(NodeIndex, void),
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    mir: MIRModule,
    current_func: ?*MIRFunction,
    locals: LocalVars,

    pub fn init(
        allocator: mem.Allocator,
        comptime_result: *const ComptimeResult,
        symbols: *const SymbolTable,
        types: *const TypeRegistry,
        node_types: *const std.AutoHashMapUnmanaged(NodeIndex, TypeId),
        skip_nodes: *const std.AutoHashMapUnmanaged(NodeIndex, void),
        ast: *const Ast,
        tokens: *const TokenList,
        src: *const SourceCode,
    ) CodeGenContext {
        return .{
            .allocator = allocator,
            .comptime_result = comptime_result,
            .symbols = symbols,
            .types = types,
            .node_types = node_types,
            .skip_nodes = skip_nodes,
            .ast = ast,
            .tokens = tokens,
            .src = src,
            .mir = MIRModule.init(allocator),
            .current_func = null,
            .locals = LocalVars.init(),
        };
    }

    pub fn deinit(self: *CodeGenContext) void {
        self.mir.deinit();
        self.locals.deinit(self.allocator);
    }

    pub fn generate(self: *CodeGenContext) !void {
        const program = self.ast.getProgram(self.ast.root);
        const declarations = self.ast.getExtra(program.declarations);

        // Phase 1: Collect global variables
        var globals_needing_init = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 16);
        defer globals_needing_init.deinit(self.allocator);

        for (declarations) |decl_idx| {
            const kind = self.ast.getKind(decl_idx);
            if (kind == .var_decl) {
                try self.collectGlobalVar(decl_idx, &globals_needing_init);
            } else if (kind == .const_decl) {
                try self.collectGlobalConst(decl_idx, &globals_needing_init);
            } else if (kind == .namespace_decl) {
                try self.collectNamespaceGlobals(decl_idx, "", &globals_needing_init);
            }
        }

        // Phase 2: Generate __honey_init (always, for runtime startup compatibility)
        try self.generateGlobalInit(globals_needing_init.items);

        // Phase 3: Generate user functions
        for (declarations) |decl_idx| {
            try self.generateDeclaration(decl_idx);
        }
    }

    fn collectGlobalVar(self: *CodeGenContext, node_idx: NodeIndex, needs_init: *std.ArrayList(NodeIndex)) !void {
        if (self.skip_nodes.contains(node_idx)) return;
        const name = self.getDeclShortName(node_idx) orelse return;
        try self.collectGlobalVarByName(node_idx, name, needs_init);
    }

    fn collectGlobalVarByName(self: *CodeGenContext, node_idx: NodeIndex, name: []const u8, needs_init: *std.ArrayList(NodeIndex)) !void {
        const var_decl = self.ast.getVarDecl(node_idx);
        const sym_idx = self.symbols.lookup(name) orelse return;
        const type_id = self.symbols.getTypeId(sym_idx);
        const width = typeIdToWidth(type_id);

        // Try to evaluate initializer at compile time
        var init_value: ?i64 = null;
        var struct_init: ?[]const i64 = null;

        if (type_id.isStruct()) {
            struct_init = self.tryEvaluateStructInit(var_decl.value, type_id);
        } else {
            init_value = self.tryEvaluateLiteral(var_decl.value);
        }

        // Register global in MIR
        const global_idx = try self.mir.globals.add(
            self.allocator,
            name,
            width,
            type_id,
            false,
            init_value,
            sym_idx,
        );

        if (struct_init) |values| {
            try self.mir.globals.struct_inits.put(self.allocator, global_idx, values);
        }

        // If needs runtime init, add to list
        if (init_value == null and struct_init == null) {
            try needs_init.append(self.allocator, node_idx);
        }
    }

    fn collectGlobalConst(self: *CodeGenContext, node_idx: NodeIndex, needs_init: *std.ArrayList(NodeIndex)) !void {
        if (self.skip_nodes.contains(node_idx)) return;
        const name = self.getDeclShortName(node_idx) orelse return;
        try self.collectGlobalConstByName(node_idx, name, needs_init);
    }

    fn collectGlobalConstByName(self: *CodeGenContext, node_idx: NodeIndex, name: []const u8, _: *std.ArrayList(NodeIndex)) !void {
        const const_decl = self.ast.getConstDecl(node_idx);
        const value_kind = self.ast.getKind(const_decl.value);

        // Only collect struct-typed constants as globals (primitives are handled by comptime)
        if (value_kind != .struct_literal) return;

        const sym_idx = self.symbols.lookup(name) orelse return;
        const type_id = self.symbols.getTypeId(sym_idx);
        const width = typeIdToWidth(type_id);

        // :: structs must have all compile-time known fields (no runtime init fallback)
        const struct_init = self.tryEvaluateStructInit(const_decl.value, type_id) orelse return;

        const global_idx = try self.mir.globals.add(
            self.allocator,
            name,
            width,
            type_id,
            true,
            null,
            sym_idx,
        );

        try self.mir.globals.struct_inits.put(self.allocator, global_idx, struct_init);
    }

    fn tryEvaluateLiteral(self: *CodeGenContext, node_idx: NodeIndex) ?i64 {
        const kind = self.ast.getKind(node_idx);
        if (kind != .literal) return null;

        const lit = self.ast.getLiteral(node_idx);
        const token = self.tokens.items[lit.token_idx];
        const value_str = self.src.getSlice(token.start, token.start + token.len);

        return switch (token.kind) {
            .bool => if (std.mem.eql(u8, value_str, "true")) 1 else 0,
            .number => std.fmt.parseInt(i64, value_str, 10) catch null,
            else => null,
        };
    }

    /// Evaluate a literal node with knowledge of the target type.
    /// For float types, parses as f64 and stores IEEE 754 bits as i64.
    fn tryEvaluateTypedLiteral(self: *CodeGenContext, node_idx: NodeIndex, target_type: TypeId) ?i64 {
        const kind = self.ast.getKind(node_idx);
        if (kind != .literal) return null;

        const lit = self.ast.getLiteral(node_idx);
        const token = self.tokens.items[lit.token_idx];
        const value_str = self.src.getSlice(token.start, token.start + token.len);

        return switch (token.kind) {
            .bool => if (std.mem.eql(u8, value_str, "true")) 1 else 0,
            .number => {
                if (target_type == .primitive) {
                    switch (target_type.primitive) {
                        .f16, .f32, .f64 => {
                            const float_val = std.fmt.parseFloat(f64, value_str) catch return null;
                            return @bitCast(float_val);
                        },
                        else => {},
                    }
                }
                return std.fmt.parseInt(i64, value_str, 10) catch null;
            },
            else => null,
        };
    }

    fn tryEvaluateStructInit(self: *CodeGenContext, value_node: NodeIndex, type_id: TypeId) ?[]const i64 {
        if (self.ast.getKind(value_node) != .struct_literal) return null;
        if (!type_id.isStruct()) return null;

        const struct_type = self.types.getStructType(type_id) orelse return null;
        const lit = self.ast.getStructLiteral(value_node);
        const field_data = self.ast.getExtra(lit.fields);

        const values = self.allocator.alloc(i64, struct_type.fields.len) catch return null;
        @memset(values, 0);

        var fi: usize = 0;
        while (fi < field_data.len) : (fi += 2) {
            const field_name_idx = field_data[fi];
            const field_value_idx = field_data[fi + 1];

            const field_ident = self.ast.getIdentifier(field_name_idx);
            const field_token = self.tokens.items[field_ident.token_idx];
            const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);

            var found = false;
            for (struct_type.fields, 0..) |field, i| {
                if (mem.eql(u8, field.name, field_name)) {
                    const val = self.tryEvaluateTypedLiteral(field_value_idx, field.type_id) orelse {
                        self.allocator.free(values);
                        return null;
                    };
                    values[i] = val;
                    found = true;
                    break;
                }
            }
            if (!found) {
                self.allocator.free(values);
                return null;
            }
        }

        return values;
    }

    /// Unwrap a pub_decl to get the inner declaration node.
    fn unwrapPubDecl(self: *CodeGenContext, node_idx: NodeIndex) NodeIndex {
        if (self.ast.getKind(node_idx) == .pub_decl) {
            const pub_decl = self.ast.getPubDecl(node_idx);
            return pub_decl.inner;
        }
        return node_idx;
    }

    /// Get the short name of a declaration node from its name identifier.
    fn getDeclShortName(self: *CodeGenContext, node_idx: NodeIndex) ?[]const u8 {
        const kind = self.ast.getKind(node_idx);
        const name_node = switch (kind) {
            .const_decl => self.ast.getConstDecl(node_idx).name,
            .func_decl => self.ast.getFuncDecl(node_idx).name,
            .var_decl => self.ast.getVarDecl(node_idx).name,
            .namespace_decl => self.ast.getNamespaceDecl(node_idx).name,
            else => return null,
        };
        const ident = self.ast.getIdentifier(name_node);
        const token = self.tokens.items[ident.token_idx];
        return self.src.getSlice(token.start, token.start + token.len);
    }

    /// Collect global vars/consts from inside a namespace declaration.
    fn collectNamespaceGlobals(self: *CodeGenContext, node_idx: NodeIndex, prefix: []const u8, needs_init: *std.ArrayList(NodeIndex)) !void {
        const ns_decl = self.ast.getNamespaceDecl(node_idx);
        const ns_name = self.getDeclShortName(node_idx) orelse return;

        const qualified_prefix = if (prefix.len > 0)
            try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ prefix, ns_name })
        else
            ns_name;

        const members = self.ast.getExtra(ns_decl.declarations);
        for (members) |member_idx| {
            const inner_idx = self.unwrapPubDecl(member_idx);
            if (self.skip_nodes.contains(inner_idx)) continue;
            const kind = self.ast.getKind(inner_idx);
            if (kind == .var_decl) {
                const short_name = self.getDeclShortName(inner_idx) orelse continue;
                const qualified_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ qualified_prefix, short_name });
                try self.collectGlobalVarByName(inner_idx, qualified_name, needs_init);
            } else if (kind == .const_decl) {
                const short_name = self.getDeclShortName(inner_idx) orelse continue;
                const qualified_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ qualified_prefix, short_name });
                try self.collectGlobalConstByName(inner_idx, qualified_name, needs_init);
            } else if (kind == .namespace_decl) {
                try self.collectNamespaceGlobals(inner_idx, qualified_prefix, needs_init);
            }
        }
    }

    /// Generate all declarations inside a namespace (functions, nested namespaces).
    fn generateNamespaceDecl(self: *CodeGenContext, node_idx: NodeIndex, prefix: []const u8) !void {
        const ns_decl = self.ast.getNamespaceDecl(node_idx);
        const ns_name = self.getDeclShortName(node_idx) orelse return;

        const qualified_prefix = if (prefix.len > 0)
            try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ prefix, ns_name })
        else
            ns_name;

        const members = self.ast.getExtra(ns_decl.declarations);
        for (members) |member_idx| {
            const inner_idx = self.unwrapPubDecl(member_idx);
            if (self.skip_nodes.contains(inner_idx)) continue;
            const kind = self.ast.getKind(inner_idx);
            switch (kind) {
                .func_decl => {
                    const short_name = self.getDeclShortName(inner_idx) orelse continue;
                    const qualified_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ qualified_prefix, short_name });
                    const func_ast = self.ast.getFuncDecl(inner_idx);
                    if (func_ast.body == null) {
                        try self.collectExternFunctionByName(inner_idx, qualified_name);
                    } else {
                        try self.generateFunctionByName(inner_idx, qualified_name);
                    }
                },
                .namespace_decl => try self.generateNamespaceDecl(inner_idx, qualified_prefix),
                .const_decl, .var_decl, .struct_decl => {},
                else => {},
            }
        }
    }

    /// Build a qualified name from a field_access chain (e.g., "math.add").
    fn buildQualifiedName(self: *CodeGenContext, node_idx: NodeIndex) !?[]const u8 {
        const kind = self.ast.getKind(node_idx);
        if (kind == .identifier) {
            const ident = self.ast.getIdentifier(node_idx);
            const token = self.tokens.items[ident.token_idx];
            return self.src.getSlice(token.start, token.start + token.len);
        } else if (kind == .field_access) {
            const access = self.ast.getFieldAccess(node_idx);
            const obj_name = try self.buildQualifiedName(access.object) orelse return null;
            const field_ident = self.ast.getIdentifier(access.field);
            const field_token = self.tokens.items[field_ident.token_idx];
            const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);
            return try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ obj_name, field_name });
        }
        return null;
    }

    fn generateGlobalInit(self: *CodeGenContext, var_nodes: []const NodeIndex) !void {
        // Create __honey_init function (void, no params)
        self.current_func = try self.mir.addFunction("__honey_init", .c, null, &.{});
        try self.current_func.?.emit(.prologue);

        for (var_nodes) |node_idx| {
            const kind = self.ast.getKind(node_idx);

            // Get name and value from either var_decl or const_decl
            const name: []const u8, const value_node: NodeIndex = switch (kind) {
                .var_decl => blk: {
                    const var_decl = self.ast.getVarDecl(node_idx);
                    const ni = self.ast.getIdentifier(var_decl.name);
                    const nt = self.tokens.items[ni.token_idx];
                    break :blk .{ self.src.getSlice(nt.start, nt.start + nt.len), var_decl.value };
                },
                else => continue,
            };

            const global_idx = self.mir.globals.lookup(name) orelse continue;
            const type_id = self.mir.globals.getTypeId(global_idx);

            // Evaluate initializer expression
            const value_reg = try self.generateExpression(value_node) orelse continue;

            // Store to global — struct vs primitive
            if (type_id.isStruct()) {
                const func = self.current_func.?;
                const dst_ptr = try func.emitAddrOfGlobal(global_idx);
                try func.emitCopyStruct(dst_ptr, value_reg, type_id.struct_type);
            } else {
                const width = self.mir.globals.getWidth(global_idx);
                try self.current_func.?.emitStoreGlobal(value_reg, global_idx, width);
            }
        }

        // Return void (emit ret with null value)
        try self.current_func.?.emitRet(null, .w32);
        self.current_func = null;
    }

    fn generateDeclaration(self: *CodeGenContext, node_idx: NodeIndex) !void {
        // skip declarations marked as unused by semantic analysis
        if (self.skip_nodes.contains(node_idx)) return;

        const kind = self.ast.getKind(node_idx);

        switch (kind) {
            .func_decl => {
                const func = self.ast.getFuncDecl(node_idx);
                if (func.body == null) {
                    // extern function — collect as declaration
                    try self.collectExternFunction(node_idx);
                } else {
                    try self.generateFunction(node_idx);
                }
            },
            .const_decl, .var_decl => {
                // global constants/variables handled at data section
            },
            .struct_decl => {
                // struct declarations are type definitions, handled at LLVM level
            },
            .namespace_decl => try self.generateNamespaceDecl(node_idx, ""),
            .pub_decl => {
                const pub_decl = self.ast.getPubDecl(node_idx);
                try self.generateDeclaration(pub_decl.inner);
            },
            else => {},
        }
    }

    fn collectExternFunction(self: *CodeGenContext, node_idx: NodeIndex) !void {
        const func_name = self.getDeclShortName(node_idx) orelse return;
        try self.collectExternFunctionBody(node_idx, func_name);
    }

    fn collectExternFunctionByName(self: *CodeGenContext, node_idx: NodeIndex, func_name: []const u8) !void {
        try self.collectExternFunctionBody(node_idx, func_name);
    }

    fn collectExternFunctionBody(self: *CodeGenContext, node_idx: NodeIndex, func_name: []const u8) !void {
        const func = self.ast.getFuncDecl(node_idx);

        // resolve return type from node_types (stored by semantic analysis)
        const return_width: ?Width = resolveReturnWidth(self.node_types, func.return_type);

        // resolve parameter widths
        const param_nodes = self.ast.getExtra(func.params);
        const param_count = param_nodes.len / 2;
        var param_widths = try std.ArrayList(Width).initCapacity(self.allocator, param_count);
        defer param_widths.deinit(self.allocator);
        var param_struct_indices = try std.ArrayList(?u32).initCapacity(self.allocator, param_count);
        defer param_struct_indices.deinit(self.allocator);

        var i: usize = 0;
        while (i < param_nodes.len) : (i += 2) {
            const param_name_idx = param_nodes[i];
            const param_type = self.node_types.get(param_name_idx) orelse .unresolved;
            try param_widths.append(self.allocator, typeIdToWidth(param_type));
            try param_struct_indices.append(self.allocator, if (param_type.isStruct()) param_type.struct_type else null);
        }

        try self.mir.addExternFunction(
            func_name,
            func.call_conv,
            return_width,
            try self.allocator.dupe(Width, param_widths.items),
            try self.allocator.dupe(?u32, param_struct_indices.items),
        );
    }

    fn generateFunction(self: *CodeGenContext, node_idx: NodeIndex) !void {
        const func_name = self.getDeclShortName(node_idx) orelse return;
        try self.generateFunctionBody(node_idx, func_name);
    }

    fn generateFunctionByName(self: *CodeGenContext, node_idx: NodeIndex, func_name: []const u8) !void {
        try self.generateFunctionBody(node_idx, func_name);
    }

    fn generateFunctionBody(self: *CodeGenContext, node_idx: NodeIndex, func_name: []const u8) !void {
        const func = self.ast.getFuncDecl(node_idx);

        // calling conv should be c if main (for darwin)
        const call_conv = if (std.mem.eql(u8, func_name, "main")) .c else func.call_conv;

        // resolve return type from node_types (stored by semantic analysis)
        const ret_type = self.node_types.get(func.return_type) orelse .unresolved;
        const sret_struct_idx: ?u32 = if (ret_type.isStruct()) ret_type.struct_type else null;
        const return_width: ?Width = if (sret_struct_idx != null) null else resolveReturnWidth(self.node_types, func.return_type);

        // build parameter info
        const param_nodes = self.ast.getExtra(func.params);
        const param_count = param_nodes.len / 2;
        var param_list = try std.ArrayList(mir.ParamInfo).initCapacity(self.allocator, param_count);
        defer param_list.deinit(self.allocator);

        {
            var i: usize = 0;
            while (i < param_nodes.len) : (i += 2) {
                const param_name_idx = param_nodes[i];
                const param_ident = self.ast.getIdentifier(param_name_idx);
                const param_token = self.tokens.items[param_ident.token_idx];
                const param_name = self.src.getSlice(param_token.start, param_token.start + param_token.len);
                const param_type = self.node_types.get(param_name_idx) orelse .unresolved;
                try param_list.append(self.allocator, .{
                    .name = param_name,
                    .width = typeIdToWidth(param_type),
                    .struct_idx = if (param_type.isStruct()) param_type.struct_type else null,
                });
            }
        }

        // reset locals for new function
        self.locals.reset();

        // create MIR function
        self.current_func = try self.mir.addFunction(
            func_name,
            call_conv,
            return_width,
            try self.allocator.dupe(mir.ParamInfo, param_list.items),
        );
        self.current_func.?.sret_struct_idx = sret_struct_idx;

        // emit function prologue
        try self.current_func.?.emit(.prologue);

        // sret: register hidden first parameter as __sret local
        const arg_offset: u8 = if (sret_struct_idx != null) 1 else 0;
        if (sret_struct_idx != null) {
            const sret_local_offset = try self.locals.add(self.allocator, "__sret", .ptr);
            try self.current_func.?.emitStoreArg(0, sret_local_offset, .ptr);
        }

        // register parameters as locals and spill to stack
        const func_params = self.current_func.?.params;
        for (func_params, 0..) |param, arg_i| {
            const offset = try self.locals.add(self.allocator, param.name, param.width);
            try self.current_func.?.emitStoreArg(@intCast(arg_i + arg_offset), offset, param.width);
        }

        // generate function body
        const has_return = try self.generateBlock(func.body.?);

        if (!has_return) {
            if (sret_struct_idx != null) {
                // sret function: implicit return void
                try self.current_func.?.emitRet(null, .w32);
            } else if (return_width) |rw| {
                // non-void function: implicit return 0
                const zero = try self.current_func.?.emitMovImm(0, rw);
                try self.current_func.?.emitRet(zero, rw);
            } else {
                // void function: implicit return void
                try self.current_func.?.emitRet(null, .w32);
            }
        }

        // set frame size for locals
        self.current_func.?.frame_size = self.locals.frameSize();

        self.current_func = null;
    }

    fn generateBlock(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!bool {
        var has_return = false;

        const block = self.ast.getBlock(node_idx);
        const statements = self.ast.getExtra(block.statements);
        const deferred = self.ast.getExtra(block.deferred);

        for (statements) |stmt_idx| {
            const kind = self.ast.getKind(stmt_idx);

            if (kind == .return_stmt) {
                // Execute deferred statements BEFORE returning (LIFO order)
                try self.generateDeferredStatements(deferred);
                try self.generateReturn(stmt_idx);
                has_return = true;
            } else {
                _ = try self.generateStatement(stmt_idx);
            }
        }

        // If no explicit return, still execute deferred at block end
        if (!has_return and deferred.len > 0) {
            try self.generateDeferredStatements(deferred);
        }

        return has_return;
    }

    /// Generate deferred statements in reverse order (LIFO)
    fn generateDeferredStatements(self: *CodeGenContext, deferred: []const NodeIndex) CodeGenError!void {
        var i = deferred.len;
        while (i > 0) {
            i -= 1;
            _ = try self.generateStatement(deferred[i]);
        }
    }

    fn generateStatement(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?NodeKind {
        const kind = self.ast.getKind(node_idx);
        switch (kind) {
            .return_stmt => try self.generateReturn(node_idx),
            .var_decl => try self.generateLocalVarDecl(node_idx),
            .defer_stmt => {
                // unwrap defer and generate the inner statement
                const defer_node = self.ast.getDefer(node_idx);
                _ = try self.generateStatement(defer_node.stmt);
            },
            .assignment => try self.generateAssignment(node_idx),
            .call_expr => {
                _ = try self.generateExpression(node_idx);
            },
            .if_stmt => try self.generateIf(node_idx),
            else => return null,
        }
        return kind;
    }

    fn generateLocalVarDecl(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!void {
        // skip declarations marked as unused by semantic analysis
        if (self.skip_nodes.contains(node_idx)) return;

        const var_decl = self.ast.getVarDecl(node_idx);
        const name_ident = self.ast.getIdentifier(var_decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        // Get resolved type from sema — .unresolved here means a used-but-unresolvable
        // type (already reported as error S018). Use w32 as error recovery so the
        // compiler can still produce an executable.
        const type_id = self.node_types.get(node_idx) orelse .unresolved;
        const width = typeIdToWidth(type_id);

        // Allocate stack slot
        const offset = try self.locals.add(self.allocator, name, width);

        // Generate initializer expression
        const value_reg = try self.generateExpression(var_decl.value) orelse return;

        // Store to local
        try self.current_func.?.emitStoreLocal(value_reg, offset, width);
    }

    fn generateAssignment(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!void {
        const assign = self.ast.getAssignment(node_idx);

        // Generate the value expression (for `x += 4` this is the desugared `x + 4`)
        const value_reg = try self.generateExpression(assign.value) orelse return;

        // Check if target is a dereference (p^ = 10)
        if (self.ast.getKind(assign.target) == .deref) {
            try self.generateDerefAssignment(assign.target, value_reg);
            return;
        }

        // Check if target is a field access (p.x = 10)
        if (self.ast.getKind(assign.target) == .field_access) {
            try self.generateFieldAssignment(assign.target, value_reg);
            return;
        }

        const target_ident = self.ast.getIdentifier(assign.target);
        const target_token = self.tokens.items[target_ident.token_idx];
        const name = self.src.getSlice(target_token.start, target_token.start + target_token.len);

        // Try local first, then global
        if (self.locals.lookup(name)) |local| {
            // Check if this is a struct assignment (needs deep copy)
            const target_type_id = self.node_types.get(assign.target) orelse .unresolved;
            if (target_type_id.isStruct()) {
                const func = self.current_func.?;
                const dst_reg = try func.emitLoadLocal(local.offset, .ptr);
                try func.emitCopyStruct(dst_reg, value_reg, target_type_id.struct_type);
                return;
            }
            try self.current_func.?.emitStoreLocal(value_reg, local.offset, local.width);
        } else if (self.mir.globals.lookup(name)) |global_idx| {
            const target_type_id = self.mir.globals.getTypeId(global_idx);
            if (target_type_id.isStruct()) {
                const func = self.current_func.?;
                const dst_ptr = try func.emitAddrOfGlobal(global_idx);
                try func.emitCopyStruct(dst_ptr, value_reg, target_type_id.struct_type);
                return;
            }
            const width = self.mir.globals.getWidth(global_idx);
            try self.current_func.?.emitStoreGlobal(value_reg, global_idx, width);
        }
    }

    fn generateFieldAssignment(self: *CodeGenContext, target_node: NodeIndex, value_reg: VReg) CodeGenError!void {
        const func = self.current_func.?;
        const access = self.ast.getFieldAccess(target_node);

        // Generate the base object expression (produces a pointer)
        const base_reg = try self.generateExpression(access.object) orelse return;

        // Get the object's type
        const object_type = self.node_types.get(access.object) orelse return;
        if (object_type != .struct_type) return;

        const struct_idx = object_type.struct_type;
        const struct_type = self.types.getStructType(object_type) orelse return;

        // Get field name
        const field_ident = self.ast.getIdentifier(access.field);
        const field_token = self.tokens.items[field_ident.token_idx];
        const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);

        // Find field index and emit store
        for (struct_type.fields, 0..) |field, i| {
            if (mem.eql(u8, field.name, field_name)) {
                const field_width = typeIdToWidth(field.type_id);
                try func.emitStoreField(base_reg, value_reg, struct_idx, @intCast(i), field_width);
                return;
            }
        }
    }

    fn generateIf(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!void {
        const if_node = self.ast.getIf(node_idx);
        const func = self.current_func.?;

        const end_label = func.allocLabel();

        // Generate the if guard
        const guard_reg = try self.generateExpression(if_node.if_guard) orelse return;
        const guard_width = if (self.node_types.get(if_node.if_guard)) |tid| typeIdToWidth(tid) else Width.w8;

        const then_label = func.allocLabel();
        // Determine where to jump on false: first else-if, else block, or end
        const else_if_count = if_node.elseIfCount();
        const first_false_label = func.allocLabel();

        try func.emitBrCond(guard_reg, guard_width, then_label, first_false_label);

        // Then block
        try func.emitLabel(then_label);
        const then_has_return = try self.generateBlock(if_node.if_block);
        if (!then_has_return) {
            try func.emitBr(end_label);
        }

        // Else-if chains
        var next_false_label = first_false_label;
        for (0..else_if_count) |i| {
            try func.emitLabel(next_false_label);

            const pair = if_node.getElseIf(self.ast, i) orelse continue;
            const elif_guard_reg = try self.generateExpression(pair.guard) orelse continue;
            const elif_guard_width = if (self.node_types.get(pair.guard)) |tid| typeIdToWidth(tid) else Width.w8;

            const elif_then_label = func.allocLabel();
            next_false_label = func.allocLabel();

            try func.emitBrCond(elif_guard_reg, elif_guard_width, elif_then_label, next_false_label);

            try func.emitLabel(elif_then_label);
            const elif_has_return = try self.generateBlock(pair.block);
            if (!elif_has_return) {
                try func.emitBr(end_label);
            }
        }

        // Else block or fall-through
        try func.emitLabel(if (else_if_count > 0) next_false_label else first_false_label);
        if (if_node.else_block) |else_block| {
            const else_has_return = try self.generateBlock(else_block);
            if (!else_has_return) {
                try func.emitBr(end_label);
            }
        } else {
            try func.emitBr(end_label);
        }

        // End label — code after the if continues here
        try func.emitLabel(end_label);
    }

    fn generateReturn(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!void {
        const ret = self.ast.getReturn(node_idx);
        const func = self.current_func.?;
        const ret_width = func.return_width orelse .w32;

        if (self.ast.getKind(ret.expr) == .void_literal) {
            try func.emitRet(null, ret_width);
            return;
        }

        const result_reg = try self.generateExpression(ret.expr);

        if (func.sret_struct_idx) |struct_idx| {
            // sret: copy result into the sret pointer and return void
            if (result_reg) |src_reg| {
                const sret_local = self.locals.lookup("__sret") orelse return;
                const sret_ptr = try func.emitLoadLocal(sret_local.offset, .ptr);
                try func.emitCopyStruct(sret_ptr, src_reg, struct_idx);
            }
            try func.emitRet(null, .w32);
        } else {
            // If the expression type doesn't match the declared return type
            // (semantic error already reported), trap at runtime so the user
            // knows this code path is invalid.
            const expr_type = self.node_types.get(ret.expr) orelse .unresolved;
            const expr_width = typeIdToWidth(expr_type);
            if (expr_width != ret_width) {
                try func.emitTrap();
                try func.emitRet(null, ret_width);
            } else {
                try func.emitRet(result_reg, ret_width);
            }
        }
    }

    fn generateExpression(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?VReg {
        const kind = self.ast.getKind(node_idx);
        return switch (kind) {
            .literal => try self.generateLiteral(node_idx),
            .identifier => try self.generateIdentifier(node_idx),
            .binary_op => try self.generateBinaryOp(node_idx),
            .call_expr => try self.generateCallExpr(node_idx),
            .field_access => try self.generateFieldAccess(node_idx),
            .struct_literal => try self.generateStructLiteral(node_idx),
            .address_of => try self.generateAddressOf(node_idx),
            .deref => try self.generateDeref(node_idx),
            else => null,
        };
    }

    fn generateLiteral(self: *CodeGenContext, node_idx: NodeIndex) !?VReg {
        const func = self.current_func.?;
        const lit = self.ast.getLiteral(node_idx);
        const token = self.tokens.items[lit.token_idx];
        const value_str = self.src.getSlice(token.start, token.start + token.len);

        // Resolve width from semantic analysis; bare unresolved literals default to w32
        const width = if (self.node_types.get(node_idx)) |tid| typeIdToWidth(tid) else Width.w32;

        switch (token.kind) {
            .bool => {
                const value: i64 = if (std.mem.eql(u8, value_str, "true")) 1 else 0;
                return try func.emitMovImm(value, width);
            },
            .number => {
                if (width.isFloat()) {
                    const float_val = std.fmt.parseFloat(f64, value_str) catch 0.0;
                    const bits: i64 = @bitCast(float_val);
                    return try func.emitMovImm(bits, width);
                }
                const value = std.fmt.parseInt(i64, value_str, 10) catch 0;
                return try func.emitMovImm(value, width);
            },
            else => return null,
        }
    }

    fn generateIdentifier(self: *CodeGenContext, node_idx: NodeIndex) !?VReg {
        const ident = self.ast.getIdentifier(node_idx);
        const token = self.tokens.items[ident.token_idx];
        const name = self.src.getSlice(token.start, token.start + token.len);

        // Check locals @"type" (for function-scoped variables)
        if (self.locals.lookup(name)) |local| {
            return try self.current_func.?.emitLoadLocal(local.offset, local.width);
        }

        // Then check symbols (globals/constants)
        const sym_idx = self.symbols.lookup(name) orelse return null;
        const type_id = self.symbols.getTypeId(sym_idx);
        const kind = self.symbols.getKind(sym_idx);

        return switch (kind) {
            .constant => {
                // For struct constants, comptime returns null — use global path
                const result = try self.generateConstLoad(sym_idx, type_id);
                if (result == null and type_id.isStruct()) {
                    return self.generateVarLoad(name, type_id);
                }
                return result;
            },
            .variable => self.generateVarLoad(name, type_id),
            .function, .@"type", .namespace => error.UnsupportedFeature,
        };
    }

    fn generateVarLoad(self: *CodeGenContext, name: []const u8, type_id: TypeId) !?VReg {
        const func = self.current_func.?;
        const global_idx = self.mir.globals.lookup(name) orelse return null;
        if (type_id.isStruct()) {
            // The global IS the struct data; return a pointer to it
            return try func.emitAddrOfGlobal(global_idx);
        }
        const width = typeIdToWidth(type_id);
        return try func.emitLoadGlobal(global_idx, width);
    }

    /// Map a resolved TypeId to a MIR operand width.
    /// .unresolved reaching codegen means semantic analysis reported an error (S018)
    /// but the compiler still produces an executable. w32 is used as error recovery.
    /// Struct types are passed by pointer, so they use .ptr width.
    fn typeIdToWidth(type_id: TypeId) Width {
        return switch (type_id) {
            .primitive => |prim| switch (prim) {
                .bool, .i8, .u8 => .w8,
                .i16, .u16 => .w16,
                .i32, .u32 => .w32,
                .i64, .u64 => .w64,
                .f16 => .wf16,
                .f32 => .wf32,
                .f64 => .wf64,
                .void => .w32, // error recovery
            },
            .struct_type => .ptr,
            .pointer => .ptr,
            .unresolved => .w32,
            else => .w32,
        };
    }

    /// Resolve the return width for a function from the return type node.
    /// Returns null for void, w32/w64 for value types, w32 as fallback.
    fn resolveReturnWidth(
        node_types: *const std.AutoHashMapUnmanaged(NodeIndex, TypeId),
        return_type_node: NodeIndex,
    ) ?Width {
        const ret_type = node_types.get(return_type_node) orelse return .w32;
        if (ret_type == .primitive and ret_type.primitive == .void) return null;
        return typeIdToWidth(ret_type);
    }

    fn generateConstLoad(self: *CodeGenContext, sym_idx: SymbolIndex, type_id: TypeId) !?VReg {
        const eval_state = self.comptime_result.eval_states.items[sym_idx];
        if (eval_state != .evaluated) return null;

        const value_str = self.comptime_result.getEvalLiteral(sym_idx) orelse return null;

        return switch (type_id) {
            .primitive => |prim| self.generatePrimitiveImmediate(prim, value_str),
            else => null,
        };
    }

    fn generatePrimitiveImmediate(self: *CodeGenContext, prim: PrimitiveType, value_str: []const u8) !?VReg {
        const func = self.current_func.?;

        switch (prim) {
            .bool => {
                const val: i64 = if (std.mem.eql(u8, value_str, "true")) 1 else 0;
                return try func.emitMovImm(val, .w32);
            },
            .i8, .i16, .i32, .u8, .u16, .u32 => {
                const val = std.fmt.parseInt(i64, value_str, 10) catch 0;
                return try func.emitMovImm(val, .w32);
            },
            .i64, .u64 => {
                const val = std.fmt.parseInt(i64, value_str, 10) catch 0;
                return try func.emitMovImm(val, .w64);
            },
            .f16, .f32 => {
                const float_val = std.fmt.parseFloat(f64, value_str) catch 0.0;
                const bits: i64 = @bitCast(float_val);
                return try func.emitMovImm(bits, .wf32);
            },
            .f64 => {
                const float_val = std.fmt.parseFloat(f64, value_str) catch 0.0;
                const bits: i64 = @bitCast(float_val);
                return try func.emitMovImm(bits, .wf64);
            },
            .void => return null,
        }
    }

    fn generateBinaryOp(self: *CodeGenContext, node_idx: NodeIndex) !?VReg {
        const func = self.current_func.?;
        const binary = self.ast.getBinaryOp(node_idx);

        const left_reg = try self.generateExpression(binary.left) orelse return null;
        const right_reg = try self.generateExpression(binary.right) orelse return null;

        // Comparisons: use operand type for width/signedness, result is always bool
        if (binary.isComparison()) {
            const operand_type = self.node_types.get(binary.left) orelse .unresolved;
            const operand_width = typeIdToWidth(operand_type);
            const signed = operand_type.isSignedInteger();

            const cmp_op: CmpOp = switch (binary.op) {
                .equal => .eq,
                .not_equal => .ne,
                .less => if (signed) CmpOp.lt_s else CmpOp.lt_u,
                .greater => if (signed) CmpOp.gt_s else CmpOp.gt_u,
                .less_equal => if (signed) CmpOp.le_s else CmpOp.le_u,
                .greater_equal => if (signed) CmpOp.ge_s else CmpOp.ge_u,
                else => unreachable,
            };

            return try func.emitCmp(cmp_op, left_reg, right_reg, operand_width);
        }

        // Many-item pointer arithmetic: emit ptr_offset instruction
        const result_type = self.node_types.get(node_idx) orelse TypeId.unresolved;
        if (result_type.isPointer()) {
            if (self.types.getPointerType(result_type)) |ptr_info| {
                if (ptr_info.is_many_item) {
                    const pointee_size = types_mod.sizeOf(ptr_info.pointee, self.types);
                    const left_type = self.node_types.get(binary.left) orelse TypeId.unresolved;
                    const left_is_ptr = left_type.isPointer();

                    const ptr_reg = if (left_is_ptr) left_reg else right_reg;
                    const off_reg = if (left_is_ptr) right_reg else left_reg;
                    const is_sub = binary.op == .sub;

                    return try func.emitPtrOffset(ptr_reg, off_reg, pointee_size, is_sub);
                }
            }
        }

        const mir_op: BinOp = switch (binary.op) {
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .div => .div_s, // TODO: signed vs unsigned based on type
            else => return error.UnsupportedFeature,
        };

        // Resolve width from semantic analysis; default to w32
        const width = if (self.node_types.get(node_idx)) |tid| typeIdToWidth(tid) else Width.w32;
        return try func.emitBinOp(mir_op, left_reg, right_reg, width);
    }

    fn generateCallExpr(self: *CodeGenContext, node_idx: NodeIndex) !?VReg {
        const func = self.current_func.?;
        const call = self.ast.getCallExpr(node_idx);
        const func_node_kind = self.ast.getKind(call.func);

        // 1. Get function name (direct identifier or qualified via field_access)
        const func_name: []const u8 = if (func_node_kind == .identifier) blk: {
            const func_ident = self.ast.getIdentifier(call.func);
            const func_token = self.tokens.items[func_ident.token_idx];
            break :blk self.src.getSlice(func_token.start, func_token.start + func_token.len);
        } else if (func_node_kind == .field_access) blk: {
            break :blk try self.buildQualifiedName(call.func) orelse return null;
        } else return null;

        // 2. Look up function to get calling convention
        const sym_idx = self.symbols.lookup(func_name) orelse return null;
        const value_node = self.symbols.getValueNode(sym_idx);
        const func_decl = self.ast.getFuncDecl(value_node);
        const call_conv = func_decl.call_conv;

        // 3. Generate argument expressions
        const arg_nodes = self.ast.getExtra(call.args);
        var arg_regs = try std.ArrayList(VReg).initCapacity(self.allocator, arg_nodes.len);
        defer arg_regs.deinit(self.allocator);
        var arg_widths = try std.ArrayList(Width).initCapacity(self.allocator, arg_nodes.len);
        defer arg_widths.deinit(self.allocator);
        var arg_struct_indices = try std.ArrayList(?u32).initCapacity(self.allocator, arg_nodes.len);
        defer arg_struct_indices.deinit(self.allocator);

        for (arg_nodes) |arg_idx| {
            const arg_type = self.node_types.get(arg_idx) orelse .unresolved;
            const arg_width = typeIdToWidth(arg_type);
            const arg_reg = try self.generateExpression(arg_idx) orelse {
                // If arg generation fails, use 0 as placeholder
                const zero = try func.emitMovImm(0, .w32);
                try arg_regs.append(self.allocator, zero);
                try arg_widths.append(self.allocator, .w32);
                try arg_struct_indices.append(self.allocator, null);
                continue;
            };
            try arg_regs.append(self.allocator, arg_reg);
            try arg_widths.append(self.allocator, arg_width);
            try arg_struct_indices.append(self.allocator, if (arg_type.isStruct()) arg_type.struct_type else null);
        }

        // 4. Determine return type and check for sret
        const return_type = self.node_types.get(node_idx);
        const callee_returns_struct = if (return_type) |rt| rt.isStruct() else false;

        if (callee_returns_struct) {
            // sret: allocate local struct, pass as hidden first argument
            const struct_idx = return_type.?.struct_type;
            const result_ptr = try func.emitAllocaStruct(struct_idx);

            // prepend sret pointer to arguments
            var sret_args = try std.ArrayList(VReg).initCapacity(self.allocator, arg_regs.items.len + 1);
            defer sret_args.deinit(self.allocator);
            try sret_args.append(self.allocator, result_ptr);
            for (arg_regs.items) |a| try sret_args.append(self.allocator, a);

            var sret_widths = try std.ArrayList(Width).initCapacity(self.allocator, arg_widths.items.len + 1);
            defer sret_widths.deinit(self.allocator);
            try sret_widths.append(self.allocator, .ptr);
            for (arg_widths.items) |w| try sret_widths.append(self.allocator, w);

            var sret_struct_indices = try std.ArrayList(?u32).initCapacity(self.allocator, arg_struct_indices.items.len + 1);
            defer sret_struct_indices.deinit(self.allocator);
            try sret_struct_indices.append(self.allocator, null); // sret pointer itself is not byval
            for (arg_struct_indices.items) |s| try sret_struct_indices.append(self.allocator, s);

            // call returns void; the struct is written via sret pointer
            _ = try func.emitCall(func_name, sret_args.items, sret_widths.items, sret_struct_indices.items, call_conv, null, struct_idx);

            return result_ptr;
        }

        // 5. Non-sret call
        const return_width: ?Width = if (return_type) |rt| blk: {
            if (rt == .unresolved) break :blk .w32;
            if (rt == .primitive and rt.primitive == .void) break :blk null;
            break :blk typeIdToWidth(rt);
        } else .w32;

        return try func.emitCall(func_name, arg_regs.items, arg_widths.items, arg_struct_indices.items, call_conv, return_width, null);
    }

    fn generateFieldAccess(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?VReg {
        const func = self.current_func.?;
        const access = self.ast.getFieldAccess(node_idx);

        // get the object's type from semantic analysis
        const object_type = self.node_types.get(access.object) orelse return null;

        // namespace field access: resolve to the member symbol
        if (object_type.isNamespace()) {
            const qualified_name = try self.buildQualifiedName(node_idx) orelse return null;
            const sym_idx = self.symbols.lookup(qualified_name) orelse return null;
            const type_id = self.symbols.getTypeId(sym_idx);
            const kind = self.symbols.getKind(sym_idx);

            return switch (kind) {
                .constant => {
                    const result = try self.generateConstLoad(sym_idx, type_id);
                    if (result == null and type_id.isStruct()) {
                        return self.generateVarLoad(qualified_name, type_id);
                    }
                    return result;
                },
                .variable => self.generateVarLoad(qualified_name, type_id),
                .namespace => null, // nested namespace has no runtime value
                .function, .@"type" => null,
            };
        }

        if (object_type != .struct_type) return null;

        // generate the base object expression (produces a pointer for struct types)
        const base_reg = try self.generateExpression(access.object) orelse return null;

        const struct_idx = object_type.struct_type;
        const struct_type = self.types.getStructType(object_type) orelse return null;

        // get field name
        const field_ident = self.ast.getIdentifier(access.field);
        const field_token = self.tokens.items[field_ident.token_idx];
        const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);

        // find field index
        for (struct_type.fields, 0..) |field, i| {
            if (mem.eql(u8, field.name, field_name)) {
                const field_width = typeIdToWidth(field.type_id);
                return try func.emitLoadField(base_reg, struct_idx, @intCast(i), field_width);
            }
        }

        return null;
    }

    fn generateAddressOf(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?VReg {
        const addr = self.ast.getAddressOf(node_idx);
        const operand_kind = self.ast.getKind(addr.operand);

        if (operand_kind == .identifier) {
            const ident = self.ast.getIdentifier(addr.operand);
            const token = self.tokens.items[ident.token_idx];
            const name = self.src.getSlice(token.start, token.start + token.len);

            // get the operand's type to determine how to take its address
            const operand_type = self.node_types.get(addr.operand) orelse .unresolved;

            if (operand_type.isStruct()) {
                // struct variables already store a pointer to the data
                // &pt just returns that pointer
                return try self.generateIdentifier(addr.operand);
            }

            // primitive variable — get address of stack slot or global
            if (self.locals.lookup(name)) |local| {
                return try self.current_func.?.emitAddrOfLocal(local.offset);
            } else if (self.mir.globals.lookup(name)) |global_idx| {
                return try self.current_func.?.emitAddrOfGlobal(global_idx);
            }
        } else if (operand_kind == .field_access) {
            // &obj.field — GEP to the field without loading
            const access = self.ast.getFieldAccess(addr.operand);
            const base_reg = try self.generateExpression(access.object) orelse return null;
            const object_type = self.node_types.get(access.object) orelse return null;
            if (object_type != .struct_type) return null;

            const struct_idx = object_type.struct_type;
            const struct_type = self.types.getStructType(object_type) orelse return null;

            const field_ident = self.ast.getIdentifier(access.field);
            const field_token = self.tokens.items[field_ident.token_idx];
            const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);

            for (struct_type.fields, 0..) |field, i| {
                if (mem.eql(u8, field.name, field_name)) {
                    return try self.current_func.?.emitAddrOfField(base_reg, struct_idx, @intCast(i));
                }
            }
        }

        return null;
    }

    fn generateDeref(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?VReg {
        const deref = self.ast.getDeref(node_idx);

        // generate the pointer expression
        const ptr_reg = try self.generateExpression(deref.operand) orelse return null;

        // get the result type (pointee type)
        const result_type = self.node_types.get(node_idx) orelse .unresolved;

        if (result_type.isStruct()) {
            // struct: the pointer IS the value (structs are always by-pointer)
            return ptr_reg;
        }

        // primitive: load through pointer
        const width = typeIdToWidth(result_type);
        return try self.current_func.?.emitLoadPtr(ptr_reg, width);
    }

    fn generateDerefAssignment(self: *CodeGenContext, target_node: NodeIndex, value_reg: VReg) CodeGenError!void {
        const func = self.current_func.?;
        const deref = self.ast.getDeref(target_node);

        // generate the pointer expression
        const ptr_reg = try self.generateExpression(deref.operand) orelse return;

        // get the target type (pointee type)
        const target_type = self.node_types.get(target_node) orelse .unresolved;

        if (target_type.isStruct()) {
            // struct: memcpy the data
            try func.emitCopyStruct(ptr_reg, value_reg, target_type.struct_type);
        } else {
            // primitive: store through pointer
            const width = typeIdToWidth(target_type);
            try func.emitStorePtr(ptr_reg, value_reg, width);
        }
    }

    fn generateStructLiteral(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?VReg {
        const func = self.current_func.?;
        const lit = self.ast.getStructLiteral(node_idx);

        // get struct type from semantic analysis
        const type_id = self.node_types.get(node_idx) orelse return null;
        if (type_id != .struct_type) return null;

        const struct_idx = type_id.struct_type;
        const struct_type = self.types.getStructType(type_id) orelse return null;

        // allocate struct on stack
        const base = try func.emitAllocaStruct(struct_idx);

        // store each field value
        const field_data = self.ast.getExtra(lit.fields);
        var fi: usize = 0;
        while (fi < field_data.len) : (fi += 2) {
            const field_name_idx = field_data[fi];
            const field_value_idx = field_data[fi + 1];

            // get field name
            const field_ident = self.ast.getIdentifier(field_name_idx);
            const field_token = self.tokens.items[field_ident.token_idx];
            const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);

            // find field index in struct definition
            for (struct_type.fields, 0..) |field, i| {
                if (mem.eql(u8, field.name, field_name)) {
                    const value_reg = try self.generateExpression(field_value_idx) orelse continue;
                    const field_width = typeIdToWidth(field.type_id);
                    try func.emitStoreField(base, value_reg, struct_idx, @intCast(i), field_width);
                    break;
                }
            }
        }

        // return pointer to the struct
        return base;
    }
};

pub const CodeGenError = error{
    OutOfMemory,
    UnsupportedFeature,
};

pub const CodeGenResult = struct {
    output: []const u8, // LLVM IR text
    mir: MIRModule,
};
