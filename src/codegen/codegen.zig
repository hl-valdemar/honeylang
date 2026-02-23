const std = @import("std");
const mem = std.mem;

const tupleFieldName = @import("../utils/tuple.zig").fieldName;
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
const SourceIndex = @import("../source/source.zig").SourceIndex;

const mir = @import("mir.zig");
const MIRModule = mir.MIRModule;
const MIRFunction = mir.MIRFunction;
const MInst = mir.MInst;
const VReg = mir.VReg;
const Width = mir.Width;
const BinOp = mir.BinOp;
const CmpOp = mir.CmpOp;
const LabelId = mir.LabelId;
const GlobalIndex = mir.GlobalIndex;

const llvm = @import("llvm.zig");
const ResolvedImports = @import("../imports/imports.zig").ResolvedImports;

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
    arm,
    x86,
};

pub const Os = enum {
    macos,
    linux,
};

pub const Target = struct {
    arch: Arch,
    os: Os,

    /// Get the LLVM target triple for LLVM IR emission.
    pub fn getLLVMTriple(self: Target) []const u8 {
        return switch (self.os) {
            .macos => switch (self.arch) {
                .aarch64 => "arm64-apple-darwin",
                .x86_64 => "x86_64-apple-darwin",
                .arm, .x86 => unreachable, // 32-bit Darwin not supported
            },
            .linux => switch (self.arch) {
                .aarch64 => "aarch64-unknown-linux-gnu",
                .x86_64 => "x86_64-unknown-linux-gnu",
                .arm => "armv7-unknown-linux-gnueabihf",
                .x86 => "i386-unknown-linux-gnu",
            },
        };
    }

    /// Get the target triple for zig cc (differs from LLVM triple for some targets).
    pub fn getZigTriple(self: Target) []const u8 {
        return switch (self.os) {
            .macos => switch (self.arch) {
                .aarch64 => "aarch64-macos",
                .x86_64 => "x86_64-macos",
                .arm, .x86 => unreachable,
            },
            .linux => switch (self.arch) {
                .aarch64 => "aarch64-linux-gnu",
                .x86_64 => "x86_64-linux-gnu",
                .arm => "arm-linux-gnueabihf",
                .x86 => "x86-linux-gnu",
            },
        };
    }

    pub fn ptrSize(self: Target) u32 {
        return switch (self.arch) {
            .aarch64, .x86_64 => 8,
            .arm, .x86 => 4,
        };
    }

    pub fn ptrAlign(self: Target) u32 {
        return self.ptrSize();
    }

    pub fn ptrLLVMType(self: Target) []const u8 {
        return switch (self.arch) {
            .aarch64, .x86_64 => "i64",
            .arm, .x86 => "i32",
        };
    }

    pub fn ptrWidth(self: Target) Width {
        return switch (self.arch) {
            .aarch64, .x86_64 => .w64,
            .arm, .x86 => .w32,
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
    import_node_types: *const std.AutoHashMapUnmanaged(usize, std.AutoHashMapUnmanaged(NodeIndex, TypeId)),
    skip_nodes: *const std.AutoHashMapUnmanaged(NodeIndex, void),
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    resolved_imports: ?*const ResolvedImports,
    lexer_error_positions: []const SourceIndex,
) !CodeGenResult {
    // generate MIR (architecture-independent)
    var ctx = CodeGenContext.init(
        allocator,
        target,
        comptime_result,
        symbols,
        types,
        node_types,
        import_node_types,
        skip_nodes,
        ast,
        tokens,
        src,
        resolved_imports,
        lexer_error_positions,
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
    target: Target,
    comptime_result: *const ComptimeResult,
    symbols: *const SymbolTable,
    types: *const TypeRegistry,
    node_types: *const std.AutoHashMapUnmanaged(NodeIndex, TypeId),
    import_node_types: *const std.AutoHashMapUnmanaged(usize, std.AutoHashMapUnmanaged(NodeIndex, TypeId)),
    skip_nodes: *const std.AutoHashMapUnmanaged(NodeIndex, void),
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    resolved_imports: ?*const ResolvedImports,
    current_import_map: ?*const std.AutoHashMapUnmanaged(NodeIndex, usize) = null,
    mir: MIRModule,
    current_func: ?*MIRFunction,
    locals: LocalVars,
    next_string_id: u32 = 0,
    loop_end_label: ?LabelId = null,
    loop_cont_label: ?LabelId = null,
    lexer_error_positions: []const SourceIndex,

    pub fn init(
        allocator: mem.Allocator,
        target: Target,
        comptime_result: *const ComptimeResult,
        symbols: *const SymbolTable,
        types: *const TypeRegistry,
        node_types: *const std.AutoHashMapUnmanaged(NodeIndex, TypeId),
        import_node_types: *const std.AutoHashMapUnmanaged(usize, std.AutoHashMapUnmanaged(NodeIndex, TypeId)),
        skip_nodes: *const std.AutoHashMapUnmanaged(NodeIndex, void),
        ast: *const Ast,
        tokens: *const TokenList,
        src: *const SourceCode,
        resolved_imports: ?*const ResolvedImports,
        lexer_error_positions: []const SourceIndex,
    ) CodeGenContext {
        return .{
            .allocator = allocator,
            .target = target,
            .comptime_result = comptime_result,
            .symbols = symbols,
            .types = types,
            .node_types = node_types,
            .import_node_types = import_node_types,
            .skip_nodes = skip_nodes,
            .ast = ast,
            .tokens = tokens,
            .src = src,
            .resolved_imports = resolved_imports,
            .current_import_map = if (resolved_imports) |ri| &ri.map else null,
            .mir = MIRModule.init(allocator),
            .current_func = null,
            .locals = LocalVars.init(),
            .lexer_error_positions = lexer_error_positions,
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
            } else if (kind == .import_decl or kind == .c_include_decl or kind == .c_import_block) {
                try self.collectImportGlobals(decl_idx, &globals_needing_init);
            }
        }

        // Phase 2: Generate __honey_init (always, for runtime startup compatibility)
        try self.generateGlobalInit(globals_needing_init.items, declarations);

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
        const width = typeIdToWidth(type_id, self.target);

        // Try to evaluate initializer at compile time
        var init_value: ?i64 = null;
        var struct_init: ?[]const i64 = null;
        var array_init: ?[]const i64 = null;

        if (type_id.isSlice()) {
            // Slices always need runtime init (ptr + len from another global)
            const global_idx = try self.mir.globals.add(
                self.allocator,
                name,
                width,
                type_id,
                false,
                null,
                sym_idx,
            );
            _ = global_idx;
            try needs_init.append(self.allocator, node_idx);
            return;
        } else if (type_id.isStruct()) {
            struct_init = self.tryEvaluateStructInit(var_decl.value, type_id);
        } else if (type_id.isArray()) {
            array_init = self.tryEvaluateArrayInit(var_decl.value, type_id);
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
        if (array_init) |values| {
            try self.mir.globals.array_inits.put(self.allocator, global_idx, values);
        }

        // If needs runtime init, add to list
        if (init_value == null and struct_init == null and array_init == null) {
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

        // Only collect struct/array constants as globals (primitives are handled by comptime)
        if (value_kind != .struct_literal and value_kind != .array_literal) return;

        const sym_idx = self.symbols.lookup(name) orelse return;
        const type_id = self.symbols.getTypeId(sym_idx);
        const width = typeIdToWidth(type_id, self.target);

        if (value_kind == .array_literal) {
            const array_init = self.tryEvaluateArrayInit(const_decl.value, type_id) orelse return;

            const global_idx = try self.mir.globals.add(
                self.allocator,
                name,
                width,
                type_id,
                true,
                null,
                sym_idx,
            );

            try self.mir.globals.array_inits.put(self.allocator, global_idx, array_init);
            return;
        }

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
            .number => std.fmt.parseInt(i64, value_str, 0) catch null,
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
                return std.fmt.parseInt(i64, value_str, 0) catch null;
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

    fn tryEvaluateArrayInit(self: *CodeGenContext, value_node: NodeIndex, type_id: TypeId) ?[]const i64 {
        if (self.ast.getKind(value_node) != .array_literal) return null;
        if (!type_id.isArray()) return null;

        const arr_info = self.types.getArrayType(type_id) orelse return null;
        const lit = self.ast.getArrayLiteral(value_node);
        const elements = self.ast.getExtra(lit.elements);

        if (elements.len != arr_info.length) return null;

        const values = self.allocator.alloc(i64, arr_info.length) catch return null;
        for (elements, 0..) |elem_idx, i| {
            const val = self.tryEvaluateTypedLiteral(elem_idx, arr_info.element_type) orelse {
                self.allocator.free(values);
                return null;
            };
            values[i] = val;
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

    /// Collect global vars/consts from an imported file.
    fn collectImportGlobals(self: *CodeGenContext, node_idx: NodeIndex, needs_init: *std.ArrayList(NodeIndex)) !void {
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

        if (self.import_node_types.getPtr(import_idx)) |import_nt| {
            self.node_types = import_nt;
        }

        defer {
            self.ast = saved_ast;
            self.tokens = saved_tokens;
            self.src = saved_src;
            self.node_types = saved_node_types;
            self.current_import_map = saved_import_map;
        }

        const ns_name = resolved.namespace_name;
        const program = self.ast.getProgram(self.ast.root);
        const members = self.ast.getExtra(program.declarations);

        for (members) |member_idx| {
            const inner_idx = self.unwrapPubDecl(member_idx);
            if (self.skip_nodes.contains(inner_idx)) continue;
            const kind = self.ast.getKind(inner_idx);
            if (kind == .var_decl) {
                const short_name = self.getDeclShortName(inner_idx) orelse continue;
                const qualified_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ ns_name, short_name });
                try self.collectGlobalVarByName(inner_idx, qualified_name, needs_init);
            } else if (kind == .const_decl) {
                const short_name = self.getDeclShortName(inner_idx) orelse continue;
                const qualified_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ ns_name, short_name });
                try self.collectGlobalConstByName(inner_idx, qualified_name, needs_init);
            } else if (kind == .namespace_decl) {
                try self.collectNamespaceGlobals(inner_idx, ns_name, needs_init);
            } else if (kind == .import_decl or kind == .c_include_decl or kind == .c_import_block) {
                try self.collectImportGlobals(inner_idx, needs_init);
            }
        }
    }

    /// Generate functions from an imported file.
    fn generateImportDecl(self: *CodeGenContext, node_idx: NodeIndex) !void {
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

        // Switch to the import's node_types map (populated during semantic analysis)
        if (self.import_node_types.getPtr(import_idx)) |import_nt| {
            self.node_types = import_nt;
        }

        defer {
            self.ast = saved_ast;
            self.tokens = saved_tokens;
            self.src = saved_src;
            self.node_types = saved_node_types;
            self.current_import_map = saved_import_map;
        }

        const ns_name = resolved.namespace_name;
        const program = self.ast.getProgram(self.ast.root);
        const members = self.ast.getExtra(program.declarations);

        for (members) |member_idx| {
            const inner_idx = self.unwrapPubDecl(member_idx);
            if (self.skip_nodes.contains(inner_idx)) continue;
            const kind = self.ast.getKind(inner_idx);
            switch (kind) {
                .func_decl => {
                    const short_name = self.getDeclShortName(inner_idx) orelse continue;
                    const qualified_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ ns_name, short_name });
                    const func_ast = self.ast.getFuncDecl(inner_idx);
                    if (func_ast.body == null) {
                        try self.collectExternFunctionByName(inner_idx, short_name);
                    } else {
                        try self.generateFunctionByName(inner_idx, qualified_name);
                    }
                },
                .namespace_decl => try self.generateNamespaceDecl(inner_idx, ns_name),
                .import_decl, .c_include_decl, .c_import_block => try self.generateImportDecl(inner_idx),
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

    fn generateGlobalInit(self: *CodeGenContext, var_nodes: []const NodeIndex, declarations: []const NodeIndex) !void {
        // Create __honey_init function (void, no params)
        self.current_func = try self.mir.addFunction("__honey_init", .c, null, &.{});
        try self.current_func.?.emit(.prologue);

        // Trap if any top-level declaration has a parse error
        for (declarations) |decl_idx| {
            if (self.ast.getKind(decl_idx) == .err) {
                try self.current_func.?.emitTrap();
                break;
            }
        }

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

            // Slice globals: evaluate the initializer (e.g. arr[..]) and copy fat pointer
            if (type_id.isSlice()) {
                const func = self.current_func.?;
                const value_reg = try self.generateExpression(value_node) orelse continue;
                const global_ptr = try func.emitAddrOfGlobal(global_idx);
                const src_ptr = try func.emitSliceGetPtr(value_reg);
                const src_len = try func.emitSliceGetLen(value_reg);
                try func.emitMakeSlice(global_ptr, src_ptr, src_len);
                continue;
            }

            // Array globals: store elements directly into global via GEP
            if (type_id.isArray()) {
                const array_idx = type_id.array;
                const arr_info = self.types.getArrayType(type_id) orelse continue;
                const elem_width = typeIdToWidth(arr_info.element_type, self.target);
                const func = self.current_func.?;
                const global_ptr = try func.emitAddrOfGlobal(global_idx);

                if (self.ast.getKind(value_node) == .array_literal) {
                    const lit = self.ast.getArrayLiteral(value_node);
                    const elements = self.ast.getExtra(lit.elements);
                    for (elements, 0..) |elem_idx, i| {
                        const val_reg = try self.generateExpression(elem_idx) orelse continue;
                        const idx_reg = try func.emitMovImm(@intCast(i), self.target.ptrWidth());
                        try func.emitStoreElement(global_ptr, idx_reg, val_reg, array_idx, elem_width);
                    }
                }
                continue;
            }

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
            .struct_decl, .opaque_decl => {
                // type declarations — no codegen needed
            },
            .namespace_decl => try self.generateNamespaceDecl(node_idx, ""),
            .import_decl, .c_include_decl, .c_import_block => try self.generateImportDecl(node_idx),
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
        const return_width: ?Width = resolveReturnWidth(self.node_types, func.return_type, self.target);

        // resolve parameter widths
        const param_nodes = self.ast.getExtra(func.params);
        const param_count = param_nodes.len / 2;
        var param_widths = try std.ArrayList(Width).initCapacity(self.allocator, param_count);
        defer param_widths.deinit(self.allocator);
        var param_struct_indices = try std.ArrayList(?u32).initCapacity(self.allocator, param_count);
        defer param_struct_indices.deinit(self.allocator);
        var param_slice_flags = try std.ArrayList(bool).initCapacity(self.allocator, param_count);
        defer param_slice_flags.deinit(self.allocator);

        var i: usize = 0;
        while (i < param_nodes.len) : (i += 2) {
            const param_name_idx = param_nodes[i];
            const param_type = self.node_types.get(param_name_idx) orelse .unresolved;
            try param_widths.append(self.allocator, typeIdToWidth(param_type, self.target));
            try param_struct_indices.append(self.allocator, if (param_type.isStruct()) param_type.struct_type else null);
            // sentinel-terminated slices in C functions are passed as raw pointers
            const is_sentinel = if (self.types.getSliceType(param_type)) |si| si.sentinel != null else false;
            try param_slice_flags.append(self.allocator, param_type.isSlice() and !is_sentinel);
        }

        try self.mir.addExternFunction(
            func_name,
            func.call_conv,
            return_width,
            try self.allocator.dupe(Width, param_widths.items),
            try self.allocator.dupe(?u32, param_struct_indices.items),
            try self.allocator.dupe(bool, param_slice_flags.items),
            func.is_variadic,
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

        // calling conv should be c if main (for macOS)
        const call_conv = if (std.mem.eql(u8, func_name, "main")) .c else func.call_conv;

        // resolve return type from node_types (stored by semantic analysis)
        const ret_type = self.node_types.get(func.return_type) orelse .unresolved;
        const sret_struct_idx: ?u32 = if (ret_type.isStruct()) ret_type.struct_type else null;
        const return_width: ?Width = if (sret_struct_idx != null) null else resolveReturnWidth(self.node_types, func.return_type, self.target);

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
                // sentinel-terminated slices in C-convention functions are raw pointers
                const is_sentinel = if (self.types.getSliceType(param_type)) |si| si.sentinel != null else false;
                try param_list.append(self.allocator, .{
                    .name = param_name,
                    .width = typeIdToWidth(param_type, self.target),
                    .struct_idx = if (param_type.isStruct()) param_type.struct_type else null,
                    .is_slice = param_type.isSlice() and !(is_sentinel and call_conv == .c),
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
            .while_stmt => try self.generateWhile(node_idx),
            .break_stmt => {
                if (self.loop_end_label) |end_label| {
                    try self.current_func.?.emitBr(end_label);
                }
            },
            .continue_stmt => {
                if (self.loop_cont_label) |cont_label| {
                    try self.current_func.?.emitBr(cont_label);
                }
            },
            .block => {
                _ = try self.generateBlock(node_idx);
            },
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

        // Array locals: the local IS the [N x T] alloca, no pointer wrapper
        if (type_id.isArray()) {
            const array_idx = type_id.array;
            const arr_info = self.types.getArrayType(type_id) orelse return;

            const offset = try self.locals.add(self.allocator, name, .ptr);
            const func = self.current_func.?;
            try func.emitAllocaArrayLocal(offset, array_idx);

            // Store elements directly from array literal
            if (self.ast.getKind(var_decl.value) == .array_literal) {
                const base = try func.emitAddrOfLocal(offset);
                const lit = self.ast.getArrayLiteral(var_decl.value);
                const elements = self.ast.getExtra(lit.elements);
                const is_struct_elem = arr_info.element_type.isStruct();

                for (elements, 0..) |elem_idx, i| {
                    const value_reg = try self.generateExpression(elem_idx) orelse continue;
                    const index_reg = try func.emitMovImm(@intCast(i), self.target.ptrWidth());

                    if (is_struct_elem) {
                        const slot_ptr = try func.emitAddrOfElement(base, index_reg, array_idx);
                        try func.emitCopyStruct(slot_ptr, value_reg, arr_info.element_type.struct_type);
                    } else {
                        const elem_width = typeIdToWidth(arr_info.element_type, self.target);
                        try func.emitStoreElement(base, index_reg, value_reg, array_idx, elem_width);
                    }
                }
            }
            return;
        }

        // Slice locals: allocate { ptr, usize } storage, then store a pointer to it
        // in the named local — matching the parameter convention (local holds ptr to fat ptr)
        if (type_id.isSlice()) {
            const func = self.current_func.?;

            // Anonymous alloca for the actual { ptr, usize } storage
            const storage_offset = try self.locals.add(self.allocator, "__slice_var", .ptr);
            try func.emitAllocaSliceLocal(storage_offset);
            const storage_ptr = try func.emitAddrOfLocal(storage_offset);

            // Generate initializer (e.g. arr[..], arr[1..3]) — returns ptr to a {ptr, len}
            const value_reg = try self.generateExpression(var_decl.value) orelse {
                try func.emitTrap();
                return;
            };

            // Copy the fat pointer from source into our storage
            const src_ptr = try func.emitSliceGetPtr(value_reg);
            const src_len = try func.emitSliceGetLen(value_reg);
            try func.emitMakeSlice(storage_ptr, src_ptr, src_len);

            // Named local stores a pointer to the fat pointer (same as params)
            const offset = try self.locals.add(self.allocator, name, .ptr);
            try func.emitStoreLocal(storage_ptr, offset, .ptr);
            return;
        }

        const width = typeIdToWidth(type_id, self.target);

        // Allocate stack slot
        const offset = try self.locals.add(self.allocator, name, width);

        // Generate initializer expression
        const value_reg = try self.generateExpression(var_decl.value) orelse {
            try self.current_func.?.emitTrap();
            return;
        };

        // Store to local
        try self.current_func.?.emitStoreLocal(value_reg, offset, width);
    }

    fn generateAssignment(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!void {
        // Trap on erroneous assignments (e.g. immutable target, immutable element)
        if (self.node_types.get(node_idx)) |tid| {
            if (tid.isUnresolved()) {
                try self.current_func.?.emitTrap();
                return;
            }
        }

        const assign = self.ast.getAssignment(node_idx);

        // Generate the value expression (for `x += 4` this is the desugared `x + 4`)
        const value_reg = try self.generateExpression(assign.value) orelse {
            try self.current_func.?.emitTrap();
            return;
        };

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

        // Check if target is an array index (arr[i] = 10)
        if (self.ast.getKind(assign.target) == .array_index) {
            try self.generateArrayIndexAssignment(assign.target, value_reg);
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

        // Get the object's type, auto-deref pointer layers
        var object_type = self.node_types.get(access.object) orelse return;
        while (object_type.isPointer()) {
            const ptr_info = self.types.getPointerType(object_type) orelse break;
            object_type = ptr_info.pointee;
        }
        if (object_type != .struct_type) return;

        const struct_idx = object_type.struct_type;
        const struct_type = self.types.getStructType(object_type) orelse return;

        // Get field name — identifier for named fields, literal for tuple .0 .1
        const field_name = self.getFieldNameFromNode(access.field);

        // Find field index and emit store
        for (struct_type.fields, 0..) |field, i| {
            if (mem.eql(u8, field.name, field_name)) {
                const field_width = typeIdToWidth(field.type_id, self.target);
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
        const guard_width = if (self.node_types.get(if_node.if_guard)) |tid| typeIdToWidth(tid, self.target) else Width.w8;

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
            const elif_guard_width = if (self.node_types.get(pair.guard)) |tid| typeIdToWidth(tid, self.target) else Width.w8;

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

    fn generateWhile(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!void {
        const while_node = self.ast.getWhile(node_idx);
        const func = self.current_func.?;

        const loop_label = func.allocLabel();
        const body_label = func.allocLabel();
        const cont_label = func.allocLabel();
        const end_label = func.allocLabel();

        // Save outer loop context and set inner
        const saved_end = self.loop_end_label;
        const saved_cont = self.loop_cont_label;
        self.loop_end_label = end_label;
        self.loop_cont_label = cont_label;

        // Jump to condition check
        try func.emitBr(loop_label);

        // Loop header: evaluate condition
        try func.emitLabel(loop_label);
        const cond_reg = try self.generateExpression(while_node.condition) orelse {
            // Restore and bail if condition can't be generated
            self.loop_end_label = saved_end;
            self.loop_cont_label = saved_cont;
            return;
        };
        const cond_width = if (self.node_types.get(while_node.condition)) |tid| typeIdToWidth(tid, self.target) else Width.w8;
        try func.emitBrCond(cond_reg, cond_width, body_label, end_label);

        // Body
        try func.emitLabel(body_label);
        _ = try self.generateBlock(while_node.body);
        try func.emitBr(cont_label);

        // Continue expression block (runs on fallthrough and on continue)
        try func.emitLabel(cont_label);
        if (while_node.cont_expr) |cont_expr| {
            _ = try self.generateStatement(cont_expr);
        }
        try func.emitBr(loop_label);

        // End
        try func.emitLabel(end_label);

        // Restore outer loop context
        self.loop_end_label = saved_end;
        self.loop_cont_label = saved_cont;
    }

    fn generateReturn(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!void {
        const ret = self.ast.getReturn(node_idx);
        const func = self.current_func.?;
        const ret_width = func.return_width orelse .w32;

        if (self.ast.getKind(ret.expr) == .void_literal) {
            try func.emitRet(null, ret_width);
            return;
        }

        const result_reg = try self.generateExpression(ret.expr) orelse {
            try func.emitTrap();
            try func.emitRet(null, ret_width);
            return;
        };

        if (func.sret_struct_idx) |struct_idx| {
            // sret: copy result into the sret pointer and return void
            const sret_local = self.locals.lookup("__sret") orelse return;
            const sret_ptr = try func.emitLoadLocal(sret_local.offset, .ptr);
            try func.emitCopyStruct(sret_ptr, result_reg, struct_idx);
            try func.emitRet(null, .w32);
        } else {
            // If the expression type doesn't match the declared return type
            // (semantic error already reported), trap at runtime so the user
            // knows this code path is invalid.
            const expr_type = self.node_types.get(ret.expr) orelse .unresolved;
            const expr_width = typeIdToWidth(expr_type, self.target);
            if (expr_width != ret_width) {
                try func.emitTrap();
                try func.emitRet(null, ret_width);
            } else {
                try func.emitRet(result_reg, ret_width);
            }
        }
    }

    fn generateExpression(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?VReg {
        // Trap at expressions containing lexer errors
        const loc = self.ast.getLocation(node_idx);
        for (self.lexer_error_positions) |err_pos| {
            if (err_pos >= loc.start and err_pos < loc.end) {
                try self.current_func.?.emitTrap();
                return null;
            }
        }

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
            .unary_op => try self.generateUnaryOp(node_idx),
            .array_literal => try self.generateArrayLiteral(node_idx),
            .array_index => try self.generateArrayIndex(node_idx),
            .array_slice => try self.generateArraySlice(node_idx),
            else => null,
        };
    }

    fn generateUnaryOp(self: *CodeGenContext, node_idx: NodeIndex) !?VReg {
        const func = self.current_func.?;
        const unary = self.ast.getUnaryOp(node_idx);
        const operand_reg = try self.generateExpression(unary.operand) orelse return null;
        const width = if (self.node_types.get(node_idx)) |tid| typeIdToWidth(tid, self.target) else Width.w32;

        switch (unary.op) {
            .negate => {
                const zero = try func.emitMovImm(0, width);
                return try func.emitBinOp(.sub, zero, operand_reg, width);
            },
            .not => {
                const zero = try func.emitMovImm(0, width);
                return try func.emitCmp(.eq, operand_reg, zero, width);
            },
        }
    }

    fn generateLiteral(self: *CodeGenContext, node_idx: NodeIndex) !?VReg {
        const func = self.current_func.?;
        const lit = self.ast.getLiteral(node_idx);
        const token = self.tokens.items[lit.token_idx];
        const value_str = self.src.getSlice(token.start, token.start + token.len);

        // Resolve width from semantic analysis; bare unresolved literals default to w32
        const width = if (self.node_types.get(node_idx)) |tid| typeIdToWidth(tid, self.target) else Width.w32;

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
                const value = std.fmt.parseInt(i64, value_str, 0) catch 0;
                return try func.emitMovImm(value, width);
            },
            .string_literal => {
                return try self.generateStringLiteral(value_str);
            },
            else => return null,
        }
    }

    fn generateStringLiteral(self: *CodeGenContext, raw: []const u8) CodeGenError!?VReg {
        const func = self.current_func.?;

        // Process escape sequences → byte array
        const content = processStringEscapes(self.allocator, raw) orelse return null;

        // Append null terminator (sentinel) — string type is [:0]u8
        const bytes_with_sentinel = self.allocator.alloc(u8, content.len + 1) catch return null;
        @memcpy(bytes_with_sentinel[0..content.len], content);
        bytes_with_sentinel[content.len] = 0;

        // Generate unique name: str.0, str.1, ...
        const name = std.fmt.allocPrint(self.allocator, "str.{d}", .{self.next_string_id}) catch return null;
        self.next_string_id += 1;

        // Register as anonymous global constant
        const global_idx = try self.mir.globals.add(
            self.allocator,
            name,
            .ptr,
            .unresolved, // type_id not used — LLVM emission uses string_inits
            true, // is_const
            null, // no scalar init
            std.math.maxInt(SymbolIndex), // no symbol
        );
        try self.mir.globals.string_inits.put(self.allocator, global_idx, bytes_with_sentinel);

        // Create slice fat pointer: { ptr to global, content length (excludes sentinel) }
        const global_ptr = try func.emitAddrOfGlobal(global_idx);
        const len_reg = try func.emitMovImm(@intCast(content.len), self.target.ptrWidth());

        const slice_offset = try self.locals.add(self.allocator, "__str_tmp", .ptr);
        try func.emitAllocaSliceLocal(slice_offset);
        const slice_ptr = try func.emitAddrOfLocal(slice_offset);
        try func.emitMakeSlice(slice_ptr, global_ptr, len_reg);

        return slice_ptr;
    }

    fn processStringEscapes(allocator: mem.Allocator, raw: []const u8) ?[]const u8 {
        // Count output length (escapes shrink the string)
        var out_len: usize = 0;
        var i: usize = 0;
        while (i < raw.len) {
            if (raw[i] == '\\' and i + 1 < raw.len) {
                i += 2; // skip escape pair
            } else {
                i += 1;
            }
            out_len += 1;
        }

        const buf = allocator.alloc(u8, out_len) catch return null;
        var out: usize = 0;
        i = 0;
        while (i < raw.len) {
            if (raw[i] == '\\' and i + 1 < raw.len) {
                buf[out] = switch (raw[i + 1]) {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '0' => 0,
                    '\\' => '\\',
                    '"' => '"',
                    else => raw[i + 1], // unknown escape: keep literal char
                };
                i += 2;
            } else {
                buf[out] = raw[i];
                i += 1;
            }
            out += 1;
        }

        return buf[0..out_len];
    }

    fn generateIdentifier(self: *CodeGenContext, node_idx: NodeIndex) !?VReg {
        const ident = self.ast.getIdentifier(node_idx);
        const token = self.tokens.items[ident.token_idx];
        const name = self.src.getSlice(token.start, token.start + token.len);

        // Check locals (for function-scoped variables)
        if (self.locals.lookup(name)) |local| {
            // Array locals: the local IS the [N x T] storage, return its address
            const node_type = self.node_types.get(node_idx) orelse .unresolved;
            if (node_type.isArray()) {
                return try self.current_func.?.emitAddrOfLocal(local.offset);
            }
            return try self.current_func.?.emitLoadLocal(local.offset, local.width);
        }

        // Then check symbols (globals/constants)
        const sym_idx = self.symbols.lookup(name) orelse return null;
        const type_id = self.symbols.getTypeId(sym_idx);
        const kind = self.symbols.getKind(sym_idx);

        return switch (kind) {
            .constant => {
                // For struct/array constants, comptime returns null — use global path
                const result = try self.generateConstLoad(sym_idx, type_id);
                if (result == null and (type_id.isStruct() or type_id.isArray())) {
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
        if (type_id.isStruct() or type_id.isArray() or type_id.isSlice()) {
            // The global IS the data storage; return a pointer to it
            return try func.emitAddrOfGlobal(global_idx);
        }
        const width = typeIdToWidth(type_id, self.target);
        return try func.emitLoadGlobal(global_idx, width);
    }

    /// Map a resolved TypeId to an LLVM type string for use in MIR instructions.
    fn typeIdToLLVMTypeStr(type_id: TypeId, target: Target) []const u8 {
        return switch (type_id) {
            .primitive => |p| switch (p) {
                .void => "void",
                .bool => "i1",
                .u8, .i8 => "i8",
                .u16, .i16 => "i16",
                .f16 => "half",
                .u32, .i32 => "i32",
                .f32 => "float",
                .u64, .i64 => "i64",
                .usize => target.ptrLLVMType(),
                .f64 => "double",
            },
            .struct_type, .pointer, .array, .slice => "ptr",
            .unresolved => "i32",
            else => "i32",
        };
    }

    /// Map a resolved TypeId to a MIR operand width.
    /// .unresolved reaching codegen means semantic analysis reported an error (S018)
    /// but the compiler still produces an executable. w32 is used as error recovery.
    /// Struct types are passed by pointer, so they use .ptr width.
    fn typeIdToWidth(type_id: TypeId, target: Target) Width {
        return switch (type_id) {
            .primitive => |prim| switch (prim) {
                .bool, .i8, .u8 => .w8,
                .i16, .u16 => .w16,
                .i32, .u32 => .w32,
                .i64, .u64 => .w64,
                .usize => target.ptrWidth(),
                .f16 => .wf16,
                .f32 => .wf32,
                .f64 => .wf64,
                .void => .w32, // error recovery
            },
            .struct_type => .ptr,
            .pointer => .ptr,
            .array => .ptr,
            .slice => .ptr,
            .unresolved => .w32,
            else => .w32,
        };
    }

    /// Resolve the return width for a function from the return type node.
    /// Returns null for void, w32/w64 for value types, w32 as fallback.
    fn resolveReturnWidth(
        node_types: *const std.AutoHashMapUnmanaged(NodeIndex, TypeId),
        return_type_node: NodeIndex,
        target: Target,
    ) ?Width {
        const ret_type = node_types.get(return_type_node) orelse return .w32;
        if (ret_type == .primitive and ret_type.primitive == .void) return null;
        return typeIdToWidth(ret_type, target);
    }

    fn generateConstLoad(self: *CodeGenContext, sym_idx: SymbolIndex, type_id: TypeId) !?VReg {
        const eval_state = self.comptime_result.eval_states.items[sym_idx];
        if (eval_state != .evaluated) return null;

        const value_str = self.comptime_result.getEvalLiteral(sym_idx) orelse return null;

        return switch (type_id) {
            .primitive => |prim| self.generatePrimitiveImmediate(prim, value_str),
            .slice => self.generateStringLiteral(value_str),
            else => null,
        };
    }

    fn generatePrimitiveImmediate(self: *CodeGenContext, prim: PrimitiveType, value_str: []const u8) !?VReg {
        const func = self.current_func.?;

        switch (prim) {
            .bool => {
                const val: i64 = if (std.mem.eql(u8, value_str, "true")) 1 else 0;
                return try func.emitMovImm(val, .w8);
            },
            .i8, .u8 => {
                const val = std.fmt.parseInt(i64, value_str, 0) catch 0;
                return try func.emitMovImm(val, .w8);
            },
            .i16, .u16 => {
                const val = std.fmt.parseInt(i64, value_str, 0) catch 0;
                return try func.emitMovImm(val, .w16);
            },
            .i32, .u32 => {
                const val = std.fmt.parseInt(i64, value_str, 0) catch 0;
                return try func.emitMovImm(val, .w32);
            },
            .i64, .u64 => {
                const val = std.fmt.parseInt(i64, value_str, 0) catch 0;
                return try func.emitMovImm(val, .w64);
            },
            .usize => {
                const val = std.fmt.parseInt(i64, value_str, 0) catch 0;
                return try func.emitMovImm(val, self.target.ptrWidth());
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
            const operand_width = typeIdToWidth(operand_type, self.target);
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
        const width = if (self.node_types.get(node_idx)) |tid| typeIdToWidth(tid, self.target) else Width.w32;
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

        // 2. Look up function to get calling convention and extern name
        const sym_idx = self.symbols.lookup(func_name) orelse return null;
        const func_type_id = self.symbols.getTypeId(sym_idx);
        const func_type_info = self.types.getFunctionType(func_type_id);
        const call_conv = if (func_type_info) |ft| ft.calling_conv else .honey;
        const is_variadic = if (func_type_info) |ft| ft.is_variadic else false;

        // Use extern_name for imported extern C functions (unqualified symbol name)
        const emit_name = self.symbols.getExternName(sym_idx) orelse func_name;

        // 3. Generate argument expressions
        const arg_nodes = self.ast.getExtra(call.args);
        const param_types = self.types.getParamTypes(func_type_id);
        var arg_regs = try std.ArrayList(VReg).initCapacity(self.allocator, arg_nodes.len);
        defer arg_regs.deinit(self.allocator);
        var arg_widths = try std.ArrayList(Width).initCapacity(self.allocator, arg_nodes.len);
        defer arg_widths.deinit(self.allocator);
        var arg_struct_indices = try std.ArrayList(?u32).initCapacity(self.allocator, arg_nodes.len);
        defer arg_struct_indices.deinit(self.allocator);
        var arg_slice_flags = try std.ArrayList(bool).initCapacity(self.allocator, arg_nodes.len);
        defer arg_slice_flags.deinit(self.allocator);

        const fixed_param_len: usize = if (param_types) |pt| pt.len else 0;

        // Phase A: Process fixed (non-variadic) parameters
        for (arg_nodes, 0..) |arg_idx, arg_i| {
            if (is_variadic and arg_i >= fixed_param_len) break; // variadic extras handled in Phase B

            const arg_type = self.node_types.get(arg_idx) orelse .unresolved;
            const expected_type: ?TypeId = if (param_types) |pt| (if (arg_i < pt.len) pt[arg_i] else null) else null;

            const arg_width = typeIdToWidth(arg_type, self.target);
            const arg_reg = try self.generateExpression(arg_idx) orelse {
                const zero = try func.emitMovImm(0, .w32);
                try arg_regs.append(self.allocator, zero);
                try arg_widths.append(self.allocator, .w32);
                try arg_struct_indices.append(self.allocator, null);
                try arg_slice_flags.append(self.allocator, false);
                continue;
            };
            // Sentinel-terminated slice in C convention → extract data pointer
            const is_expected_slice = expected_type != null and expected_type.?.isSlice();
            const is_c_sentinel = is_expected_slice and call_conv == .c and
                (if (self.types.getSliceType(expected_type.?)) |si| si.sentinel != null else false);

            if (is_c_sentinel) {
                const data_ptr = try func.emitSliceGetPtr(arg_reg);
                try arg_regs.append(self.allocator, data_ptr);
                try arg_widths.append(self.allocator, .ptr);
                try arg_struct_indices.append(self.allocator, null);
                try arg_slice_flags.append(self.allocator, false);
            } else {
                try arg_regs.append(self.allocator, arg_reg);
                try arg_widths.append(self.allocator, arg_width);
                try arg_struct_indices.append(self.allocator, if (arg_type.isStruct()) arg_type.struct_type else null);
                try arg_slice_flags.append(self.allocator, is_expected_slice);
            }
        }

        // Phase B: Collect variadic extra args into a tuple, then unpack for C calls
        if (is_variadic and arg_nodes.len > fixed_param_len) {
            const extra_args = arg_nodes[fixed_param_len..];

            // Evaluate each extra arg and collect types + registers
            var extra_regs = try std.ArrayList(VReg).initCapacity(self.allocator, extra_args.len);
            defer extra_regs.deinit(self.allocator);
            var extra_types = try std.ArrayList(TypeId).initCapacity(self.allocator, extra_args.len);
            defer extra_types.deinit(self.allocator);

            for (extra_args) |arg_idx| {
                const arg_type = self.node_types.get(arg_idx) orelse .unresolved;
                const arg_reg = try self.generateExpression(arg_idx) orelse {
                    try extra_regs.append(self.allocator, try func.emitMovImm(0, .w32));
                    try extra_types.append(self.allocator, TypeId.i32);
                    continue;
                };
                try extra_regs.append(self.allocator, arg_reg);
                try extra_types.append(self.allocator, arg_type);
            }

            // Note: we conceptually have a tuple {arg0, arg1, ...} here.
            // For C calls, we unpack the tuple fields directly into the call args.
            // For future Honey variadic calls, we'd pass the tuple pointer as a single arg.

            if (call_conv == .c) {
                // Unpack tuple for C ABI: each field becomes an individual call argument
                for (extra_regs.items, extra_types.items) |reg, typ| {
                    // Sentinel slice → extract data pointer for C
                    if (typ.isSlice()) {
                        if (self.types.getSliceType(typ)) |si| {
                            if (si.sentinel != null) {
                                const data_ptr = try func.emitSliceGetPtr(reg);
                                try arg_regs.append(self.allocator, data_ptr);
                                try arg_widths.append(self.allocator, .ptr);
                                try arg_struct_indices.append(self.allocator, null);
                                try arg_slice_flags.append(self.allocator, false);
                                continue;
                            }
                        }
                    }
                    try arg_regs.append(self.allocator, reg);
                    try arg_widths.append(self.allocator, typeIdToWidth(typ, self.target));
                    try arg_struct_indices.append(self.allocator, null);
                    try arg_slice_flags.append(self.allocator, false);
                }
            } else {
                // Honey-native variadic: allocate tuple struct, store fields, pass pointer.
                // Full implementation requires monomorphization (each call site has a different
                // tuple type) which depends on comptime infrastructure. For now, pass extra args
                // individually (same layout as C but without ABI-specific transformations).
                for (extra_regs.items, extra_types.items) |reg, typ| {
                    try arg_regs.append(self.allocator, reg);
                    try arg_widths.append(self.allocator, typeIdToWidth(typ, self.target));
                    try arg_struct_indices.append(self.allocator, if (typ.isStruct()) typ.struct_type else null);
                    try arg_slice_flags.append(self.allocator, typ.isSlice());
                }
            }
        }

        // 4. Check for argument type mismatches — emit trap if any arg width
        //    doesn't match the expected parameter width (error already reported
        //    by semantic analysis; always-compile inserts a runtime trap).
        if (param_types) |pt| {
            const check_count = @min(arg_widths.items.len, pt.len);
            for (0..check_count) |i| {
                const expected_width = typeIdToWidth(pt[i], self.target);
                if (arg_widths.items[i] != expected_width) {
                    try func.emitTrap();
                    // Return a zero register as placeholder for the call result
                    return try func.emitMovImm(0, .w32);
                }
            }
        }

        // 5. Determine return type and check for sret
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

            var sret_slice_flags = try std.ArrayList(bool).initCapacity(self.allocator, arg_slice_flags.items.len + 1);
            defer sret_slice_flags.deinit(self.allocator);
            try sret_slice_flags.append(self.allocator, false); // sret pointer is not a slice
            for (arg_slice_flags.items) |f| try sret_slice_flags.append(self.allocator, f);

            // call returns void; the struct is written via sret pointer
            const fixed_count: u32 = if (param_types) |pt| @intCast(pt.len) else 0;
            _ = try func.emitCall(emit_name, sret_args.items, sret_widths.items, sret_struct_indices.items, sret_slice_flags.items, call_conv, null, struct_idx, is_variadic, fixed_count);

            return result_ptr;
        }

        // 5. Non-sret call
        const return_width: ?Width = if (return_type) |rt| blk: {
            if (rt == .unresolved) break :blk .w32;
            if (rt == .primitive and rt.primitive == .void) break :blk null;
            break :blk typeIdToWidth(rt, self.target);
        } else .w32;

        const fixed_count: u32 = if (param_types) |pt| @intCast(pt.len) else 0;
        return try func.emitCall(emit_name, arg_regs.items, arg_widths.items, arg_struct_indices.items, arg_slice_flags.items, call_conv, return_width, null, is_variadic, fixed_count);
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

        // slice .len access
        if (object_type.isSlice()) {
            const field_ident = self.ast.getIdentifier(access.field);
            const field_token = self.tokens.items[field_ident.token_idx];
            const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);
            if (mem.eql(u8, field_name, "len")) {
                const base_reg = try self.generateExpression(access.object) orelse return null;
                return try func.emitSliceGetLen(base_reg);
            }
            return null;
        }

        // array .len access (compile-time constant)
        if (object_type.isArray()) {
            const field_ident = self.ast.getIdentifier(access.field);
            const field_token = self.tokens.items[field_ident.token_idx];
            const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);
            if (mem.eql(u8, field_name, "len")) {
                const arr_info = self.types.getArrayType(object_type) orelse return null;
                return try func.emitMovImm(@intCast(arr_info.length), self.target.ptrWidth());
            }
            return null;
        }

        // auto-deref: unwrap pointer layers to reach the struct type
        var resolved_type = object_type;
        while (resolved_type.isPointer()) {
            const ptr_info = self.types.getPointerType(resolved_type) orelse break;
            resolved_type = ptr_info.pointee;
        }

        if (resolved_type != .struct_type) return null;

        // generate the base object expression (produces a pointer for struct types;
        // for pointer-to-struct, the pointer value already points to struct data)
        const base_reg = try self.generateExpression(access.object) orelse return null;

        const struct_idx = resolved_type.struct_type;
        const struct_type = self.types.getStructType(resolved_type) orelse return null;

        // get field name — identifier for named fields, literal for tuple .0 .1
        const field_name = self.getFieldNameFromNode(access.field);

        // find field index
        for (struct_type.fields, 0..) |field, i| {
            if (mem.eql(u8, field.name, field_name)) {
                const field_width = typeIdToWidth(field.type_id, self.target);
                return try func.emitLoadField(base_reg, struct_idx, @intCast(i), field_width);
            }
        }

        return null;
    }

    /// Get field name from a field access node — handles both .identifier and .literal (numeric) fields.
    fn getFieldNameFromNode(self: *CodeGenContext, field_node: NodeIndex) []const u8 {
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
            var object_type = self.node_types.get(access.object) orelse return null;
            // auto-deref pointer layers
            while (object_type.isPointer()) {
                const ptr_info = self.types.getPointerType(object_type) orelse break;
                object_type = ptr_info.pointee;
            }
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
        const width = typeIdToWidth(result_type, self.target);
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
            const width = typeIdToWidth(target_type, self.target);
            try func.emitStorePtr(ptr_reg, value_reg, width);
        }
    }

    fn generateArrayLiteral(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?VReg {
        const func = self.current_func.?;
        const lit = self.ast.getArrayLiteral(node_idx);

        // get array type from semantic analysis
        const type_id = self.node_types.get(node_idx) orelse return null;
        if (type_id != .array) return null;

        const array_idx = type_id.array;
        const arr_info = self.types.getArrayType(type_id) orelse return null;

        // allocate array on stack
        const base = try func.emitAllocaArray(array_idx);

        // store each element
        const elements = self.ast.getExtra(lit.elements);
        const is_struct_elem = arr_info.element_type.isStruct();

        for (elements, 0..) |elem_idx, i| {
            const value_reg = try self.generateExpression(elem_idx) orelse continue;
            const index_reg = try func.emitMovImm(@intCast(i), self.target.ptrWidth());

            if (is_struct_elem) {
                // struct elements: GEP to array slot, then memcpy
                const slot_ptr = try func.emitAddrOfElement(base, index_reg, array_idx);
                try func.emitCopyStruct(slot_ptr, value_reg, arr_info.element_type.struct_type);
            } else {
                const elem_width = typeIdToWidth(arr_info.element_type, self.target);
                try func.emitStoreElement(base, index_reg, value_reg, array_idx, elem_width);
            }
        }

        // return pointer to the array
        return base;
    }

    /// Emit a runtime bounds check: if index >= length, trap.
    fn emitBoundsCheck(self: *CodeGenContext, index_reg: VReg, length_reg: VReg) CodeGenError!void {
        const func = self.current_func.?;
        const ptr_width = self.target.ptrWidth();

        // compare: index >= length (unsigned)
        const cmp_reg = try func.emitCmp(.ge_u, index_reg, length_reg, ptr_width);

        // branch: if out of bounds goto trap, else continue
        const trap_label = func.allocLabel();
        const ok_label = func.allocLabel();
        try func.emitBrCond(cmp_reg, .w8, trap_label, ok_label);

        // trap block
        try func.emitLabel(trap_label);
        try func.emitTrap();

        // continue
        try func.emitLabel(ok_label);
    }

    fn generateArrayIndex(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?VReg {
        const func = self.current_func.?;
        const idx_node = self.ast.getArrayIndex(node_idx);

        // generate the object (array/slice pointer)
        const base_reg = try self.generateExpression(idx_node.object) orelse return null;

        // generate the index
        const index_reg = try self.generateExpression(idx_node.index) orelse return null;

        // get type from the object
        const obj_type = self.node_types.get(idx_node.object) orelse return null;

        if (obj_type.isSlice()) {
            const slice_info = self.types.getSliceType(obj_type) orelse return null;
            const elem_llvm_type = typeIdToLLVMTypeStr(slice_info.element_type, self.target);
            const elem_width = typeIdToWidth(slice_info.element_type, self.target);
            // bounds check: index < slice.len
            const len_reg = try func.emitSliceGetLen(base_reg);
            try self.emitBoundsCheck(index_reg, len_reg);
            // extract data pointer from fat pointer, GEP to element, load
            const data_ptr = try func.emitSliceGetPtr(base_reg);
            const elem_ptr = try func.emitSliceElemPtr(data_ptr, index_reg, elem_llvm_type);
            return try func.emitLoadPtr(elem_ptr, elem_width);
        }

        if (obj_type != .array) return null;

        const array_idx = obj_type.array;
        const arr_info = self.types.getArrayType(obj_type) orelse return null;

        // bounds check: index < array.length
        const len_reg = try func.emitMovImm(@intCast(arr_info.length), self.target.ptrWidth());
        try self.emitBoundsCheck(index_reg, len_reg);

        if (arr_info.element_type.isStruct()) {
            // struct elements: GEP-only, return pointer to inline struct
            return try func.emitAddrOfElement(base_reg, index_reg, array_idx);
        }

        const elem_width = typeIdToWidth(arr_info.element_type, self.target);
        return try func.emitLoadElement(base_reg, index_reg, array_idx, elem_width);
    }

    fn generateArrayIndexAssignment(self: *CodeGenContext, target_node: NodeIndex, value_reg: VReg) CodeGenError!void {
        const func = self.current_func.?;
        const idx_node = self.ast.getArrayIndex(target_node);

        // generate the base (array pointer)
        const base_reg = try self.generateExpression(idx_node.object) orelse return;

        // generate the index
        const index_reg = try self.generateExpression(idx_node.index) orelse return;

        // get object type
        const obj_type = self.node_types.get(idx_node.object) orelse return;

        if (obj_type.isSlice()) {
            const slice_info = self.types.getSliceType(obj_type) orelse return;
            const elem_llvm_type = typeIdToLLVMTypeStr(slice_info.element_type, self.target);
            const elem_width = typeIdToWidth(slice_info.element_type, self.target);

            // bounds check: index < slice.len
            const len_reg = try func.emitSliceGetLen(base_reg);
            try self.emitBoundsCheck(index_reg, len_reg);

            // store through slice: get data ptr, GEP to element, store
            const data_ptr = try func.emitSliceGetPtr(base_reg);
            const elem_ptr = try func.emitSliceElemPtr(data_ptr, index_reg, elem_llvm_type);
            try func.emitStorePtr(elem_ptr, value_reg, elem_width);
            return;
        }

        if (obj_type != .array) return;

        const array_idx = obj_type.array;
        const arr_info = self.types.getArrayType(obj_type) orelse return;

        // bounds check: index < array.length
        const len_reg = try func.emitMovImm(@intCast(arr_info.length), self.target.ptrWidth());
        try self.emitBoundsCheck(index_reg, len_reg);

        if (arr_info.element_type.isStruct()) {
            // struct elements: GEP to array slot, then memcpy
            const slot_ptr = try func.emitAddrOfElement(base_reg, index_reg, array_idx);
            try func.emitCopyStruct(slot_ptr, value_reg, arr_info.element_type.struct_type);
        } else {
            const elem_width = typeIdToWidth(arr_info.element_type, self.target);
            try func.emitStoreElement(base_reg, index_reg, value_reg, array_idx, elem_width);
        }
    }

    /// Emit a runtime check: if val > limit, trap. (for slice end <= length)
    fn emitUpperBoundsCheck(self: *CodeGenContext, val_reg: VReg, limit_reg: VReg) CodeGenError!void {
        const func = self.current_func.?;
        const ptr_width = self.target.ptrWidth();
        const cmp_reg = try func.emitCmp(.gt_u, val_reg, limit_reg, ptr_width);
        const trap_label = func.allocLabel();
        const ok_label = func.allocLabel();
        try func.emitBrCond(cmp_reg, .w8, trap_label, ok_label);
        try func.emitLabel(trap_label);
        try func.emitTrap();
        try func.emitLabel(ok_label);
    }

    fn generateArraySlice(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?VReg {
        const func = self.current_func.?;
        const slice_node = self.ast.getArraySlice(node_idx);
        const ptr_width = self.target.ptrWidth();

        // generate object
        const base_reg = try self.generateExpression(slice_node.object) orelse return null;
        const obj_type = self.node_types.get(slice_node.object) orelse return null;

        // generate start (default: 0) and end (default: length)
        const start_reg = if (slice_node.range_start) |s|
            try self.generateExpression(s) orelse return null
        else
            try func.emitMovImm(0, ptr_width);

        // determine element type, data pointer, length, and end bound
        var element_type: TypeId = undefined;
        var data_ptr: VReg = undefined;
        var end_reg: VReg = undefined;

        if (obj_type.isArray()) {
            const arr_info = self.types.getArrayType(obj_type) orelse return null;
            element_type = arr_info.element_type;
            const len_reg = try func.emitMovImm(@intCast(arr_info.length), ptr_width);

            end_reg = if (slice_node.range_end) |e|
                try self.generateExpression(e) orelse return null
            else
                len_reg;

            // bounds checks: end <= length, start <= end
            try self.emitUpperBoundsCheck(end_reg, len_reg);
            try self.emitUpperBoundsCheck(start_reg, end_reg);

            data_ptr = base_reg;
        } else if (obj_type.isSlice()) {
            const slice_info = self.types.getSliceType(obj_type) orelse return null;
            element_type = slice_info.element_type;
            const len_reg = try func.emitSliceGetLen(base_reg);

            end_reg = if (slice_node.range_end) |e|
                try self.generateExpression(e) orelse return null
            else
                len_reg;

            // bounds checks: end <= length, start <= end
            try self.emitUpperBoundsCheck(end_reg, len_reg);
            try self.emitUpperBoundsCheck(start_reg, end_reg);

            data_ptr = try func.emitSliceGetPtr(base_reg);
        } else {
            return null;
        }

        // compute new pointer: data_ptr + start * elem_size
        const stride: u32 = types_mod.sizeOf(element_type, self.types);
        const offset_ptr = try func.emitPtrOffset(data_ptr, start_reg, stride, false);

        // compute new length: end - start
        const new_len = try func.emitBinOp(.sub, end_reg, start_reg, ptr_width);

        // allocate temporary slice fat pointer and fill it
        const slice_offset = try self.locals.add(self.allocator, "__slice_tmp", .ptr);
        try func.emitAllocaSliceLocal(slice_offset);
        const slice_ptr = try func.emitAddrOfLocal(slice_offset);
        try func.emitMakeSlice(slice_ptr, offset_ptr, new_len);

        return slice_ptr;
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
        var positional_idx: usize = 0;

        const field_data = self.ast.getExtra(lit.fields);
        var fi: usize = 0;
        while (fi < field_data.len) : (fi += 2) {
            const field_name_idx = field_data[fi];
            const field_value_idx = field_data[fi + 1];

            // get field name — sentinel means positional (tuple literal)
            const field_name = if (field_name_idx == std.math.maxInt(NodeIndex)) blk: {
                const name = tupleFieldName(self.allocator, positional_idx) catch "?";
                positional_idx += 1;
                break :blk name;
            } else self.getFieldNameFromNode(field_name_idx);

            // find field index in struct definition
            for (struct_type.fields, 0..) |field, i| {
                if (mem.eql(u8, field.name, field_name)) {
                    // Skip store if value type doesn't match field type (error already reported by sema)
                    const value_type = self.node_types.get(field_value_idx) orelse break;
                    const field_width = typeIdToWidth(field.type_id, self.target);
                    const value_width = typeIdToWidth(value_type, self.target);
                    if (field_width != value_width) break;

                    const value_reg = try self.generateExpression(field_value_idx) orelse continue;
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
