const std = @import("std");
const mem = std.mem;

str_pool: *StringPool,
hir: *const HIR,
diagnostics: *Diagnostic,
shared_alloc: mem.Allocator,
mir: MIR,
scope: Scope,
type_map: std.AutoHashMapUnmanaged(StringPool.ID, MIR.Inst.Type),
/// resolved type per mir inst (parallel to mir.insts). sema-only.
inst_types: std.ArrayList(MIR.Inst.Type),
/// maps hir inst refs to mir inst refs.
ref_map: std.ArrayList(BaseRef),
/// tracks hir instructions already analyzed, including instructions that map to .none.
analyzed: std.DynamicBitSetUnmanaged,
current_ret_type: ?MIR.Inst.Type,

const Self = @This();

const StringPool = @import("../util/StringPool.zig");
const BaseRef = @import("../root.zig").BaseRef;
const Diagnostic = @import("../diagnostic/Store.zig");
const HIR = @import("../parser/HIR.zig");
const MIR = @import("MIR.zig");

const Binding = union(enum) {
    pending_decl: HIR.Inst.Ref,
    analyzing_decl: HIR.Inst.Ref,
    value: MIR.Inst.Ref,
    namespace,
};

const Scope = struct {
    parent: ?*Scope = null,
    bindings: std.AutoHashMapUnmanaged(StringPool.ID, Binding) = .{},

    fn deinit(self: *Scope, alloc: mem.Allocator) void {
        self.bindings.deinit(alloc);
    }

    fn localBinding(self: *Scope, name: StringPool.ID) ?*Binding {
        return self.bindings.getPtr(name);
    }
};

const ResolvedBinding = struct {
    scope: *Scope,
    binding: *Binding,
};

const TypeResolution = struct {
    type: MIR.Inst.Type,
    diagnostic: Diagnostic.Ref = .none,
};

pub fn init(hir: *const HIR, str_pool: *StringPool, diagnostics: *Diagnostic, shared_alloc: mem.Allocator) Self {
    return .{
        .str_pool = str_pool,
        .hir = hir,
        .diagnostics = diagnostics,
        .shared_alloc = shared_alloc,
        .mir = .{},
        .scope = .{},
        .type_map = .{},
        .inst_types = .empty,
        .ref_map = .empty,
        .analyzed = .{},
        .current_ret_type = null,
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.mir.deinit(alloc);
    self.scope.deinit(alloc);
    self.type_map.deinit(alloc);
    self.inst_types.deinit(alloc);
    self.ref_map.deinit(alloc);
    self.analyzed.deinit(alloc);
}

pub fn analyze(self: *Self, alloc: mem.Allocator) !void {
    try self.seedBuiltins(alloc);

    try self.ref_map.resize(alloc, self.hir.insts.len);
    @memset(self.ref_map.items, .none);
    self.analyzed = try std.DynamicBitSetUnmanaged.initEmpty(alloc, self.hir.insts.len);

    const root_decls = self.hir.rootDecls();
    try self.predeclareDecls(alloc, &self.scope, root_decls);
    try self.analyzeDecls(alloc, &self.scope, root_decls);
}

fn seedBuiltins(self: *Self, alloc: mem.Allocator) !void {
    const int_str_id = try self.str_pool.intern(self.shared_alloc, "int");
    const float_str_id = try self.str_pool.intern(self.shared_alloc, "float");
    const i32_str_id = try self.str_pool.intern(self.shared_alloc, "i32");
    const f32_str_id = try self.str_pool.intern(self.shared_alloc, "f32");
    const bool_str_id = try self.str_pool.intern(self.shared_alloc, "bool");
    const void_str_id = try self.str_pool.intern(self.shared_alloc, "void");

    const true_str_id = try self.str_pool.intern(self.shared_alloc, "true");
    const false_str_id = try self.str_pool.intern(self.shared_alloc, "false");

    try self.type_map.put(alloc, i32_str_id, .i32);
    try self.type_map.put(alloc, f32_str_id, .f32);
    try self.type_map.put(alloc, bool_str_id, .bool);
    try self.type_map.put(alloc, void_str_id, .void);
    try self.type_map.put(alloc, int_str_id, .i32);
    try self.type_map.put(alloc, float_str_id, .f32);

    const true_ref = try self.emitTyped(alloc, .bool_literal, .{ .a = @enumFromInt(1) }, .bool);
    const false_ref = try self.emitTyped(alloc, .bool_literal, .{ .a = @enumFromInt(0) }, .bool);

    try self.scope.bindings.put(alloc, true_str_id, .{ .value = true_ref });
    try self.scope.bindings.put(alloc, false_str_id, .{ .value = false_ref });
}

fn predeclareDecls(self: *Self, alloc: mem.Allocator, scope: *Scope, refs: []const BaseRef) anyerror!void {
    for (refs) |ref| {
        const inst = self.hir.insts.get(@intFromEnum(ref));
        switch (inst.tag) {
            .decl_const, .decl_var, .decl_func => {
                const name: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
                if (!try self.bind(scope, alloc, name, .{ .pending_decl = ref }))
                    _ = try self.emitTrap(alloc, .sema_duplicate_declaration);
            },
            .decl_namespace => {
                const name: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
                if (!try self.bind(scope, alloc, name, .namespace))
                    _ = try self.emitTrap(alloc, .sema_duplicate_declaration);
            },
            else => {},
        }
    }
}

fn analyzeDecls(self: *Self, alloc: mem.Allocator, scope: *Scope, refs: []const BaseRef) anyerror!void {
    for (refs) |ref| _ = try self.analyzeDecl(alloc, scope, ref);
}

fn analyzeDecl(self: *Self, alloc: mem.Allocator, scope: *Scope, ref: HIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    if (self.analyzed.isSet(@intFromEnum(ref))) return self.ref_map.items[@intFromEnum(ref)];

    const inst = self.hir.insts.get(@intFromEnum(ref));
    const name: StringPool.ID = switch (inst.tag) {
        .decl_const, .decl_var, .decl_func, .decl_namespace => @enumFromInt(@intFromEnum(inst.data.a)),
        else => return self.analyzeInst(alloc, scope, ref),
    };

    if (inst.tag == .decl_namespace) {
        try self.analyzeNamespace(alloc, scope, ref);
        return .none;
    }

    const binding = scope.localBinding(name) orelse return self.emitTrap(alloc, .sema_duplicate_declaration);
    switch (binding.*) {
        .value => |mir_ref| {
            self.ref_map.items[@intFromEnum(ref)] = mir_ref;
            self.analyzed.set(@intFromEnum(ref));
            return mir_ref;
        },
        .pending_decl => |pending_ref| {
            if (pending_ref != ref) return self.emitTrap(alloc, .sema_duplicate_declaration);
        },
        .analyzing_decl => return self.emitTrap(alloc, .sema_declaration_cycle),
        .namespace => return self.emitTrap(alloc, .sema_namespace_not_value),
    }

    binding.* = .{ .analyzing_decl = ref };
    const mir_ref = switch (inst.tag) {
        .decl_const, .decl_var => try self.analyzeValueDecl(alloc, scope, ref),
        .decl_func => try self.analyzeFuncDecl(alloc, scope, ref),
        else => unreachable,
    };
    binding.* = .{ .value = mir_ref };
    self.ref_map.items[@intFromEnum(ref)] = mir_ref;
    self.analyzed.set(@intFromEnum(ref));
    return mir_ref;
}

fn analyzeNamespace(self: *Self, alloc: mem.Allocator, parent: *Scope, ref: HIR.Inst.Ref) anyerror!void {
    if (self.analyzed.isSet(@intFromEnum(ref))) return;

    const inst = self.hir.insts.get(@intFromEnum(ref));
    std.debug.assert(inst.tag == .decl_namespace);

    var scope: Scope = .{ .parent = parent };
    defer scope.deinit(alloc);

    const block = self.hir.insts.get(@intFromEnum(inst.data.b));
    std.debug.assert(block.tag == .block);
    const members = self.hir.extraSlice(block.data.a, block.data.b);

    try self.predeclareDecls(alloc, &scope, members);
    try self.analyzeDecls(alloc, &scope, members);

    self.ref_map.items[@intFromEnum(ref)] = .none;
    self.analyzed.set(@intFromEnum(ref));
}

fn analyzeValueDecl(self: *Self, alloc: mem.Allocator, scope: *Scope, ref: HIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    const inst = self.hir.insts.get(@intFromEnum(ref));
    const hir_decl = self.hir.unpackExtraData(HIR.DeclInfo, inst.data.b);

    var value = try self.analyzeInst(alloc, scope, hir_decl.value);
    if (value == .none) value = try self.emitTrap(alloc, .sema_not_a_value);

    const value_type = self.inst_types.items[@intFromEnum(value)];
    const type_resolution: TypeResolution = if (hir_decl.type != .none)
        try self.resolveType(hir_decl.type)
    else
        .{ .type = value_type };
    var decl_type = type_resolution.type;

    if (hir_decl.type != .none and decl_type != .err and value_type != .err and decl_type != value_type) {
        value = try self.emitTrap(alloc, .sema_type_mismatch);
        decl_type = .err;
    }
    if (type_resolution.diagnostic != .none and value_type != .err)
        value = try self.emitTrapRef(alloc, type_resolution.diagnostic);

    const decl_ref = try self.mir.emitDeclInfo(alloc, .{
        .value = value,
        .type = decl_type,
    });

    const mir_tag: MIR.Inst.Tag = switch (inst.tag) {
        .decl_const => .decl_const,
        .decl_var => .decl_var,
        else => unreachable,
    };
    return self.emitTyped(alloc, mir_tag, .{
        .a = inst.data.a,
        .b = MIR.asBaseRef(decl_ref),
    }, decl_type);
}

fn analyzeFuncDecl(self: *Self, alloc: mem.Allocator, parent: *Scope, ref: HIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    const inst = self.hir.insts.get(@intFromEnum(ref));
    const func_info = self.hir.unpackExtraData(HIR.FuncInfo, inst.data.b);

    var func_scope: Scope = .{ .parent = parent };
    defer func_scope.deinit(alloc);

    const hir_params = self.hir.extraSlice(func_info.params_start, func_info.params_end);
    const params_start = self.mir.refListStart();
    for (hir_params) |hir_ref| {
        const mir_ref = try self.analyzeParam(alloc, &func_scope, hir_ref);
        try self.mir.appendRef(alloc, mir_ref);
    }
    const params_end = self.mir.refListEnd();

    const ret_type = (try self.resolveType(func_info.ret_type)).type;
    const outer_ret_type = self.current_ret_type;
    self.current_ret_type = ret_type;
    defer self.current_ret_type = outer_ret_type;

    const body: MIR.Inst.Ref = if (func_info.body != .none)
        try self.analyzeInst(alloc, &func_scope, func_info.body)
    else
        .none;

    const func_ref = try self.mir.emitFuncInfo(alloc, .{
        .params_start = params_start,
        .params_end = params_end,
        .body = body,
        .ret_type = ret_type,
        .flags = func_info.flags,
    });

    return self.emitTyped(alloc, .decl_func, .{
        .a = inst.data.a,
        .b = MIR.asBaseRef(func_ref),
    }, .void);
}

fn analyzeParam(self: *Self, alloc: mem.Allocator, scope: *Scope, ref: HIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    if (self.analyzed.isSet(@intFromEnum(ref))) return self.ref_map.items[@intFromEnum(ref)];

    const inst = self.hir.insts.get(@intFromEnum(ref));
    std.debug.assert(inst.tag == .param);

    const name: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
    const param_type = (try self.resolveType(inst.data.b)).type;
    const param_ref = try self.emitTyped(alloc, .param, .{
        .a = inst.data.a,
        .b = @enumFromInt(@intFromEnum(param_type)),
    }, param_type);

    if (!try self.bind(scope, alloc, name, .{ .value = param_ref }))
        return self.emitTrap(alloc, .sema_duplicate_declaration);
    self.ref_map.items[@intFromEnum(ref)] = param_ref;
    self.analyzed.set(@intFromEnum(ref));
    return param_ref;
}

fn analyzeInst(self: *Self, alloc: mem.Allocator, scope: *Scope, ref: HIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    if (ref == .none) return .none;
    if (self.analyzed.isSet(@intFromEnum(ref))) return self.ref_map.items[@intFromEnum(ref)];

    const inst = self.hir.insts.get(@intFromEnum(ref));
    const mir_ref = switch (inst.tag) {
        .int_literal, .float_literal => blk: {
            const mir_tag: MIR.Inst.Tag, const mir_type: MIR.Inst.Type = switch (inst.tag) {
                .int_literal => .{ .int_literal, .i32 },
                .float_literal => .{ .float_literal, .f32 },
                else => unreachable,
            };
            break :blk try self.emitTyped(alloc, mir_tag, .{
                .a = @enumFromInt(@intFromEnum(inst.data.a)),
            }, mir_type);
        },
        .trap => try self.emitTyped(alloc, .trap, .{ .a = inst.data.a }, .err),
        .add, .sub, .mul, .div => blk: {
            const left = try self.valueOrTrap(alloc, try self.analyzeInst(alloc, scope, inst.data.a));
            const right = try self.valueOrTrap(alloc, try self.analyzeInst(alloc, scope, inst.data.b));

            const left_type = self.inst_types.items[@intFromEnum(left)];
            const right_type = self.inst_types.items[@intFromEnum(right)];
            if (left_type == .err) break :blk left;
            if (right_type == .err) break :blk right;
            if (left_type != right_type) break :blk try self.emitTrap(alloc, .sema_type_mismatch);

            const mir_tag: MIR.Inst.Tag = switch (inst.tag) {
                .add => .add,
                .sub => .sub,
                .mul => .mul,
                .div => .div,
                else => unreachable,
            };
            break :blk try self.emitTyped(alloc, mir_tag, .{
                .a = left,
                .b = right,
            }, left_type);
        },
        .ref => blk: {
            const name: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
            break :blk try self.resolveValue(alloc, scope, name);
        },
        .decl_const, .decl_var, .decl_func => try self.analyzeLocalOrPredeclaredDecl(alloc, scope, ref),
        .decl_namespace => blk: {
            try self.analyzeNamespace(alloc, scope, ref);
            break :blk .none;
        },
        .block => blk: {
            var block_scope: Scope = .{ .parent = scope };
            defer block_scope.deinit(alloc);

            const hir_stmts = self.hir.extraSlice(inst.data.a, inst.data.b);
            var stmt_refs: std.ArrayList(BaseRef) = .empty;
            defer stmt_refs.deinit(alloc);

            for (hir_stmts) |hir_ref| {
                const stmt_ref = try self.analyzeInst(alloc, &block_scope, hir_ref);
                if (stmt_ref != .none) try stmt_refs.append(alloc, stmt_ref);
            }

            const refs = try self.mir.appendRefList(alloc, stmt_refs.items);

            break :blk try self.emitTyped(alloc, .block, .{
                .a = refs.start,
                .b = refs.end,
            }, .void);
        },
        .ret => blk: {
            const ret_type = self.current_ret_type orelse {
                const trap = try self.emitTrap(alloc, .sema_return_outside_function);
                break :blk try self.emitTyped(alloc, .ret, .{ .a = trap }, .void);
            };
            const val = if (inst.data.a != .none)
                try self.valueOrTrap(alloc, try self.analyzeInst(alloc, scope, inst.data.a))
            else
                .none;
            const ret_val = try self.checkReturnType(alloc, ret_type, val);
            break :blk try self.emitTyped(alloc, .ret, .{ .a = ret_val }, .void);
        },
        .if_simple => blk: {
            var condition = try self.valueOrTrap(alloc, try self.analyzeInst(alloc, scope, inst.data.a));
            const condition_type = self.inst_types.items[@intFromEnum(condition)];
            if (condition_type != .err and condition_type != .bool)
                condition = try self.emitTrap(alloc, .sema_type_mismatch);
            const body = try self.analyzeInst(alloc, scope, inst.data.b);
            break :blk try self.emitTyped(alloc, .if_simple, .{
                .a = condition,
                .b = body,
            }, .void);
        },
        .if_else => blk: {
            var condition = try self.valueOrTrap(alloc, try self.analyzeInst(alloc, scope, inst.data.a));
            const condition_type = self.inst_types.items[@intFromEnum(condition)];
            if (condition_type != .err and condition_type != .bool)
                condition = try self.emitTrap(alloc, .sema_type_mismatch);

            const hir_info = self.hir.unpackExtraData(HIR.IfElseInfo, inst.data.b);
            const body = try self.analyzeInst(alloc, scope, hir_info.body);
            const else_node = try self.analyzeInst(alloc, scope, hir_info.else_node);
            const branch_ref = try self.mir.emitBranchInfo(alloc, .{
                .body = body,
                .else_node = else_node,
            });
            break :blk try self.emitTyped(alloc, .if_else, .{
                .a = condition,
                .b = MIR.asBaseRef(branch_ref),
            }, .void);
        },
        .not, .negate => blk: {
            const operand = try self.valueOrTrap(alloc, try self.analyzeInst(alloc, scope, inst.data.a));
            const operand_type = self.inst_types.items[@intFromEnum(operand)];
            if (operand_type == .err) break :blk operand;
            const mir_tag: MIR.Inst.Tag = switch (inst.tag) {
                .not => tag: {
                    if (operand_type != .bool) break :blk try self.emitTrap(alloc, .sema_type_mismatch);
                    break :tag .not;
                },
                .negate => tag: {
                    if (!isNumericType(operand_type)) break :blk try self.emitTrap(alloc, .sema_type_mismatch);
                    break :tag .negate;
                },
                else => unreachable,
            };
            break :blk try self.emitTyped(alloc, mir_tag, .{ .a = operand }, operand_type);
        },
        .param => try self.analyzeParam(alloc, scope, ref),
        .str_literal => try self.emitTrap(alloc, .sema_unsupported_string_literal),
    };

    self.ref_map.items[@intFromEnum(ref)] = mir_ref;
    self.analyzed.set(@intFromEnum(ref));
    return mir_ref;
}

fn analyzeLocalOrPredeclaredDecl(self: *Self, alloc: mem.Allocator, scope: *Scope, ref: HIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    const inst = self.hir.insts.get(@intFromEnum(ref));
    const name: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));

    if (scope.localBinding(name)) |binding| {
        switch (binding.*) {
            .pending_decl, .analyzing_decl, .value => return self.analyzeDecl(alloc, scope, ref),
            .namespace => return self.emitTrap(alloc, .sema_namespace_not_value),
        }
    }

    if (!try self.bind(scope, alloc, name, .{ .analyzing_decl = ref }))
        return self.emitTrap(alloc, .sema_duplicate_declaration);
    const mir_ref = switch (inst.tag) {
        .decl_const, .decl_var => try self.analyzeValueDecl(alloc, scope, ref),
        .decl_func => try self.analyzeFuncDecl(alloc, scope, ref),
        else => unreachable,
    };
    const binding = scope.localBinding(name) orelse return error.InternalMissingDeclarationBinding;
    binding.* = .{ .value = mir_ref };
    return mir_ref;
}

fn bind(self: *Self, scope: *Scope, alloc: mem.Allocator, name: StringPool.ID, binding: Binding) anyerror!bool {
    if (self.type_map.contains(name)) {
        _ = try self.addDiagnostic(.sema_duplicate_declaration);
        return false;
    }
    if (scope.bindings.contains(name)) {
        _ = try self.addDiagnostic(.sema_duplicate_declaration);
        return false;
    }

    var ancestor = scope.parent;
    while (ancestor) |parent| {
        if (parent.bindings.contains(name)) {
            _ = try self.addDiagnostic(.sema_duplicate_declaration);
            return false;
        }
        ancestor = parent.parent;
    }

    try scope.bindings.put(alloc, name, binding);
    return true;
}

fn resolveValue(self: *Self, alloc: mem.Allocator, scope: *Scope, name: StringPool.ID) anyerror!MIR.Inst.Ref {
    if (self.type_map.contains(name)) return self.emitTrap(alloc, .sema_type_not_value);

    const resolved = self.resolveBinding(scope, name) orelse return self.emitTrap(alloc, .sema_undefined_name);
    return switch (resolved.binding.*) {
        .value => |mir_ref| mir_ref,
        .pending_decl => |ref| try self.analyzeDecl(alloc, resolved.scope, ref),
        .analyzing_decl => try self.emitTrap(alloc, .sema_declaration_cycle),
        .namespace => try self.emitTrap(alloc, .sema_namespace_not_value),
    };
}

fn resolveBinding(_: *Self, scope: *Scope, name: StringPool.ID) ?ResolvedBinding {
    var current: ?*Scope = scope;
    while (current) |s| {
        if (s.localBinding(name)) |binding| return .{ .scope = s, .binding = binding };
        current = s.parent;
    }
    return null;
}

fn resolveType(self: *Self, hir_type_ref: BaseRef) anyerror!TypeResolution {
    if (hir_type_ref == .none) return .{ .type = .void };

    const hir_inst = self.hir.insts.get(@intFromEnum(hir_type_ref));
    if (hir_inst.tag == .trap) return .{
        .type = .err,
        .diagnostic = @enumFromInt(@intFromEnum(hir_inst.data.a)),
    };
    if (hir_inst.tag != .ref) {
        const diagnostic = try self.addDiagnostic(.sema_undefined_type);
        return .{ .type = .err, .diagnostic = diagnostic };
    }

    const name: StringPool.ID = @enumFromInt(@intFromEnum(hir_inst.data.a));
    if (self.type_map.get(name)) |resolved_type|
        return .{ .type = resolved_type };

    const diagnostic = try self.addDiagnostic(.sema_undefined_type);
    return .{ .type = .err, .diagnostic = diagnostic };
}

fn valueOrTrap(self: *Self, alloc: mem.Allocator, ref: MIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    if (ref == .none) return self.emitTrap(alloc, .sema_not_a_value);
    return ref;
}

fn checkReturnType(self: *Self, alloc: mem.Allocator, expected: MIR.Inst.Type, val: MIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    if (expected == .void) {
        if (val != .none) {
            const val_type = self.inst_types.items[@intFromEnum(val)];
            if (val_type != .err) return self.emitTrap(alloc, .sema_type_mismatch);
        }
        return val;
    }

    if (expected == .err) return val;
    if (val == .none) return self.emitTrap(alloc, .sema_type_mismatch);
    const val_type = self.inst_types.items[@intFromEnum(val)];
    if (expected != .err and val_type != .err and val_type != expected)
        return self.emitTrap(alloc, .sema_type_mismatch);
    return val;
}

fn isNumericType(@"type": MIR.Inst.Type) bool {
    return switch (@"type") {
        .i32, .f32 => true,
        .bool, .void, .err => false,
    };
}

fn emitTyped(self: *Self, alloc: mem.Allocator, tag: MIR.Inst.Tag, data: MIR.Inst.Data, resolved_type: MIR.Inst.Type) anyerror!MIR.Inst.Ref {
    const ref = try self.mir.emit(alloc, tag, data);
    try self.inst_types.append(alloc, resolved_type);
    return ref;
}

fn emitTrap(self: *Self, alloc: mem.Allocator, tag: Diagnostic.Tag) anyerror!MIR.Inst.Ref {
    const diag_ref = try self.addDiagnostic(tag);
    return self.emitTrapRef(alloc, diag_ref);
}

fn emitTrapRef(self: *Self, alloc: mem.Allocator, diag_ref: Diagnostic.Ref) anyerror!MIR.Inst.Ref {
    return self.emitTyped(alloc, .trap, .{ .a = @enumFromInt(@intFromEnum(diag_ref)) }, .err);
}

fn addDiagnostic(self: *Self, tag: Diagnostic.Tag) !Diagnostic.Ref {
    return self.diagnostics.add(self.shared_alloc, .{
        .stage = .sema,
        .severity = .err,
        .tag = tag,
        .span = null,
    });
}
