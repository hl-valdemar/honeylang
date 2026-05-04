const std = @import("std");
const mem = std.mem;

str_pool: *StringPool,
root_hir: *const HIR,
module_hirs: ?[]const *const HIR,
root_module: HIR.ModuleRef,
diagnostics: *Diagnostic,
shared_alloc: mem.Allocator,
mir: MIR,
scope: Scope,
type_map: std.AutoHashMapUnmanaged(StringPool.ID, MIR.Inst.Type),
/// resolved type per mir inst (parallel to mir.insts). sema-only.
inst_types: std.ArrayList(MIR.Inst.Type),
module_states: ModuleStates,
namespaces: NamespaceInfos,
current_ret_type: ?MIR.Inst.Type,
current_ref: HirRef,
true_ref: MIR.Inst.Ref,
false_ref: MIR.Inst.Ref,

const Self = @This();

const StringPool = @import("../util/StringPool.zig");
const Payload = @import("../root.zig").Payload;
const Diagnostic = @import("../diagnostic/Store.zig");
const HIR = @import("../parser/HIR.zig");
const MIR = @import("MIR.zig");

const HirRef = struct {
    module: HIR.ModuleRef,
    inst: HIR.Inst.Ref,

    const none: HirRef = .{ .module = .none, .inst = .none };

    fn eql(a: HirRef, b: HirRef) bool {
        return a.module == b.module and a.inst == b.inst;
    }
};

const Binding = union(enum) {
    pending_decl: HirRef,
    analyzing_decl: HirRef,
    value: MIR.Inst.Ref,
    namespace: NamespaceRef,
};

const NamespaceRef = enum(u32) {
    none = std.math.maxInt(u32),
    _,
};

const NamespaceInfos = std.MultiArrayList(NamespaceInfo);

const NamespaceKind = enum {
    lexical,
    module,
};

const NamespaceInfo = struct {
    ref: HirRef,
    scope: *Scope,
    kind: NamespaceKind,
    owns_scope: bool,
};

const ModuleStatus = enum {
    unvisited,
    predeclared,
    analyzing,
    analyzed,
};

const ModuleState = struct {
    ref_map: std.ArrayList(Payload) = .empty,
    analyzed: std.DynamicBitSetUnmanaged = .{},
    root_scope: ?*Scope = null,
    status: ModuleStatus = .unvisited,
};

const ModuleStates = std.MultiArrayList(ModuleState);

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

pub fn init(root_hir: *const HIR, str_pool: *StringPool, diagnostics: *Diagnostic, shared_alloc: mem.Allocator) Self {
    return .{
        .str_pool = str_pool,
        .root_hir = root_hir,
        .module_hirs = null,
        .root_module = Payload.fromIndex(0).to(HIR.ModuleRef),
        .diagnostics = diagnostics,
        .shared_alloc = shared_alloc,
        .mir = .{},
        .scope = .{},
        .type_map = .{},
        .inst_types = .empty,
        .module_states = .empty,
        .namespaces = .{},
        .current_ret_type = null,
        .current_ref = .none,
        .true_ref = .none,
        .false_ref = .none,
    };
}

pub fn initModules(
    module_hirs: []const *const HIR,
    root_module: HIR.ModuleRef,
    str_pool: *StringPool,
    diagnostics: *Diagnostic,
    shared_alloc: mem.Allocator,
) Self {
    return .{
        .str_pool = str_pool,
        .root_hir = module_hirs[@intFromEnum(root_module)],
        .module_hirs = module_hirs,
        .root_module = root_module,
        .diagnostics = diagnostics,
        .shared_alloc = shared_alloc,
        .mir = .{},
        .scope = .{},
        .type_map = .{},
        .inst_types = .empty,
        .module_states = .empty,
        .namespaces = .{},
        .current_ret_type = null,
        .current_ref = .none,
        .true_ref = .none,
        .false_ref = .none,
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.mir.deinit(alloc);
    self.scope.deinit(alloc);
    self.type_map.deinit(alloc);
    self.inst_types.deinit(alloc);
    const root_scopes = self.module_states.items(.root_scope);
    const ref_maps = self.module_states.items(.ref_map);
    const analyzed = self.module_states.items(.analyzed);
    for (0..self.module_states.len) |idx| {
        if (root_scopes[idx]) |root_scope| {
            if (@intFromPtr(root_scope) != @intFromPtr(&self.scope)) {
                root_scope.deinit(alloc);
                alloc.destroy(root_scope);
            }
        }
        ref_maps[idx].deinit(alloc);
        analyzed[idx].deinit(alloc);
    }
    self.module_states.deinit(alloc);
    for (self.namespaces.items(.scope), self.namespaces.items(.owns_scope)) |scope, owns_scope| {
        if (owns_scope) {
            scope.deinit(alloc);
            alloc.destroy(scope);
        }
    }
    self.namespaces.deinit(alloc);
}

pub fn analyze(self: *Self, alloc: mem.Allocator) !void {
    try self.seedBuiltins(alloc);
    try self.initModuleStates(alloc);
    _ = try self.ensureModuleRootScope(alloc, self.root_module);

    const root_decls = self.hir(self.root_module).rootDecls();
    try self.predeclareDecls(alloc, self.root_module, &self.scope, root_decls);
    self.moduleStatus(self.root_module).* = .predeclared;
    try self.analyzeDecls(alloc, self.root_module, &self.scope, root_decls);
    self.moduleStatus(self.root_module).* = .analyzed;
}

fn initModuleStates(self: *Self, alloc: mem.Allocator) !void {
    for (0..self.moduleCount()) |idx| {
        const module_ref: HIR.ModuleRef = Payload.fromIndex(idx).to(HIR.ModuleRef);
        var state: ModuleState = .{};
        const module_hir = self.hir(module_ref);
        try state.ref_map.resize(alloc, module_hir.insts.len);
        @memset(state.ref_map.items, .none);
        state.analyzed = try std.DynamicBitSetUnmanaged.initEmpty(alloc, module_hir.insts.len);
        try self.module_states.append(alloc, state);
    }
}

fn moduleCount(self: *const Self) usize {
    if (self.module_hirs) |module_hirs| return module_hirs.len;
    return 1;
}

fn hir(self: *const Self, module: HIR.ModuleRef) *const HIR {
    if (self.module_hirs) |module_hirs| return module_hirs[@intFromEnum(module)];
    std.debug.assert(module == self.root_module);
    return self.root_hir;
}

fn moduleRefMap(self: *Self, module: HIR.ModuleRef) *std.ArrayList(Payload) {
    return &self.module_states.items(.ref_map)[@intFromEnum(module)];
}

fn moduleAnalyzed(self: *Self, module: HIR.ModuleRef) *std.DynamicBitSetUnmanaged {
    return &self.module_states.items(.analyzed)[@intFromEnum(module)];
}

fn moduleRootScope(self: *Self, module: HIR.ModuleRef) *?*Scope {
    return &self.module_states.items(.root_scope)[@intFromEnum(module)];
}

fn moduleStatus(self: *Self, module: HIR.ModuleRef) *ModuleStatus {
    return &self.module_states.items(.status)[@intFromEnum(module)];
}

fn refMap(self: *Self, module: HIR.ModuleRef) []Payload {
    return self.moduleRefMap(module).items;
}

fn seedBuiltins(self: *Self, alloc: mem.Allocator) !void {
    const int_str_id = try self.str_pool.intern(self.shared_alloc, "int");
    const float_str_id = try self.str_pool.intern(self.shared_alloc, "float");
    const i32_str_id = try self.str_pool.intern(self.shared_alloc, "i32");
    const f32_str_id = try self.str_pool.intern(self.shared_alloc, "f32");
    const bool_str_id = try self.str_pool.intern(self.shared_alloc, "bool");
    const void_str_id = try self.str_pool.intern(self.shared_alloc, "void");

    try self.type_map.put(alloc, i32_str_id, .i32);
    try self.type_map.put(alloc, f32_str_id, .f32);
    try self.type_map.put(alloc, bool_str_id, .bool);
    try self.type_map.put(alloc, void_str_id, .void);
    try self.type_map.put(alloc, int_str_id, .i32);
    try self.type_map.put(alloc, float_str_id, .f32);

    self.true_ref = try self.emitTyped(alloc, .bool_literal, .{ .a = Payload.fromIndex(1) }, .bool);
    self.false_ref = try self.emitTyped(alloc, .bool_literal, .{ .a = Payload.fromIndex(0) }, .bool);
    try self.seedBuiltinValues(alloc, &self.scope);
}

fn seedBuiltinValues(self: *Self, alloc: mem.Allocator, scope: *Scope) !void {
    const true_str_id = try self.str_pool.intern(self.shared_alloc, "true");
    const false_str_id = try self.str_pool.intern(self.shared_alloc, "false");
    try scope.bindings.put(alloc, true_str_id, .{ .value = self.true_ref });
    try scope.bindings.put(alloc, false_str_id, .{ .value = self.false_ref });
}

fn ensureModuleRootScope(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef) !*Scope {
    const root_scope_slot = self.moduleRootScope(module);
    if (root_scope_slot.*) |root_scope| return root_scope;

    if (module == self.root_module) {
        root_scope_slot.* = &self.scope;
        return &self.scope;
    }

    const root_scope = try alloc.create(Scope);
    root_scope.* = .{};
    try self.seedBuiltinValues(alloc, root_scope);
    root_scope_slot.* = root_scope;
    return root_scope;
}

fn ensureModulePredeclared(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef) anyerror!*Scope {
    const root_scope = try self.ensureModuleRootScope(alloc, module);
    const status = self.moduleStatus(module);
    switch (status.*) {
        .unvisited => {
            status.* = .predeclared;
            try self.predeclareDecls(alloc, module, root_scope, self.hir(module).rootDecls());
        },
        .predeclared, .analyzing, .analyzed => {},
    }
    return root_scope;
}

fn analyzeModuleRoot(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef) anyerror!*Scope {
    const root_scope = try self.ensureModulePredeclared(alloc, module);
    const status = self.moduleStatus(module);
    switch (status.*) {
        .analyzed, .analyzing => return root_scope,
        .unvisited => unreachable,
        .predeclared => {
            status.* = .analyzing;
            try self.analyzeDecls(alloc, module, root_scope, self.hir(module).rootDecls());
            status.* = .analyzed;
        },
    }
    return root_scope;
}

fn predeclareDecls(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef, scope: *Scope, refs: []const Payload) anyerror!void {
    for (refs) |ref| {
        const inst = self.hir(module).insts.get(@intFromEnum(ref));
        switch (inst.tag) {
            .decl_const, .decl_var, .decl_func => {
                const name = inst.data.a.to(StringPool.ID);
                if (!try self.bind(scope, alloc, name, .{ .pending_decl = .{ .module = module, .inst = ref } }))
                    _ = try self.emitTrap(alloc, .sema_duplicate_declaration);
            },
            .decl_namespace => {
                const name = inst.data.a.to(StringPool.ID);
                const namespace_ref = try self.createNamespace(alloc, module, scope, ref);
                if (!try self.bind(scope, alloc, name, .{ .namespace = namespace_ref }))
                    _ = try self.emitTrap(alloc, .sema_duplicate_declaration);
            },
            .decl_module_namespace => {
                const name = inst.data.a.to(StringPool.ID);
                const namespace_ref = try self.createModuleNamespace(alloc, inst.data.b.to(HIR.ModuleRef));
                if (!try self.bind(scope, alloc, name, .{ .namespace = namespace_ref }))
                    _ = try self.emitTrap(alloc, .sema_duplicate_declaration);
            },
            .import_decl => _ = try self.emitTrap(alloc, .sema_unexpanded_import),
            else => {},
        }
    }
}

fn analyzeDecls(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef, scope: *Scope, refs: []const Payload) anyerror!void {
    for (refs) |ref| _ = try self.analyzeDecl(alloc, module, scope, ref);
}

fn analyzeDecl(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef, scope: *Scope, ref: HIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    const analyzed = self.moduleAnalyzed(module);
    if (analyzed.isSet(@intFromEnum(ref))) return self.refMap(module)[@intFromEnum(ref)];

    const previous_ref = self.current_ref;
    self.current_ref = .{ .module = module, .inst = ref };
    defer self.current_ref = previous_ref;

    const inst = self.hir(module).insts.get(@intFromEnum(ref));
    const name: StringPool.ID = switch (inst.tag) {
        .decl_const, .decl_var, .decl_func, .decl_namespace, .decl_module_namespace => inst.data.a.to(StringPool.ID),
        else => return self.analyzeInst(alloc, module, scope, ref),
    };

    if (inst.tag == .decl_namespace) {
        try self.analyzeNamespace(alloc, module, scope, ref);
        return .none;
    }
    if (inst.tag == .decl_module_namespace) {
        self.refMap(module)[@intFromEnum(ref)] = .none;
        analyzed.set(@intFromEnum(ref));
        return .none;
    }

    const binding = scope.localBinding(name) orelse return self.emitTrap(alloc, .sema_duplicate_declaration);
    switch (binding.*) {
        .value => |mir_ref| {
            self.refMap(module)[@intFromEnum(ref)] = mir_ref;
            analyzed.set(@intFromEnum(ref));
            return mir_ref;
        },
        .pending_decl => |pending_ref| {
            if (!pending_ref.eql(.{ .module = module, .inst = ref })) return self.emitTrap(alloc, .sema_duplicate_declaration);
        },
        .analyzing_decl => return self.emitTrap(alloc, .sema_declaration_cycle),
        .namespace => return self.emitTrap(alloc, .sema_namespace_not_value),
    }

    binding.* = .{ .analyzing_decl = .{ .module = module, .inst = ref } };
    const mir_ref = switch (inst.tag) {
        .decl_const, .decl_var => try self.analyzeValueDecl(alloc, module, scope, ref),
        .decl_func => try self.analyzeFuncDecl(alloc, module, scope, ref),
        else => unreachable,
    };
    binding.* = .{ .value = mir_ref };
    self.refMap(module)[@intFromEnum(ref)] = mir_ref;
    analyzed.set(@intFromEnum(ref));
    return mir_ref;
}

fn analyzeNamespace(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef, parent: *Scope, ref: HIR.Inst.Ref) anyerror!void {
    const analyzed = self.moduleAnalyzed(module);
    if (analyzed.isSet(@intFromEnum(ref))) return;

    const inst = self.hir(module).insts.get(@intFromEnum(ref));
    std.debug.assert(inst.tag == .decl_namespace);

    const name = inst.data.a.to(StringPool.ID);
    const namespace_scope = if (parent.localBinding(name)) |binding| switch (binding.*) {
        .namespace => |namespace| self.namespaceScope(namespace),
        else => return,
    } else self.namespaceScope(try self.createNamespace(alloc, module, parent, ref));

    const block = self.hir(module).insts.get(@intFromEnum(inst.data.b));
    std.debug.assert(block.tag == .block);
    const members = self.hir(module).refSlice(block.data.a, block.data.b);

    self.refMap(module)[@intFromEnum(ref)] = .none;
    analyzed.set(@intFromEnum(ref));

    try self.predeclareDecls(alloc, module, namespace_scope, members);
    try self.analyzeDecls(alloc, module, namespace_scope, members);
}

fn createNamespace(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef, parent: *Scope, ref: HIR.Inst.Ref) !NamespaceRef {
    const scope = try alloc.create(Scope);
    scope.* = .{ .parent = parent };
    const namespace_ref: NamespaceRef = Payload.fromIndex(self.namespaces.len).to(NamespaceRef);
    try self.namespaces.append(alloc, .{
        .ref = .{ .module = module, .inst = ref },
        .scope = scope,
        .kind = .lexical,
        .owns_scope = true,
    });
    return namespace_ref;
}

fn createModuleNamespace(self: *Self, alloc: mem.Allocator, target_module: HIR.ModuleRef) !NamespaceRef {
    const scope = try self.ensureModuleRootScope(alloc, target_module);
    const namespace_ref: NamespaceRef = Payload.fromIndex(self.namespaces.len).to(NamespaceRef);
    try self.namespaces.append(alloc, .{
        .ref = .{ .module = target_module, .inst = .none },
        .scope = scope,
        .kind = .module,
        .owns_scope = false,
    });
    return namespace_ref;
}

fn namespaceScope(self: *Self, namespace: NamespaceRef) *Scope {
    return self.namespaces.items(.scope)[@intFromEnum(namespace)];
}

fn namespaceInfo(self: *Self, namespace: NamespaceRef) NamespaceInfo {
    return self.namespaces.get(@intFromEnum(namespace));
}

fn analyzeValueDecl(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef, scope: *Scope, ref: HIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    const inst = self.hir(module).insts.get(@intFromEnum(ref));
    const hir_decl = self.hir(module).declInfo(HIR.asDeclRef(inst.data.b));

    var value = try self.analyzeInst(alloc, module, scope, hir_decl.value);
    if (value == .none) value = try self.emitTrap(alloc, .sema_not_a_value);

    const value_type = self.inst_types.items[@intFromEnum(value)];
    const type_resolution: TypeResolution = if (hir_decl.type != .none)
        try self.resolveType(module, hir_decl.type)
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
        .b = MIR.asPayload(decl_ref),
    }, decl_type);
}

fn analyzeFuncDecl(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef, parent: *Scope, ref: HIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    const inst = self.hir(module).insts.get(@intFromEnum(ref));
    const func_info = self.hir(module).funcInfo(HIR.asFuncRef(inst.data.b));

    var func_scope: Scope = .{ .parent = parent };
    defer func_scope.deinit(alloc);

    const hir_params = self.hir(module).refSlice(func_info.params_start, func_info.params_end);
    const params_start = self.mir.refListStart();
    for (hir_params) |hir_ref| {
        const mir_ref = try self.analyzeParam(alloc, module, &func_scope, hir_ref);
        try self.mir.appendRef(alloc, mir_ref);
    }
    const params_end = self.mir.refListEnd();

    const ret_type = (try self.resolveType(module, func_info.ret_type)).type;
    const outer_ret_type = self.current_ret_type;
    self.current_ret_type = ret_type;
    defer self.current_ret_type = outer_ret_type;

    const body: MIR.Inst.Ref = if (func_info.body != .none)
        try self.analyzeInst(alloc, module, &func_scope, func_info.body)
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
        .b = MIR.asPayload(func_ref),
    }, .void);
}

fn analyzeParam(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef, scope: *Scope, ref: HIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    const analyzed = self.moduleAnalyzed(module);
    if (analyzed.isSet(@intFromEnum(ref))) return self.refMap(module)[@intFromEnum(ref)];

    const inst = self.hir(module).insts.get(@intFromEnum(ref));
    std.debug.assert(inst.tag == .param);

    const name = inst.data.a.to(StringPool.ID);
    const param_type = (try self.resolveType(module, inst.data.b)).type;
    const param_ref = try self.emitTyped(alloc, .param, .{
        .a = inst.data.a,
        .b = Payload.from(param_type),
    }, param_type);

    if (!try self.bind(scope, alloc, name, .{ .value = param_ref }))
        return self.emitTrap(alloc, .sema_duplicate_declaration);
    self.refMap(module)[@intFromEnum(ref)] = param_ref;
    analyzed.set(@intFromEnum(ref));
    return param_ref;
}

fn analyzeInst(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef, scope: *Scope, ref: HIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    if (ref == .none) return .none;
    const analyzed = self.moduleAnalyzed(module);
    if (analyzed.isSet(@intFromEnum(ref))) return self.refMap(module)[@intFromEnum(ref)];

    const previous_ref = self.current_ref;
    self.current_ref = .{ .module = module, .inst = ref };
    defer self.current_ref = previous_ref;

    const inst = self.hir(module).insts.get(@intFromEnum(ref));
    const mir_ref = switch (inst.tag) {
        .int_literal, .float_literal => blk: {
            const mir_tag: MIR.Inst.Tag, const mir_type: MIR.Inst.Type = switch (inst.tag) {
                .int_literal => .{ .int_literal, .i32 },
                .float_literal => .{ .float_literal, .f32 },
                else => unreachable,
            };
            break :blk try self.emitTyped(alloc, mir_tag, .{
                .a = inst.data.a,
            }, mir_type);
        },
        .trap => try self.emitTyped(alloc, .trap, .{ .a = inst.data.a }, .err),
        .add, .sub, .mul, .div => blk: {
            const left = try self.valueOrTrap(alloc, try self.analyzeInst(alloc, module, scope, inst.data.a));
            const right = try self.valueOrTrap(alloc, try self.analyzeInst(alloc, module, scope, inst.data.b));

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
            const name = inst.data.a.to(StringPool.ID);
            break :blk try self.resolveValue(alloc, scope, name);
        },
        .qualified_ref => try self.resolveQualifiedValue(alloc, module, scope, ref),
        .decl_const, .decl_var, .decl_func => try self.analyzeLocalOrPredeclaredDecl(alloc, module, scope, ref),
        .decl_namespace => blk: {
            try self.analyzeNamespace(alloc, module, scope, ref);
            break :blk .none;
        },
        .decl_module_namespace => .none,
        .import_decl => try self.emitTrap(alloc, .sema_unexpanded_import),
        .block => blk: {
            var block_scope: Scope = .{ .parent = scope };
            defer block_scope.deinit(alloc);

            const hir_stmts = self.hir(module).refSlice(inst.data.a, inst.data.b);
            var stmt_refs: std.ArrayList(Payload) = .empty;
            defer stmt_refs.deinit(alloc);

            for (hir_stmts) |hir_ref| {
                const stmt_ref = try self.analyzeInst(alloc, module, &block_scope, hir_ref);
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
                try self.valueOrTrap(alloc, try self.analyzeInst(alloc, module, scope, inst.data.a))
            else
                .none;
            const ret_val = try self.checkReturnType(alloc, ret_type, val);
            break :blk try self.emitTyped(alloc, .ret, .{ .a = ret_val }, .void);
        },
        .if_simple => blk: {
            var condition = try self.valueOrTrap(alloc, try self.analyzeInst(alloc, module, scope, inst.data.a));
            const condition_type = self.inst_types.items[@intFromEnum(condition)];
            if (condition_type != .err and condition_type != .bool)
                condition = try self.emitTrap(alloc, .sema_type_mismatch);
            const body = try self.analyzeInst(alloc, module, scope, inst.data.b);
            break :blk try self.emitTyped(alloc, .if_simple, .{
                .a = condition,
                .b = body,
            }, .void);
        },
        .if_else => blk: {
            var condition = try self.valueOrTrap(alloc, try self.analyzeInst(alloc, module, scope, inst.data.a));
            const condition_type = self.inst_types.items[@intFromEnum(condition)];
            if (condition_type != .err and condition_type != .bool)
                condition = try self.emitTrap(alloc, .sema_type_mismatch);

            const hir_info = self.hir(module).branchInfo(HIR.asBranchRef(inst.data.b));
            const body = try self.analyzeInst(alloc, module, scope, hir_info.body);
            const else_node = try self.analyzeInst(alloc, module, scope, hir_info.else_node);
            const branch_ref = try self.mir.emitBranchInfo(alloc, .{
                .body = body,
                .else_node = else_node,
            });
            break :blk try self.emitTyped(alloc, .if_else, .{
                .a = condition,
                .b = MIR.asPayload(branch_ref),
            }, .void);
        },
        .not, .negate => blk: {
            const operand = try self.valueOrTrap(alloc, try self.analyzeInst(alloc, module, scope, inst.data.a));
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
        .param => try self.analyzeParam(alloc, module, scope, ref),
        .str_literal => try self.emitTrap(alloc, .sema_unsupported_string_literal),
    };

    self.refMap(module)[@intFromEnum(ref)] = mir_ref;
    analyzed.set(@intFromEnum(ref));
    return mir_ref;
}

fn analyzeLocalOrPredeclaredDecl(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef, scope: *Scope, ref: HIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    const inst = self.hir(module).insts.get(@intFromEnum(ref));
    const name = inst.data.a.to(StringPool.ID);

    if (scope.localBinding(name)) |binding| {
        switch (binding.*) {
            .pending_decl, .analyzing_decl, .value => return self.analyzeDecl(alloc, module, scope, ref),
            .namespace => return self.emitTrap(alloc, .sema_namespace_not_value),
        }
    }

    if (!try self.bind(scope, alloc, name, .{ .analyzing_decl = .{ .module = module, .inst = ref } }))
        return self.emitTrap(alloc, .sema_duplicate_declaration);
    const mir_ref = switch (inst.tag) {
        .decl_const, .decl_var => try self.analyzeValueDecl(alloc, module, scope, ref),
        .decl_func => try self.analyzeFuncDecl(alloc, module, scope, ref),
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
        .pending_decl => |ref| try self.analyzeDecl(alloc, ref.module, resolved.scope, ref.inst),
        .analyzing_decl => try self.emitTrap(alloc, .sema_declaration_cycle),
        .namespace => try self.emitTrap(alloc, .sema_namespace_not_value),
    };
}

fn resolveQualifiedValue(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef, scope: *Scope, ref: HIR.Inst.Ref) anyerror!MIR.Inst.Ref {
    const inst = self.hir(module).insts.get(@intFromEnum(ref));
    std.debug.assert(inst.tag == .qualified_ref);

    const namespace_scope = try self.resolveNamespaceRef(alloc, module, scope, inst.data.a);
    if (namespace_scope == null) return self.emitTrap(alloc, .sema_expected_namespace);
    const name = inst.data.b.to(StringPool.ID);
    const binding = namespace_scope.?.localBinding(name) orelse return self.emitTrap(alloc, .sema_undefined_name);

    return switch (binding.*) {
        .value => |mir_ref| mir_ref,
        .pending_decl => |pending_ref| try self.analyzeDecl(alloc, pending_ref.module, namespace_scope.?, pending_ref.inst),
        .analyzing_decl => try self.emitTrap(alloc, .sema_declaration_cycle),
        .namespace => try self.emitTrap(alloc, .sema_namespace_not_value),
    };
}

fn resolveNamespaceRef(self: *Self, alloc: mem.Allocator, module: HIR.ModuleRef, scope: *Scope, ref: HIR.Inst.Ref) anyerror!?*Scope {
    if (ref == .none) return null;

    const inst = self.hir(module).insts.get(@intFromEnum(ref));
    switch (inst.tag) {
        .ref => {
            const name = inst.data.a.to(StringPool.ID);
            const resolved = self.resolveBinding(scope, name) orelse return null;
            return try self.namespaceScopeFromBinding(alloc, resolved.binding);
        },
        .qualified_ref => {
            const parent_scope = try self.resolveNamespaceRef(alloc, module, scope, inst.data.a) orelse return null;
            const name = inst.data.b.to(StringPool.ID);
            const binding = parent_scope.localBinding(name) orelse return null;
            return try self.namespaceScopeFromBinding(alloc, binding);
        },
        else => return null,
    }
}

fn namespaceScopeFromBinding(self: *Self, alloc: mem.Allocator, binding: *Binding) anyerror!?*Scope {
    return switch (binding.*) {
        .namespace => |namespace| blk: {
            const info = self.namespaceInfo(namespace);
            switch (info.kind) {
                .lexical => try self.analyzeNamespace(alloc, info.ref.module, info.scope.parent orelse try self.ensureModuleRootScope(alloc, info.ref.module), info.ref.inst),
                .module => _ = try self.analyzeModuleRoot(alloc, info.ref.module),
            }
            break :blk info.scope;
        },
        else => null,
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

fn resolveType(self: *Self, module: HIR.ModuleRef, hir_type_ref: Payload) anyerror!TypeResolution {
    if (hir_type_ref == .none) return .{ .type = .void };

    const hir_inst = self.hir(module).insts.get(@intFromEnum(hir_type_ref));
    if (hir_inst.tag == .trap) return .{
        .type = .err,
        .diagnostic = hir_inst.data.a.to(Diagnostic.Ref),
    };
    if (hir_inst.tag != .ref) {
        const diagnostic = try self.addDiagnosticAt(.sema_undefined_type, .{ .module = module, .inst = hir_type_ref });
        return .{ .type = .err, .diagnostic = diagnostic };
    }

    const name = hir_inst.data.a.to(StringPool.ID);
    if (self.type_map.get(name)) |resolved_type|
        return .{ .type = resolved_type };

    const diagnostic = try self.addDiagnosticAt(.sema_undefined_type, .{ .module = module, .inst = hir_type_ref });
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
    return self.emitTyped(alloc, .trap, .{ .a = Payload.from(diag_ref) }, .err);
}

fn addDiagnostic(self: *Self, tag: Diagnostic.Tag) !Diagnostic.Ref {
    return self.addDiagnosticAt(tag, self.current_ref);
}

fn addDiagnosticAt(self: *Self, tag: Diagnostic.Tag, ref: HirRef) !Diagnostic.Ref {
    return self.diagnostics.add(self.shared_alloc, .{
        .stage = .sema,
        .severity = .err,
        .tag = tag,
        .span = if (ref.module != .none and ref.inst != .none) self.hir(ref.module).span(ref.inst) else null,
    });
}
