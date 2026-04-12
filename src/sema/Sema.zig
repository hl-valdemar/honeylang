const std = @import("std");
const mem = std.mem;

str_pool: *StringPool,
hir: *const HIR,
mir: MIR,
scope: Scope,
type_map: std.AutoHashMapUnmanaged(StringPool.ID, MIR.Inst.Type),
/// resolved type per MIR inst (parallel to mir.insts). sema-only.
inst_types: std.ArrayListUnmanaged(MIR.Inst.Type),
/// maps HIR Inst.Ref → MIR Inst.Ref.
ref_map: std.ArrayListUnmanaged(BaseRef),

const Self = @This();

const StringPool = @import("../util/StringPool.zig");
const BaseRef = @import("../root.zig").BaseRef;
const HIR = @import("../parser/HIR.zig");
const MIR = @import("MIR.zig");

const Scope = struct {
    parent: ?*Scope = null,
    bindings: std.AutoHashMapUnmanaged(StringPool.ID, MIR.Inst.Ref) = .{},

    fn deinit(self: *Scope, alloc: mem.Allocator) void {
        self.bindings.deinit(alloc);
        if (self.parent) |p| p.deinit(alloc);
    }

    fn bind(self: *Scope, alloc: mem.Allocator, name: StringPool.ID, ref: MIR.Inst.Ref) !void {
        const gop = try self.bindings.getOrPut(alloc, name);
        if (gop.found_existing) return error.DuplicateDeclaration;
        gop.value_ptr.* = ref;
    }

    fn resolve(self: *const Scope, name: StringPool.ID) ?MIR.Inst.Ref {
        if (self.bindings.get(name)) |ref| return ref;
        if (self.parent) |p| return p.resolve(name);
        return null;
    }
};

pub fn init(hir: *const HIR, str_pool: *StringPool) Self {
    return .{
        .str_pool = str_pool,
        .hir = hir,
        .mir = .{},
        .scope = .{},
        .type_map = .{},
        .inst_types = .{},
        .ref_map = .{},
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.mir.deinit(alloc);
    self.scope.deinit(alloc);
    self.type_map.deinit(alloc);
    self.inst_types.deinit(alloc);
    self.ref_map.deinit(alloc);
}

pub fn analyze(self: *Self, alloc: mem.Allocator) !void {
    const int_str_id = try self.str_pool.intern(alloc, "int");
    const float_str_id = try self.str_pool.intern(alloc, "float");
    const i32_str_id = try self.str_pool.intern(alloc, "i32");
    const f32_str_id = try self.str_pool.intern(alloc, "f32");
    const bool_str_id = try self.str_pool.intern(alloc, "bool");
    const void_str_id = try self.str_pool.intern(alloc, "void");

    const true_str_id = try self.str_pool.intern(alloc, "true");
    const false_str_id = try self.str_pool.intern(alloc, "false");

    // seed builtin types
    try self.type_map.put(alloc, i32_str_id, .i32);
    try self.type_map.put(alloc, f32_str_id, .f32);
    try self.type_map.put(alloc, bool_str_id, .bool);
    try self.type_map.put(alloc, void_str_id, .void);
    try self.type_map.put(alloc, int_str_id, .i32);
    try self.type_map.put(alloc, float_str_id, .f32);

    // seed true/false as bool literals
    try self.scope.bindings.put(alloc, true_str_id, try self.emitTyped(alloc, .bool_literal, .{
        .a = @enumFromInt(1),
    }, .bool));
    try self.scope.bindings.put(alloc, false_str_id, try self.emitTyped(alloc, .bool_literal, .{
        .a = @enumFromInt(0),
    }, .bool));

    // walk hir
    for (0..self.hir.insts.len) |idx| {
        const inst = self.hir.insts.get(idx);
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
            .add, .sub, .mul, .div => blk: {
                const left = self.ref_map.items[@intFromEnum(inst.data.a)];
                const right = self.ref_map.items[@intFromEnum(inst.data.b)];

                // type check
                const left_type = self.inst_types.items[@intFromEnum(left)];
                const right_type = self.inst_types.items[@intFromEnum(right)];
                if (left_type != right_type) return error.TypeMismatch;

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
                // type refs don't produce insts
                if (self.type_map.contains(name)) break :blk @as(MIR.Inst.Ref, .none);
                break :blk self.scope.resolve(name) orelse return error.UndefinedName;
            },
            .decl_const, .decl_var => blk: {
                const name: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
                if (self.scope.resolve(name) != null) return error.DuplicateDeclaration;

                const hir_decl = self.hir.unpackExtraData(HIR.DeclInfo, inst.data.b);

                const value = self.ref_map.items[@intFromEnum(hir_decl.value)];
                const value_type = self.inst_types.items[@intFromEnum(value)];

                // resolve type
                const decl_type = if (hir_decl.type != .none) t: {
                    break :t self.resolveType(hir_decl.type) orelse return error.UndefinedType;
                } else value_type;

                // check
                if (hir_decl.type != .none and decl_type != value_type)
                    return error.TypeMismatch;

                const mir_tag: MIR.Inst.Tag = switch (inst.tag) {
                    .decl_const => .decl_const,
                    .decl_var => .decl_var,
                    else => unreachable,
                };
                const extra_idx = try self.mir.packExtraData(alloc, MIR.DeclInfo, .{
                    .value = value,
                    .type = decl_type,
                });
                const decl_ref = try self.emitTyped(alloc, mir_tag, .{
                    .a = @enumFromInt(@intFromEnum(name)),
                    .b = extra_idx,
                }, decl_type);

                try self.scope.bind(alloc, name, decl_ref);
                break :blk decl_ref;
            },
            .param => blk: {
                const name: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
                if (self.scope.resolve(name) != null) return error.DuplicateDeclaration;

                const param_type = self.resolveType(inst.data.b) orelse return error.UndefinedType;

                const param_ref = try self.emitTyped(alloc, .param, .{
                    .a = @enumFromInt(@intFromEnum(name)),
                    .b = @enumFromInt(@intFromEnum(param_type)),
                }, param_type);
                try self.scope.bind(alloc, name, param_ref);
                break :blk param_ref;
            },
            .block => blk: {
                const hir_stmts = self.hir.extraSlice(inst.data.a, inst.data.b);

                const extra_start: BaseRef = @enumFromInt(self.mir.extra_data.items.len);
                for (hir_stmts) |hir_ref| {
                    const mir_ref = self.ref_map.items[@intFromEnum(hir_ref)];
                    try self.mir.extra_data.append(alloc, mir_ref);
                }
                const extra_end: BaseRef = @enumFromInt(self.mir.extra_data.items.len);

                break :blk try self.emitTyped(alloc, .block, .{
                    .a = extra_start,
                    .b = extra_end,
                }, .void);
            },
            .decl_func => blk: {
                const name: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
                if (self.scope.resolve(name) != null) return error.DuplicateDeclaration;

                const func_info = self.hir.unpackExtraData(HIR.FuncInfo, inst.data.b);

                // remap params range
                const hir_params = self.hir.extraSlice(func_info.params_start, func_info.params_end);

                const extra_start: BaseRef = @enumFromInt(self.mir.extra_data.items.len);
                for (hir_params) |hir_ref| {
                    const mir_ref = self.ref_map.items[@intFromEnum(hir_ref)];
                    try self.mir.extra_data.append(alloc, mir_ref);
                }
                const extra_end: BaseRef = @enumFromInt(self.mir.extra_data.items.len);

                const body = self.ref_map.items[@intFromEnum(func_info.body)];

                // resolve return type
                const ret_type = self.resolveType(func_info.ret_type) orelse return error.UndefinedType;

                const extra_idx = try self.mir.packExtraData(alloc, MIR.FuncInfo, .{
                    .params_start = extra_start,
                    .params_end = extra_end,
                    .body = body,
                    .ret_type = ret_type,
                    .flags = func_info.flags,
                });

                const func_ref = try self.emitTyped(alloc, .decl_func, .{
                    .a = @enumFromInt(@intFromEnum(name)),
                    .b = extra_idx,
                }, .void);

                try self.scope.bind(alloc, name, func_ref);
                break :blk func_ref;
            },
            .ret => blk: {
                const val = self.ref_map.items[@intFromEnum(inst.data.a)];
                break :blk try self.emitTyped(alloc, .ret, .{ .a = val }, .void);
            },
            else => {
                std.debug.print("[TODO]: unsupported HIR instruction in sema: {any}\n", .{inst.tag});
                unreachable;
            },
        };
        try self.ref_map.append(alloc, mir_ref); // record HIR idx → MIR ref
    }
}

fn resolveType(self: *const Self, hir_type_ref: BaseRef) ?MIR.Inst.Type {
    if (hir_type_ref == .none) return null;
    const hir_inst = self.hir.insts.get(@intFromEnum(hir_type_ref));
    std.debug.assert(hir_inst.tag == .ref);
    const name: StringPool.ID = @enumFromInt(@intFromEnum(hir_inst.data.a));
    return self.type_map.get(name);
}

fn emitTyped(self: *Self, alloc: mem.Allocator, tag: MIR.Inst.Tag, data: MIR.Inst.Data, resolved_type: MIR.Inst.Type) !MIR.Inst.Ref {
    const ref = try self.mir.emit(alloc, tag, data);
    try self.inst_types.append(alloc, resolved_type);
    return ref;
}
