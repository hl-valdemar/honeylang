const std = @import("std");
const mem = std.mem;

str_pool: *const StringPool,
hir: *const HIR,
mir: MIR,
scope: Scope,
/// maps HIR Inst.Ref → MIR Inst.Ref.
ref_map: std.ArrayListUnmanaged(BaseRef),

const Self = @This();

const StringPool = @import("../util/StringPool.zig");
const BaseRef = @import("../root.zig").BaseRef;
const HIR = @import("../parser/HIR.zig");
const MIR = @import("MIR.zig");

const Scope = struct {
    parent: ?*Scope = null,
    bindings: std.StringHashMapUnmanaged(MIR.Inst.Ref) = .{},

    fn deinit(self: *Scope, alloc: mem.Allocator) void {
        self.bindings.deinit(alloc);
        if (self.parent) |p| p.deinit(alloc);
    }

    fn resolve(self: *const Scope, name: []const u8) ?MIR.Inst.Ref {
        if (self.bindings.get(name)) |ref| return ref;
        if (self.parent) |p| return p.resolve(name);
        return null;
    }
};

pub fn init(hir: *const HIR, str_pool: *const StringPool) Self {
    return .{
        .str_pool = str_pool,
        .hir = hir,
        .mir = .{ .str_pool = str_pool },
        .scope = .{},
        .ref_map = .{},
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.mir.deinit(alloc);
    self.scope.deinit(alloc);
    self.ref_map.deinit(alloc);
}

pub fn analyze(self: *Self, alloc: mem.Allocator) !void {
    // seed builtin types
    try self.emitBuiltin(alloc, "i32", .builtin_type, .i32);
    try self.emitBuiltin(alloc, "f32", .builtin_type, .f32);
    try self.emitBuiltin(alloc, "bool", .builtin_type, .bool);
    try self.emitBuiltin(alloc, "void", .builtin_type, .void);

    // aliases
    const i32_ref = self.scope.resolve("i32").?;
    const f32_ref = self.scope.resolve("f32").?;
    try self.scope.bindings.put(alloc, "int", i32_ref); // todo: coalesce to proper anchor type
    try self.scope.bindings.put(alloc, "float", f32_ref); // todo: coalesce to proper anchor type

    // seed true/false builtin values
    try self.scope.bindings.put(alloc, "false", try self.mir.emit(alloc, .builtin_value, .{
        .a = @enumFromInt(1),
    }, .bool));
    try self.scope.bindings.put(alloc, "false", try self.mir.emit(alloc, .builtin_value, .{
        .a = @enumFromInt(0),
    }, .bool));

    // walk hir
    for (0..self.hir.insts.len) |idx| {
        const inst = self.hir.insts.get(idx);
        const types = self.mir.insts.items(.type);
        const mir_ref = switch (inst.tag) {
            .int_literal => try self.mir.emit(alloc, .int_literal, .{
                .a = @enumFromInt(@intFromEnum(inst.data.a)),
            }, .i32),
            .float_literal => try self.mir.emit(alloc, .float_literal, .{
                .a = @enumFromInt(@intFromEnum(inst.data.a)),
            }, .f32),
            .add, .sub, .mul, .div => blk: {
                const left = self.ref_map.items[@intFromEnum(inst.data.a)];
                const right = self.ref_map.items[@intFromEnum(inst.data.b)];

                // type check
                const left_type = types[@intFromEnum(left)];
                const right_type = types[@intFromEnum(right)];
                if (left_type != right_type) return error.TypeMismatch;

                break :blk try self.mir.emit(alloc, @enumFromInt(@intFromEnum(inst.tag)), .{
                    .a = left,
                    .b = right,
                }, left_type);
            },
            .ref => blk: {
                const name = self.str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                break :blk self.scope.resolve(name) orelse return error.UndefinedName;
            },
            .decl_const => blk: {
                const name: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
                const decl_info = self.hir.unpackExtraData(HIR.DeclInfo, inst.data.b);

                const value = self.ref_map.items[@intFromEnum(decl_info.value)];
                const value_type = types[@intFromEnum(value)];

                // resolve type
                const decl_type = if (decl_info.type != .none) t: {
                    const type_ref = self.ref_map.items[@intFromEnum(decl_info.type)];
                    break :t types[@intFromEnum(type_ref)];
                } else value_type;

                // check
                if (decl_info.type != .none and decl_type != value_type)
                    return error.TypeMismatch;

                const decl_ref = try self.mir.emit(alloc, .decl_const, .{
                    .a = @enumFromInt(@intFromEnum(name)),
                    .b = value,
                }, decl_type);

                try self.scope.bindings.put(alloc, self.str_pool.get(name), decl_ref);
                break :blk decl_ref;
            },
            .decl_var => blk: {
                const name: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
                const decl_info = self.hir.unpackExtraData(HIR.DeclInfo, inst.data.b);

                const value = self.ref_map.items[@intFromEnum(decl_info.value)];
                const value_type = types[@intFromEnum(value)];

                // resolve type
                const decl_type = if (decl_info.type != .none) t: {
                    const type_ref = self.ref_map.items[@intFromEnum(decl_info.type)];
                    break :t types[@intFromEnum(type_ref)];
                } else value_type;

                // check
                if (decl_info.type != .none and decl_type != value_type)
                    return error.TypeMismatch;

                const decl_ref = try self.mir.emit(alloc, .decl_var, .{
                    .a = @enumFromInt(@intFromEnum(name)),
                    .b = value,
                }, decl_type);

                try self.scope.bindings.put(alloc, self.str_pool.get(name), decl_ref);
                break :blk decl_ref;
            },
            .param => blk: {
                const name: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
                const type_ref = self.ref_map.items[@intFromEnum(inst.data.b)];
                const param_type = types[@intFromEnum(type_ref)];

                const param_ref = try self.mir.emit(alloc, .param, .{ .a = @enumFromInt(@intFromEnum(name)) }, param_type);
                try self.scope.bindings.put(alloc, self.str_pool.get(name), param_ref);
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

                break :blk try self.mir.emit(alloc, .block, .{
                    .a = extra_start,
                    .b = extra_end,
                }, .void);
            },
            .decl_func => blk: {
                const name: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
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
                const ret_type_ref = self.ref_map.items[@intFromEnum(func_info.ret_type)];
                const ret_type = types[@intFromEnum(ret_type_ref)];

                const extra_idx = try self.mir.packExtraData(alloc, MIR.FuncInfo, .{
                    .params_start = extra_start,
                    .params_end = extra_end,
                    .body = body,
                    .ret_type = ret_type,
                    .flags = func_info.flags,
                });

                const func_ref = try self.mir.emit(alloc, .decl_func, .{
                    .a = @enumFromInt(@intFromEnum(name)),
                    .b = extra_idx,
                }, .void);

                try self.scope.bindings.put(alloc, self.str_pool.get(name), func_ref);
                break :blk func_ref;
            },
            .ret => blk: {
                const val = self.ref_map.items[@intFromEnum(inst.data.a)];
                break :blk try self.mir.emit(alloc, .ret, .{ .a = val }, .void);
            },
            else => {
                std.debug.print("[TODO]: unsupported HIR instruction in sema: {any}\n", .{inst.tag});
                unreachable;
            },
        };
        try self.ref_map.append(alloc, mir_ref); // record HIR idx → MIR ref
    }
}

fn emitBuiltin(self: *Self, alloc: mem.Allocator, name: []const u8, tag: MIR.Inst.Tag, @"type": MIR.Inst.Type) !void {
    const ref = try self.mir.emit(alloc, tag, .{}, @"type");
    try self.scope.bindings.put(alloc, name, ref);
}
