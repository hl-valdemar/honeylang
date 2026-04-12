const std = @import("std");
const mem = std.mem;

str_pool: *StringPool,
mir: *const MIR,
mir_folded: MIR,
mir_dce: MIR,
/// evaluated values.
values: std.ArrayList(Value),
/// maps MIR Inst.Ref (pre) → MIR Inst.Ref (post).
ref_map: std.ArrayList(BaseRef),

const Self = @This();

const BaseRef = @import("../root.zig").BaseRef;
const StringPool = @import("../util/StringPool.zig");
const AST = @import("../parser/AST.zig");
const MIR = @import("../sema/MIR.zig");

pub const Context = struct {
    str_pool: *StringPool,
    mir: *const MIR,
};

const Value = union(enum) {
    i32: i32,
    f32: f32,
    bool: bool,
    str: StringPool.ID,
    void,
    unknown, // not comptime-known, passtrough
};

pub fn init(ctx: Context) Self {
    return .{
        .str_pool = ctx.str_pool,
        .mir = ctx.mir,
        .mir_folded = .{},
        .mir_dce = .{},
        .values = .empty,
        .ref_map = .empty,
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.mir_folded.deinit(alloc);
    self.mir_dce.deinit(alloc);
    self.values.deinit(alloc);
    self.ref_map.deinit(alloc);
}

pub fn optimize(self: *Self, alloc: mem.Allocator) !void {
    try self.foldConsts(alloc);
    try self.propagateConsts(alloc);
    try self.eliminateDeadCode(alloc);
}

fn foldConsts(self: *Self, alloc: mem.Allocator) !void {
    // walk mir
    for (0..self.mir.insts.len) |idx| {
        const inst = self.mir.insts.get(idx);
        const mir_post_ref = switch (inst.tag) {
            .bool_literal => blk: {
                try self.values.append(alloc, .unknown);
                break :blk try self.mir_folded.emit(alloc, inst.tag, inst.data);
            },
            .int_literal => blk: {
                const str_id: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
                const str = self.str_pool.get(str_id);
                try self.values.append(alloc, .{ .i32 = try std.fmt.parseInt(i32, str, 0) });
                break :blk try self.mir_folded.emit(alloc, inst.tag, inst.data);
            },
            .float_literal => blk: {
                const str_id: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
                const str = self.str_pool.get(str_id);
                try self.values.append(alloc, .{ .f32 = try std.fmt.parseFloat(f32, str) });
                break :blk try self.mir_folded.emit(alloc, inst.tag, inst.data);
            },
            .add => blk: {
                const left = self.values.items[@intFromEnum(inst.data.a)];
                const right = self.values.items[@intFromEnum(inst.data.b)];

                if (std.meta.activeTag(left) != .unknown and std.meta.activeTag(right) != .unknown) {
                    const result: Value = switch (left) {
                        .i32 => |l| .{ .i32 = l + right.i32 },
                        .f32 => |l| .{ .f32 = l + right.f32 },
                        else => unreachable,
                    };
                    try self.values.append(alloc, result);

                    const result_str: []u8, const mir_tag: MIR.Inst.Tag =
                        switch (result) {
                            .i32 => |v| .{ try std.fmt.allocPrint(alloc, "{d}", .{v}), .int_literal },
                            .f32 => |v| .{ try std.fmt.allocPrint(alloc, "{d}", .{v}), .float_literal },
                            else => unreachable,
                        };
                    defer alloc.free(result_str);

                    const result_str_id = try self.str_pool.intern(alloc, result_str);
                    break :blk try self.mir_folded.emit(alloc, mir_tag, .{
                        .a = @enumFromInt(@intFromEnum(result_str_id)),
                    });
                } else {
                    // runtime → passthrough
                    try self.values.append(alloc, .unknown);
                    break :blk try self.mir_folded.emit(alloc, .add, .{
                        .a = self.ref_map.items[@intFromEnum(inst.data.a)],
                        .b = self.ref_map.items[@intFromEnum(inst.data.b)],
                    });
                }
            },
            .decl_const => blk: {
                const decl_info = self.mir.unpackExtraData(MIR.DeclInfo, inst.data.b);
                const remapped_val = self.ref_map.items[@intFromEnum(decl_info.value)];
                const val = self.values.items[@intFromEnum(decl_info.value)];
                try self.values.append(alloc, val);

                const extra_idx = try self.mir_folded.packExtraData(alloc, MIR.DeclInfo, .{
                    .value = remapped_val,
                    .type = decl_info.type,
                });
                break :blk try self.mir_folded.emit(alloc, inst.tag, .{
                    .a = inst.data.a,
                    .b = extra_idx,
                });
            },
            .decl_var => blk: {
                const decl_info = self.mir.unpackExtraData(MIR.DeclInfo, inst.data.b);
                const remapped_val = self.ref_map.items[@intFromEnum(decl_info.value)];
                try self.values.append(alloc, .unknown); // var is runtime only

                const extra_idx = try self.mir_folded.packExtraData(alloc, MIR.DeclInfo, .{
                    .value = remapped_val,
                    .type = decl_info.type,
                });
                break :blk try self.mir_folded.emit(alloc, inst.tag, .{
                    .a = inst.data.a,
                    .b = extra_idx,
                });
            },
            .param => blk: {
                try self.values.append(alloc, .unknown);
                break :blk try self.mir_folded.emit(alloc, inst.tag, inst.data);
            },
            .block => blk: {
                const refs = self.mir.extraSlice(inst.data.a, inst.data.b);
                const extra_start: BaseRef = @enumFromInt(self.mir_folded.extra_data.items.len);
                for (refs) |ref| {
                    const remapped = self.ref_map.items[@intFromEnum(ref)];
                    try self.mir_folded.extra_data.append(alloc, remapped);
                }
                const extra_end: BaseRef = @enumFromInt(self.mir_folded.extra_data.items.len);
                try self.values.append(alloc, .unknown);
                break :blk try self.mir_folded.emit(alloc, .block, .{
                    .a = extra_start,
                    .b = extra_end,
                });
            },
            .decl_func => blk: {
                const func_info = self.mir.unpackExtraData(MIR.FuncInfo, inst.data.b);

                // remap params
                const params = self.mir.extraSlice(func_info.params_start, func_info.params_end);
                const extra_start: BaseRef = @enumFromInt(self.mir_folded.extra_data.items.len);
                for (params) |ref| {
                    const remapped = self.ref_map.items[@intFromEnum(ref)];
                    try self.mir_folded.extra_data.append(alloc, remapped);
                }
                const extra_end: BaseRef = @enumFromInt(self.mir_folded.extra_data.items.len);

                // remap body
                const body = self.ref_map.items[@intFromEnum(func_info.body)];

                // repack into extra_data
                const extra_idx = try self.mir_folded.packExtraData(alloc, MIR.FuncInfo, .{
                    .params_start = extra_start,
                    .params_end = extra_end,
                    .body = body,
                    .ret_type = func_info.ret_type,
                    .flags = func_info.flags,
                });

                try self.values.append(alloc, .unknown);
                break :blk try self.mir_folded.emit(alloc, .decl_func, .{
                    .a = inst.data.a,
                    .b = extra_idx,
                });
            },
            .ret => blk: {
                try self.values.append(alloc, .unknown);
                if (inst.data.a != .none) {
                    const remapped = self.ref_map.items[@intFromEnum(inst.data.a)];
                    break :blk try self.mir_folded.emit(alloc, .ret, .{ .a = remapped });
                } else {
                    break :blk try self.mir_folded.emit(alloc, .ret, .{ .a = .none });
                }
            },
            else => {
                std.debug.print("[TODO]: unsupported MIR instruction in comptime eval: {any}\n", .{inst.tag});
                unreachable;
            },
        };
        try self.ref_map.append(alloc, mir_post_ref); // record MIR ref (pre) → MIR ref (post)
    }
}

fn propagateConsts(self: *Self, alloc: mem.Allocator) !void {
    const mir = &self.mir_folded;

    // decl_const ref → value ref
    var const_map = try alloc.alloc(BaseRef, mir.insts.len);
    defer alloc.free(const_map);

    // default: each inst maps to itself (no substitution)
    for (0..mir.insts.len) |i| {
        const_map[i] = @enumFromInt(i);
    }

    // record const → value mappings
    for (0..mir.insts.len) |idx| {
        const inst = mir.insts.get(idx);
        if (inst.tag == .decl_const) {
            const info = mir.unpackExtraData(MIR.DeclInfo, inst.data.b);
            const_map[idx] = info.value;
        }
    }

    for (0..mir.insts.len) |idx| {
        const tag: MIR.Inst.Tag = mir.insts.items(.tag)[idx];
        const data: MIR.Inst.Data = mir.insts.items(.data)[idx];
        switch (tag) {
            .add, .sub, .mul, .div, .if_simple => {
                mir.insts.items(.data)[idx].a = const_map[@intFromEnum(data.a)];
                mir.insts.items(.data)[idx].b = const_map[@intFromEnum(data.b)];
            },
            .not, .negate, .ref => {
                mir.insts.items(.data)[idx].a = const_map[@intFromEnum(data.a)];
            },
            .decl_const, .decl_var => {
                const decl_info = mir.unpackExtraData(MIR.DeclInfo, data.b);
                mir.insts.items(.data)[idx].b = try mir.packExtraData(alloc, MIR.DeclInfo, .{
                    .value = const_map[@intFromEnum(decl_info.value)],
                    .type = decl_info.type,
                });
            },
            .ret => {
                if (data.a != .none) mir.insts.items(.data)[idx].a = const_map[@intFromEnum(data.a)];
            },
            .block => {},
            .int_literal, .float_literal, .str_literal, .bool_literal => {},
            .param => {},
            .decl_func => {},
        }
    }
}

fn eliminateDeadCode(self: *Self, alloc: mem.Allocator) !void {
    const mir = &self.mir_folded;
    const len = mir.insts.len;

    var alive = try std.DynamicBitSetUnmanaged.initEmpty(alloc, len);
    defer alive.deinit(alloc);

    // identify block-local insts
    var in_block = try std.DynamicBitSetUnmanaged.initEmpty(alloc, len);
    defer in_block.deinit(alloc);
    for (0..len) |idx| {
        const inst = mir.insts.get(idx);
        if (inst.tag == .block) {
            const refs = mir.extraSlice(inst.data.a, inst.data.b);
            for (refs) |ref| in_block.set(@intFromEnum(ref));
        }
    }

    // mark roots: top-level (public) decls
    for (0..len) |idx| {
        const tag = mir.insts.items(.tag)[idx];
        switch (tag) {
            .decl_func, .ret => alive.set(idx),
            .decl_const, .decl_var => if (!in_block.isSet(idx)) alive.set(idx),
            else => {},
        }
    }

    // mark operands of alive insts (backwards catch transitive deps)
    var i: usize = len;
    while (i > 0) {
        i -= 1;
        if (!alive.isSet(i)) continue;

        const inst = mir.insts.get(i);
        switch (inst.tag) {
            .int_literal, .float_literal, .str_literal, .bool_literal => {},
            .param => {},
            .add, .sub, .mul, .div, .if_simple => {
                alive.set(@intFromEnum(inst.data.a));
                alive.set(@intFromEnum(inst.data.b));
            },
            .not, .negate, .ref => {
                alive.set(@intFromEnum(inst.data.a));
            },
            .decl_const, .decl_var => {
                const info = mir.unpackExtraData(MIR.DeclInfo, inst.data.b);
                alive.set(@intFromEnum(info.value));
            },
            .ret => {
                if (inst.data.a != .none) alive.set(@intFromEnum(inst.data.a));
            },
            .block => {},
            .decl_func => {
                const func_info = mir.unpackExtraData(MIR.FuncInfo, inst.data.b);
                const params = mir.extraSlice(func_info.params_start, func_info.params_end);
                for (params) |ref| alive.set(@intFromEnum(ref));
                alive.set(@intFromEnum(func_info.body));
            },
        }
    }

    // emit alive insts (remap refs)
    var remap = try alloc.alloc(BaseRef, len);
    defer alloc.free(remap);
    @memset(remap, .none);

    for (0..len) |idx| {
        if (!alive.isSet(idx)) continue;

        const inst = mir.insts.get(idx);
        const new_data: MIR.Inst.Data = switch (inst.tag) {
            .int_literal, .float_literal, .str_literal, .bool_literal => inst.data,
            .param => inst.data,
            .add, .sub, .mul, .div => .{
                .a = remap[@intFromEnum(inst.data.a)],
                .b = remap[@intFromEnum(inst.data.b)],
            },
            .not, .negate, .ref => .{
                .a = remap[@intFromEnum(inst.data.a)],
            },
            .decl_const, .decl_var => blk: {
                const info = mir.unpackExtraData(MIR.DeclInfo, inst.data.b);
                const extra_idx = try self.mir_dce.packExtraData(alloc, MIR.DeclInfo, .{
                    .value = remap[@intFromEnum(info.value)],
                    .type = info.type,
                });
                break :blk .{ .a = inst.data.a, .b = extra_idx };
            },
            .ret => if (inst.data.a != .none) .{
                .a = remap[@intFromEnum(inst.data.a)],
            } else .{ .a = .none },
            .if_simple => .{
                .a = remap[@intFromEnum(inst.data.a)],
                .b = remap[@intFromEnum(inst.data.b)],
            },
            .block => blk: {
                const refs = mir.extraSlice(inst.data.a, inst.data.b);
                const extra_start: BaseRef = @enumFromInt(self.mir_dce.extra_data.items.len);
                for (refs) |ref| {
                    // skip dead statements
                    if (!alive.isSet(@intFromEnum(ref))) continue;
                    try self.mir_dce.extra_data.append(alloc, remap[@intFromEnum(ref)]);
                }
                const extra_end: BaseRef = @enumFromInt(self.mir_dce.extra_data.items.len);
                break :blk .{ .a = extra_start, .b = extra_end };
            },
            .decl_func => blk: {
                const func_info = mir.unpackExtraData(MIR.FuncInfo, inst.data.b);

                const params = mir.extraSlice(func_info.params_start, func_info.params_end);
                const extra_start: BaseRef = @enumFromInt(self.mir_dce.extra_data.items.len);
                for (params) |ref| {
                    try self.mir_dce.extra_data.append(alloc, remap[@intFromEnum(ref)]);
                }
                const extra_end: BaseRef = @enumFromInt(self.mir_dce.extra_data.items.len);

                const body = remap[@intFromEnum(func_info.body)];

                const extra_idx = try self.mir_dce.packExtraData(alloc, MIR.FuncInfo, .{
                    .params_start = extra_start,
                    .params_end = extra_end,
                    .body = body,
                    .ret_type = func_info.ret_type,
                    .flags = func_info.flags,
                });

                break :blk .{ .a = inst.data.a, .b = extra_idx };
            },
        };

        const new_ref = try self.mir_dce.emit(alloc, inst.tag, new_data);
        remap[idx] = new_ref;
    }
}
