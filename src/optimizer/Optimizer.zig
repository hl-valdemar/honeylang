const std = @import("std");
const mem = std.mem;

str_pool: *StringPool,
mir: *const MIR,
mir_folded: MIR,
mir_dce: MIR,
/// evaluated values.
values: std.ArrayListUnmanaged(Value),
/// maps MIR Inst.Ref (pre) → MIR Inst.Ref (post).
ref_map: std.ArrayListUnmanaged(BaseRef),

const Self = @This();

const BaseRef = @import("../root.zig").BaseRef;
const StringPool = @import("../util/StringPool.zig");
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
        .values = .{},
        .ref_map = .{},
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
}

fn foldConsts(self: *Self, alloc: mem.Allocator) !void {
    // walk mir
    for (0..self.mir.insts.len) |idx| {
        const inst = self.mir.insts.get(idx);
        const mir_post_ref = switch (inst.tag) {
            .builtin_type, .builtin_value => blk: {
                try self.values.append(alloc, .unknown);
                break :blk try self.mir_folded.emit(alloc, inst.tag, inst.data, inst.type);
            },
            .int_literal => blk: {
                const str_id: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
                const str = self.str_pool.get(str_id);
                const result: Value = switch (inst.type) {
                    .i32 => .{ .i32 = try std.fmt.parseInt(i32, str, 0) },
                    else => unreachable,
                };
                try self.values.append(alloc, result);
                break :blk try self.mir_folded.emit(alloc, inst.tag, inst.data, inst.type);
            },
            .float_literal => blk: {
                const str_id: StringPool.ID = @enumFromInt(@intFromEnum(inst.data.a));
                const str = self.str_pool.get(str_id);
                const result: Value = switch (inst.type) {
                    .f32 => .{ .f32 = try std.fmt.parseFloat(f32, str) },
                    else => unreachable,
                };
                try self.values.append(alloc, result);
                break :blk try self.mir_folded.emit(alloc, inst.tag, inst.data, inst.type);
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

                    const result_str: []u8, const mir_tag: MIR.Inst.Tag, const mir_type: MIR.Inst.Type =
                        switch (result) {
                            .i32 => |v| blk2: {
                                const result_str = try std.fmt.allocPrint(alloc, "{d}", .{v});
                                break :blk2 .{ result_str, .int_literal, .i32 };
                            },
                            .f32 => |v| blk2: {
                                const result_str = try std.fmt.allocPrint(alloc, "{d}", .{v});
                                break :blk2 .{ result_str, .float_literal, .f32 };
                            },
                            else => unreachable,
                        };
                    defer alloc.free(result_str);

                    const result_str_id = try self.str_pool.intern(alloc, result_str);
                    break :blk try self.mir_folded.emit(alloc, mir_tag, .{
                        .a = @enumFromInt(@intFromEnum(result_str_id)),
                    }, mir_type);
                } else {
                    // runtime → passtrough
                    try self.values.append(alloc, .unknown);
                    break :blk try self.mir_folded.emit(alloc, .add, .{
                        .a = self.ref_map.items[@intFromEnum(inst.data.a)],
                        .b = self.ref_map.items[@intFromEnum(inst.data.b)],
                    }, inst.type);
                }
            },
            .decl_const => blk: {
                const remapped_val_ref = self.ref_map.items[@intFromEnum(inst.data.b)];
                const val = self.values.items[@intFromEnum(inst.data.b)];
                try self.values.append(alloc, val);
                break :blk try self.mir_folded.emit(alloc, inst.tag, .{
                    .a = inst.data.a,
                    .b = remapped_val_ref,
                }, inst.type);
            },
            .decl_var => blk: {
                const remapped_val = self.ref_map.items[@intFromEnum(inst.data.b)];
                try self.values.append(alloc, .unknown); // var is runtime only
                break :blk try self.mir_folded.emit(alloc, inst.tag, .{
                    .a = inst.data.a,
                    .b = remapped_val,
                }, inst.type);
            },
            .param => blk: {
                try self.values.append(alloc, .unknown);
                break :blk try self.mir_folded.emit(alloc, inst.tag, inst.data, inst.type);
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
                }, .void);
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
                    .a = inst.data.a, // func name
                    .b = extra_idx,
                }, inst.type);
            },
            .ret => blk: {
                if (inst.data.a != .none) {
                    const remapped = self.ref_map.items[@intFromEnum(inst.data.a)];
                    const ret_type = self.mir.insts.items(.type)[@intFromEnum(inst.data.a)];
                    try self.values.append(alloc, .unknown);
                    break :blk try self.mir_folded.emit(alloc, .ret, .{ .a = remapped }, ret_type);
                } else {
                    try self.values.append(alloc, .unknown);
                    break :blk try self.mir_folded.emit(alloc, .ret, .{ .a = .none }, .void);
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

// fn eliminateDeadCode(self: *Self, alloc: mem.Allocator) !void {
//     var in_use = try std.DynamicBitSetUnmanaged.initEmpty(alloc, self.mir_folded.insts.len);
//     defer in_use.deinit(alloc);
//     for (0..self.mir_folded.insts.len) |idx| {
//         const inst = self.mir_folded.insts.get(idx);
//         switch (inst.tag) {
//             .add, .sub, .mul, .div => {
//                 const left = @intFromEnum(inst.data.a);
//                 const right = @intFromEnum(inst.data.b);
//                 in_use[left] = true;
//                 in_use[right] = true;
//             },
//             .decl_const, .decl_var => {
//                 const val = @intFromEnum(inst.data.b);
//                 in_use.masks[val] = true;
//             },
//         }
//     }
// }
