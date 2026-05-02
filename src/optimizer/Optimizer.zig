const std = @import("std");
const mem = std.mem;

str_pool: *StringPool,
mir: *const MIR,
diagnostics: *Diagnostic,
shared_alloc: mem.Allocator,
mir_optimized: MIR,
/// evaluated values keyed by original mir ref.
values: std.ArrayList(Value),
/// rewritten tags keyed by original mir ref.
rewritten_tags: std.ArrayList(MIR.Inst.Tag),
/// rewritten data keyed by original mir ref.
rewritten_data: std.ArrayList(MIR.Inst.Data),

const Self = @This();

const Payload = @import("../root.zig").Payload;
const StringPool = @import("../util/StringPool.zig");
const Diagnostic = @import("../diagnostic/Store.zig");
const MIR = @import("../sema/MIR.zig");

pub const Context = struct {
    str_pool: *StringPool,
    mir: *const MIR,
    shared_alloc: mem.Allocator,
    diagnostics: *Diagnostic,
};

const Value = union(enum) {
    i32: i32,
    f32: f32,
    bool: bool,
    str: StringPool.ID,
    void,
    unknown, // not comptime-known, passthrough
};

pub fn init(ctx: Context) Self {
    return .{
        .str_pool = ctx.str_pool,
        .mir = ctx.mir,
        .diagnostics = ctx.diagnostics,
        .shared_alloc = ctx.shared_alloc,
        .mir_optimized = .{},
        .values = .empty,
        .rewritten_tags = .empty,
        .rewritten_data = .empty,
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.mir_optimized.deinit(alloc);
    self.values.deinit(alloc);
    self.rewritten_tags.deinit(alloc);
    self.rewritten_data.deinit(alloc);
}

pub fn optimize(self: *Self, alloc: mem.Allocator) !void {
    try self.analyzeRewrites(alloc);

    const len = self.mir.insts.len;
    const const_map = try alloc.alloc(Payload, len);
    defer alloc.free(const_map);
    self.computeConstMap(const_map);

    var alive = try std.DynamicBitSetUnmanaged.initEmpty(alloc, len);
    defer alive.deinit(alloc);
    try self.computeAlive(alloc, const_map, &alive);

    try self.materialize(alloc, const_map, &alive);
}

fn analyzeRewrites(self: *Self, alloc: mem.Allocator) !void {
    for (0..self.mir.insts.len) |idx| {
        const inst = self.mir.insts.get(idx);
        var tag = inst.tag;
        var data = inst.data;

        const value: Value = switch (inst.tag) {
            .bool_literal => .{ .bool = @intFromEnum(inst.data.a) != 0 },
            .int_literal => blk: {
                const str_id = inst.data.a.to(StringPool.ID);
                const str = self.str_pool.get(str_id);
                const value = std.fmt.parseInt(i32, str, 0) catch break :blk .unknown;
                break :blk .{ .i32 = value };
            },
            .float_literal => blk: {
                const str_id = inst.data.a.to(StringPool.ID);
                const str = self.str_pool.get(str_id);
                const value = std.fmt.parseFloat(f32, str) catch break :blk .unknown;
                break :blk .{ .f32 = value };
            },
            .trap => .unknown,
            .add, .sub, .mul, .div => blk: {
                const left = self.values.items[@intFromEnum(inst.data.a)];
                const right = self.values.items[@intFromEnum(inst.data.b)];

                if (std.meta.activeTag(left) == .unknown or
                    std.meta.activeTag(right) == .unknown or
                    std.meta.activeTag(left) != std.meta.activeTag(right))
                {
                    break :blk .unknown;
                }

                if (inst.tag == .div and isZero(right)) {
                    tag = .trap;
                    data = try self.trapData(.optimizer_division_by_zero);
                    break :blk .unknown;
                }

                const folded: Value = switch (left) {
                    .i32 => |l| switch (inst.tag) {
                        .add => .{ .i32 = l + right.i32 },
                        .sub => .{ .i32 = l - right.i32 },
                        .mul => .{ .i32 = l * right.i32 },
                        .div => .{ .i32 = @divTrunc(l, right.i32) },
                        else => unreachable,
                    },
                    .f32 => |l| switch (inst.tag) {
                        .add => .{ .f32 = l + right.f32 },
                        .sub => .{ .f32 = l - right.f32 },
                        .mul => .{ .f32 = l * right.f32 },
                        .div => .{ .f32 = l / right.f32 },
                        else => unreachable,
                    },
                    else => unreachable,
                };

                const folded_inst = try self.literalInst(alloc, folded);
                tag = folded_inst.tag;
                data = folded_inst.data;
                break :blk folded;
            },
            .not, .negate => blk: {
                const operand = self.values.items[@intFromEnum(inst.data.a)];
                if (std.meta.activeTag(operand) == .unknown) break :blk .unknown;

                const folded: Value = switch (inst.tag) {
                    .not => .{ .bool = !operand.bool },
                    .negate => switch (operand) {
                        .i32 => |v| .{ .i32 = -v },
                        .f32 => |v| .{ .f32 = -v },
                        else => unreachable,
                    },
                    else => unreachable,
                };

                const folded_inst = try self.literalInst(alloc, folded);
                tag = folded_inst.tag;
                data = folded_inst.data;
                break :blk folded;
            },
            .decl_const => blk: {
                const info = self.mir.declInfo(MIR.asDeclRef(inst.data.b));
                break :blk self.values.items[@intFromEnum(info.value)];
            },
            .decl_var,
            .param,
            .block,
            .decl_func,
            .ret,
            .if_simple,
            .if_else,
            .str_literal,
            .ref,
            => .unknown,
        };

        try self.values.append(alloc, value);
        try self.rewritten_tags.append(alloc, tag);
        try self.rewritten_data.append(alloc, data);
    }
}

fn computeConstMap(self: *Self, const_map: []Payload) void {
    for (const_map, 0..) |*entry, idx| entry.* = @enumFromInt(idx);

    for (0..self.mir.insts.len) |idx| {
        const inst = self.mir.insts.get(idx);
        if (inst.tag != .decl_const) continue;

        const info = self.mir.declInfo(MIR.asDeclRef(inst.data.b));
        const_map[idx] = resolveAlias(info.value, const_map);
    }
}

fn computeAlive(self: *Self, alloc: mem.Allocator, const_map: []const Payload, alive: *std.DynamicBitSetUnmanaged) !void {
    const len = self.mir.insts.len;

    var in_block = try std.DynamicBitSetUnmanaged.initEmpty(alloc, len);
    defer in_block.deinit(alloc);
    for (0..len) |idx| {
        const inst = self.mir.insts.get(idx);
        if (inst.tag == .block) {
            const refs = self.mir.refSlice(inst.data.a, inst.data.b);
            for (refs) |ref| setRef(&in_block, ref);
        }
    }

    for (0..len) |idx| {
        const original_tag = self.mir.insts.items(.tag)[idx];
        const rewritten_tag = self.rewritten_tags.items[idx];
        switch (original_tag) {
            .decl_func, .ret => alive.set(idx),
            .decl_const, .decl_var => if (!in_block.isSet(idx)) alive.set(idx),
            else => if (rewritten_tag == .trap) alive.set(idx),
        }
    }

    var i: usize = len;
    while (i > 0) {
        i -= 1;
        if (!alive.isSet(i)) continue;
        self.markOperandsAlive(i, const_map, alive);
    }
}

fn markOperandsAlive(self: *Self, idx: usize, const_map: []const Payload, alive: *std.DynamicBitSetUnmanaged) void {
    const rewritten_tag = self.rewritten_tags.items[idx];
    const rewritten_data = self.rewritten_data.items[idx];
    const inst = self.mir.insts.get(idx);

    switch (rewritten_tag) {
        .int_literal, .float_literal, .str_literal, .bool_literal, .trap => {},
        .param => {},
        .add, .sub, .mul, .div => {
            setAliasedRef(alive, rewritten_data.a, const_map);
            setAliasedRef(alive, rewritten_data.b, const_map);
        },
        .not, .negate => setAliasedRef(alive, rewritten_data.a, const_map),
        .ref => setRef(alive, rewritten_data.a),
        .decl_const, .decl_var => {
            const info = self.mir.declInfo(MIR.asDeclRef(inst.data.b));
            setAliasedRef(alive, info.value, const_map);
        },
        .ret => {
            if (rewritten_data.a != .none) setAliasedRef(alive, rewritten_data.a, const_map);
        },
        .if_simple => {
            setAliasedRef(alive, rewritten_data.a, const_map);
            setRef(alive, rewritten_data.b);
        },
        .if_else => {
            const info = self.mir.branchInfo(MIR.asBranchRef(inst.data.b));
            setAliasedRef(alive, inst.data.a, const_map);
            setRef(alive, info.body);
            setRef(alive, info.else_node);
        },
        .block => {
            const refs = self.mir.refSlice(inst.data.a, inst.data.b);
            for (refs) |ref| setRef(alive, ref);
        },
        .decl_func => {
            const info = self.mir.funcInfo(MIR.asFuncRef(inst.data.b));
            const params = self.mir.refSlice(info.params_start, info.params_end);
            for (params) |ref| setRef(alive, ref);
            if (info.body != .none) setRef(alive, info.body);
        },
    }
}

fn materialize(self: *Self, alloc: mem.Allocator, const_map: []const Payload, alive: *const std.DynamicBitSetUnmanaged) !void {
    const len = self.mir.insts.len;

    var remap = try alloc.alloc(Payload, len);
    defer alloc.free(remap);
    @memset(remap, .none);

    for (0..len) |idx| {
        if (!alive.isSet(idx)) continue;

        const inst = self.mir.insts.get(idx);
        const rewritten_tag = self.rewritten_tags.items[idx];
        const rewritten_data = self.rewritten_data.items[idx];

        const new_data: MIR.Inst.Data = switch (rewritten_tag) {
            .int_literal, .float_literal, .str_literal, .bool_literal, .trap => rewritten_data,
            .param => rewritten_data,
            .add, .sub, .mul, .div => .{
                .a = remappedAliasedRef(rewritten_data.a, const_map, remap),
                .b = remappedAliasedRef(rewritten_data.b, const_map, remap),
            },
            .not, .negate => .{
                .a = remappedAliasedRef(rewritten_data.a, const_map, remap),
            },
            .ref => rewritten_data,
            .decl_const, .decl_var => blk: {
                const info = self.mir.declInfo(MIR.asDeclRef(inst.data.b));
                const decl_ref = try self.mir_optimized.emitDeclInfo(alloc, .{
                    .value = remappedAliasedRef(info.value, const_map, remap),
                    .type = info.type,
                });
                break :blk .{ .a = inst.data.a, .b = MIR.asPayload(decl_ref) };
            },
            .ret => if (rewritten_data.a != .none) .{
                .a = remappedAliasedRef(rewritten_data.a, const_map, remap),
            } else .{ .a = .none },
            .if_simple => .{
                .a = remappedAliasedRef(rewritten_data.a, const_map, remap),
                .b = remappedRef(rewritten_data.b, remap),
            },
            .if_else => blk: {
                const info = self.mir.branchInfo(MIR.asBranchRef(inst.data.b));
                const branch_ref = try self.mir_optimized.emitBranchInfo(alloc, .{
                    .body = remappedRef(info.body, remap),
                    .else_node = remappedRef(info.else_node, remap),
                });
                break :blk .{
                    .a = remappedAliasedRef(inst.data.a, const_map, remap),
                    .b = MIR.asPayload(branch_ref),
                };
            },
            .block => blk: {
                const refs = self.mir.refSlice(inst.data.a, inst.data.b);
                const refs_start = self.mir_optimized.refListStart();
                for (refs) |ref| {
                    if (!alive.isSet(@intFromEnum(ref))) continue;
                    try self.mir_optimized.appendRef(alloc, remappedRef(ref, remap));
                }
                const refs_end = self.mir_optimized.refListEnd();
                break :blk .{ .a = refs_start, .b = refs_end };
            },
            .decl_func => blk: {
                const info = self.mir.funcInfo(MIR.asFuncRef(inst.data.b));

                const params = self.mir.refSlice(info.params_start, info.params_end);
                const params_start = self.mir_optimized.refListStart();
                for (params) |ref| {
                    try self.mir_optimized.appendRef(alloc, remappedRef(ref, remap));
                }
                const params_end = self.mir_optimized.refListEnd();

                const body = if (info.body != .none)
                    remappedRef(info.body, remap)
                else
                    Payload.none;

                const func_ref = try self.mir_optimized.emitFuncInfo(alloc, .{
                    .params_start = params_start,
                    .params_end = params_end,
                    .body = body,
                    .ret_type = info.ret_type,
                    .flags = info.flags,
                });

                break :blk .{ .a = inst.data.a, .b = MIR.asPayload(func_ref) };
            },
        };

        const new_ref = try self.mir_optimized.emit(alloc, rewritten_tag, new_data);
        remap[idx] = new_ref;
    }
}

const InstRewrite = struct {
    tag: MIR.Inst.Tag,
    data: MIR.Inst.Data,
};

fn literalInst(self: *Self, alloc: mem.Allocator, value: Value) !InstRewrite {
    return switch (value) {
        .bool => |v| .{
            .tag = .bool_literal,
            .data = .{ .a = Payload.fromIndex(if (v) 1 else 0) },
        },
        .i32 => |v| blk: {
            const result_str = try std.fmt.allocPrint(alloc, "{d}", .{v});
            defer alloc.free(result_str);
            const result_str_id = try self.str_pool.intern(self.shared_alloc, result_str);
            break :blk .{
                .tag = .int_literal,
                .data = .{ .a = Payload.from(result_str_id) },
            };
        },
        .f32 => |v| blk: {
            const result_str = try std.fmt.allocPrint(alloc, "{d}", .{v});
            defer alloc.free(result_str);
            const result_str_id = try self.str_pool.intern(self.shared_alloc, result_str);
            break :blk .{
                .tag = .float_literal,
                .data = .{ .a = Payload.from(result_str_id) },
            };
        },
        else => unreachable,
    };
}

fn trapData(self: *Self, tag: Diagnostic.Tag) !MIR.Inst.Data {
    const diag_ref = try self.diagnostics.add(self.shared_alloc, .{
        .stage = .optimizer,
        .severity = .err,
        .tag = tag,
        .span = null,
    });
    return .{ .a = Payload.from(diag_ref) };
}

fn isZero(value: Value) bool {
    return switch (value) {
        .i32 => |v| v == 0,
        .f32 => |v| v == 0,
        else => false,
    };
}

fn resolveAlias(ref: Payload, const_map: []const Payload) Payload {
    if (ref == .none) return .none;

    var current = ref;
    var limit = const_map.len;
    while (limit > 0) : (limit -= 1) {
        const next = const_map[@intFromEnum(current)];
        if (next == current) return current;
        current = next;
        if (current == .none) return .none;
    }

    return current;
}

fn setRef(set: *std.DynamicBitSetUnmanaged, ref: Payload) void {
    if (ref != .none) set.set(@intFromEnum(ref));
}

fn setAliasedRef(set: *std.DynamicBitSetUnmanaged, ref: Payload, const_map: []const Payload) void {
    setRef(set, resolveAlias(ref, const_map));
}

fn remappedRef(ref: Payload, remap: []const Payload) Payload {
    if (ref == .none) return .none;
    return remap[@intFromEnum(ref)];
}

fn remappedAliasedRef(ref: Payload, const_map: []const Payload, remap: []const Payload) Payload {
    return remappedRef(resolveAlias(ref, const_map), remap);
}
