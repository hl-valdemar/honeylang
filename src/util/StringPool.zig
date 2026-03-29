const std = @import("std");
const mem = std.mem;

buffer: std.ArrayListUnmanaged(u8),
entries: std.ArrayListUnmanaged(Entry),
map: std.HashMapUnmanaged(ID, void, MapCtx, std.hash_map.default_max_load_percentage),

const Self = @This();

pub const ID = enum(u32) {
    none = std.math.maxInt(u32),
    _,
};

const Entry = struct {
    start: u32,
    len: u32,
};

/// used for rehashing existing keys (StringID → bytes).
const MapCtx = struct {
    entries: []const Entry,
    buffer: []const u8,

    pub fn hash(ctx: MapCtx, id: ID) u64 {
        const e = ctx.entries[@intFromEnum(id)];
        return std.hash.Wyhash.hash(0, ctx.buffer[e.start..][0..e.len]);
    }

    pub fn eql(ctx: MapCtx, a: ID, b: ID) bool {
        const ea = ctx.entries[@intFromEnum(a)];
        const eb = ctx.entries[@intFromEnum(b)];
        return mem.eql(u8, ctx.buffer[ea.start..][0..ea.len], ctx.buffer[eb.start..][0..eb.len]);
    }
};

//. used for lookups by raw []const u8 against stored StringIDs.
const SliceAdapter = struct {
    entries: []const Entry,
    buffer: []const u8,

    pub fn hash(_: SliceAdapter, key: []const u8) u64 {
        return std.hash.Wyhash.hash(0, key);
    }

    pub fn eql(ctx: SliceAdapter, key: []const u8, id: ID) bool {
        const e = ctx.entries[@intFromEnum(id)];
        return mem.eql(u8, key, ctx.buffer[e.start..][0..e.len]);
    }
};

pub fn init() Self {
    return .{
        .buffer = .{},
        .entries = .{},
        .map = .{},
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.buffer.deinit(alloc);
    self.entries.deinit(alloc);
    self.map.deinit(alloc);
}

pub fn intern(self: *Self, alloc: mem.Allocator, str: []const u8) !ID {
    const adapter = SliceAdapter{ .entries = self.entries.items, .buffer = self.buffer.items };
    const ctx = MapCtx{ .entries = self.entries.items, .buffer = self.buffer.items };

    const gop = try self.map.getOrPutContextAdapted(alloc, str, adapter, ctx);
    if (gop.found_existing) return gop.key_ptr.*;

    const start: u32 = @intCast(self.buffer.items.len);
    const len: u32 = @intCast(str.len);
    try self.buffer.appendSlice(alloc, str);
    try self.entries.append(alloc, .{ .start = start, .len = len });

    const new_id: ID = @enumFromInt(self.entries.items.len - 1);
    gop.key_ptr.* = new_id;

    return new_id;
}

pub fn get(self: *const Self, id: ID) []const u8 {
    const e = self.entries.items[@intFromEnum(id)];
    return self.buffer.items[e.start..][0..e.len];
}

test "intern and retrieve" {
    const alloc = std.testing.allocator;
    var pool = init();
    defer pool.deinit(alloc);

    const id = try pool.intern(alloc, "hello");
    try std.testing.expectEqualStrings("hello", pool.get(id));
}

test "deduplication returns same id" {
    const alloc = std.testing.allocator;
    var pool = init();
    defer pool.deinit(alloc);

    const a = try pool.intern(alloc, "foo");
    const b = try pool.intern(alloc, "foo");
    try std.testing.expectEqual(a, b);
}

test "distinct strings get distinct ids" {
    const alloc = std.testing.allocator;
    var pool = init();
    defer pool.deinit(alloc);

    const a = try pool.intern(alloc, "foo");
    const b = try pool.intern(alloc, "bar");
    try std.testing.expect(a != b);
    try std.testing.expectEqualStrings("foo", pool.get(a));
    try std.testing.expectEqualStrings("bar", pool.get(b));
}

test "survives buffer reallocation" {
    const alloc = std.testing.allocator;
    var pool = init();
    defer pool.deinit(alloc);

    // intern enough strings to force buffer growth
    var ids: [100]ID = undefined;
    for (0..100) |i| {
        var buf: [16]u8 = undefined;
        const name = std.fmt.bufPrint(&buf, "str_{d}", .{i}) catch unreachable;
        ids[i] = try pool.intern(alloc, name);
    }

    // verify all are still retrievable
    for (0..100) |i| {
        var buf: [16]u8 = undefined;
        const expected = std.fmt.bufPrint(&buf, "str_{d}", .{i}) catch unreachable;
        try std.testing.expectEqualStrings(expected, pool.get(ids[i]));
    }
}

test "empty string" {
    const alloc = std.testing.allocator;
    var pool = init();
    defer pool.deinit(alloc);

    const id = try pool.intern(alloc, "");
    try std.testing.expectEqualStrings("", pool.get(id));
}
