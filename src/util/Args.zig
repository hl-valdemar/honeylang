const std = @import("std");
const mem = std.mem;

source_files: std.ArrayList([]const u8),
dump_pipeline: bool,

const Self = @This();

pub fn init() Self {
    return Self{
        .source_files = .empty,
        .dump_pipeline = false,
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    for (self.source_files.items) |s| alloc.free(s);
    self.source_files.deinit(alloc);
}

pub fn parse(self: *Self, alloc: mem.Allocator, args: []const [:0]const u8) !void {
    for (args[1..]) |arg| {
        if (mem.eql(u8, arg, "--dump-pipeline")) {
            self.dump_pipeline = true;
        } else if (!mem.startsWith(u8, arg, "-")) {
            try self.source_files.append(alloc, try alloc.dupe(u8, arg));
        }
    }
}

test "parse collects source files" {
    const alloc = std.testing.allocator;
    const argv = [_][:0]const u8{ "honey", "main.hon", "lib.hon" };

    var args = Self.init();
    defer args.deinit(alloc);
    try args.parse(alloc, &argv);

    try std.testing.expect(!args.dump_pipeline);
    try std.testing.expectEqual(@as(usize, 2), args.source_files.items.len);
    try std.testing.expectEqualStrings("main.hon", args.source_files.items[0]);
    try std.testing.expectEqualStrings("lib.hon", args.source_files.items[1]);
}

test "parse dump pipeline flag" {
    const alloc = std.testing.allocator;
    const argv = [_][:0]const u8{ "honey", "--dump-pipeline", "--ignored", "main.hon" };

    var args = Self.init();
    defer args.deinit(alloc);
    try args.parse(alloc, &argv);

    try std.testing.expect(args.dump_pipeline);
    try std.testing.expectEqual(@as(usize, 1), args.source_files.items.len);
    try std.testing.expectEqualStrings("main.hon", args.source_files.items[0]);
}
