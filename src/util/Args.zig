const std = @import("std");
const mem = std.mem;

source_files: std.ArrayList([]const u8),

const Self = @This();

pub fn init() Self {
    return Self{
        .source_files = .empty,
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    for (self.source_files.items) |s| alloc.free(s);
    self.source_files.deinit(alloc);
}

pub fn parse(self: *Self, alloc: mem.Allocator, args: []const [:0]const u8) !void {
    for (args[1..]) |arg| {
        if (!mem.startsWith(u8, arg, "-")) {
            try self.source_files.append(alloc, try alloc.dupe(u8, arg));
        }
    }
}
