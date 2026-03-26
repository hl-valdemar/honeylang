const std = @import("std");
const mem = std.mem;

source_files: std.ArrayListUnmanaged([]const u8),

const Self = @This();

pub fn init() Self {
    return Self{
        .source_files = std.ArrayListUnmanaged([]const u8){},
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    for (self.source_files.items) |s| alloc.free(s);
    self.source_files.deinit(alloc);
}

pub fn parse(self: *Self, alloc: mem.Allocator) !void {
    // get command line arguments
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    // parse arguments
    for (args[1..]) |arg| {
        if (!mem.startsWith(u8, arg, "-")) {
            try self.source_files.append(alloc, try alloc.dupe(u8, arg));
        }
    }
}
