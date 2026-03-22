const std = @import("std");
const mem = std.mem;

pub const ParsedArgs = struct {
    source_files: std.ArrayListUnmanaged([]const u8),

    pub fn init(alloc: mem.Allocator) !ParsedArgs {
        return ParsedArgs{
            .source_files = try std.ArrayListUnmanaged([]const u8).initCapacity(alloc, 10),
        };
    }

    pub fn deinit(self: *ParsedArgs, alloc: mem.Allocator) void {
        for (self.source_files.items) |s| alloc.free(s);
        self.source_files.deinit(alloc);
    }
};

pub fn parse(alloc: mem.Allocator) !ParsedArgs {
    // get command line arguments
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    // parse arguments
    var parsed = try ParsedArgs.init(alloc);
    for (1..args.len) |i| {
        const arg = args[i];
        if (!mem.startsWith(u8, arg, "-")) {
            try parsed.source_files.append(alloc, try alloc.dupe(u8, arg));
        }
    }
    return parsed;
}
