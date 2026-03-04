const std = @import("std");
const mem = std.mem;

pub const ParsedArgs = struct {
    source_files: std.ArrayListUnmanaged([]const u8),

    pub fn init(gpa: mem.Allocator) !ParsedArgs {
        return ParsedArgs{
            .source_files = try std.ArrayListUnmanaged([]const u8).initCapacity(gpa, 10),
        };
    }

    pub fn deinit(self: *ParsedArgs, gpa: mem.Allocator) void {
        for (self.source_files.items) |s| gpa.free(s);
        self.source_files.deinit(gpa);
    }
};

pub fn parse(gpa: mem.Allocator) !ParsedArgs {
    // get command line arguments
    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    // parse arguments
    var parsed = try ParsedArgs.init(gpa);
    for (1..args.len) |i| {
        const arg = args[i];
        if (!mem.startsWith(u8, arg, "-")) {
            try parsed.source_files.append(gpa, try gpa.dupe(u8, arg));
        }
    }
    return parsed;
}
