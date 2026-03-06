const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const builtin = @import("builtin");
const pretty = @import("lib/pretty.zig");

const honey = @import("honeylang");

pub fn main() !void {
    var debug_alloc = heap.DebugAllocator(.{}).init;
    defer {
        const check = debug_alloc.deinit();
        switch (check) {
            .leak => std.debug.print("\nMemory leaks spotted!\n", .{}),
            .ok => std.debug.print("\nNo memory leaks spotted!\n", .{}),
        }
    }

    const gpa = debug_alloc.allocator();

    // parse arguments
    var parsed = try honey.args.parse(gpa);
    defer parsed.deinit(gpa);

    // validate
    if (parsed.source_files.items.len == 0) {
        std.debug.print("Honey requires at least one source file to compile\n", .{});
        std.process.exit(1);
    }

    var src = try honey.Source.load(gpa, parsed.source_files.items[0]);
    defer src.deload(gpa);

    std.debug.print("{s}\n", .{src.contents});

    var lexer = honey.Lexer.init(&src);
    defer lexer.deinit(gpa);

    const result = try lexer.scan(gpa);

    std.debug.print("Tokens generated:\n", .{});
    try pretty.print(gpa, result.tokens, .{ .array_show_item_idx = false });

    std.debug.print("\nErrors generated:\n", .{});
    try pretty.print(gpa, result.errors, .{ .array_show_item_idx = false });
}
