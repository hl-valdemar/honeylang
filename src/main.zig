const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const builtin = @import("builtin");
const pretty = @import("lib/pretty.zig");

const honey = @import("honeylang");

pub fn main() !void {
    var gpa = heap.DebugAllocator(.{}).init;
    defer {
        const check = gpa.deinit();
        switch (check) {
            .leak => std.debug.print("\nMemory leaks spotted!\n", .{}),
            .ok => std.debug.print("\nNo memory leaks spotted!\n", .{}),
        }
    }

    const alloc = gpa.allocator();

    // parse arguments
    var parsed = try honey.args.parse(alloc);
    defer parsed.deinit(alloc);

    // validate
    if (parsed.source_files.items.len == 0) {
        std.debug.print("Honey requires at least one source file to compile\n", .{});
        std.process.exit(1);
    }

    var src = try honey.Source.load(alloc, parsed.source_files.items[0]);
    defer src.deload(alloc);

    std.debug.print("{s}\n", .{src.contents});

    var lexer = honey.Lexer.init(&src);
    defer lexer.deinit(alloc);

    const tokens = try lexer.scan(alloc);

    var parser = honey.Parser.init(tokens, &src);
    defer parser.deinit(alloc);

    const ast = try parser.parse(alloc);

    try pretty.print(alloc, ast, .{
        .array_show_item_idx = false,
        .inline_mode = true,
    });

    const root_tag = ast.nodeTag(@enumFromInt(0));
    std.debug.print("\nroot_tag: {any}\n", .{root_tag});

    const top_level_decls_info = ast.nodeData(@enumFromInt(0));
    const top_level_decls_slice = ast.extra_data[top_level_decls_info.a..top_level_decls_info.b];

    std.debug.print("\nnum top-level decls: {d}\n\n", .{top_level_decls_slice.len});

    for (top_level_decls_slice, 0..) |node_idx, i| {
        const tag = ast.nodeTag(@enumFromInt(node_idx));
        std.debug.print("top-level decl {d}: {any}\n", .{ i, tag });
    }
}
