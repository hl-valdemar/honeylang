const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const builtin = @import("builtin");
const honey = @import("honeylang");

pub fn main() !void {
    var gpa = heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();

    const alloc = gpa.allocator();

    // parse arguments
    var args = honey.Args.init();
    defer args.deinit(alloc);
    try args.parse(alloc);

    // validate
    if (args.source_files.items.len == 0) {
        std.debug.print("Honey requires at least one source file to compile\n", .{});
        std.process.exit(1);
    }

    var src = try honey.SourceManager.init.fromFile(alloc, args.source_files.items[0]);
    defer src.deinit(alloc);

    std.debug.print("\n[::Source Code::]\n\n", .{});
    std.debug.print("{s}\n", .{src.contents});

    var str_pool = honey.StringPool.init();
    defer str_pool.deinit(alloc);

    var lexer = honey.Lexer.init(.{ .src = &src, .str_pool = &str_pool });
    defer lexer.deinit(alloc);
    const tokens = try lexer.scan(alloc);

    var parser = honey.Parser.init(.{ .src = &src, .tokens = tokens });
    defer parser.deinit(alloc);

    const ast = try parser.parse(alloc);

    const rendered = try ast.render(alloc, src.contents, &str_pool);
    defer alloc.free(rendered);

    std.debug.print("\n[::Rendered AST::]\n\n", .{});
    std.debug.print("{s}", .{rendered});
}
