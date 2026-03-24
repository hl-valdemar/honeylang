const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const builtin = @import("builtin");
const pretty = @import("lib/pretty.zig");

const honey = @import("honeylang");

pub fn main() !void {
    var gpa = heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();

    const alloc = gpa.allocator();

    // parse arguments
    var parsed = try honey.args.parse(alloc);
    defer parsed.deinit(alloc);

    // validate
    if (parsed.source_files.items.len == 0) {
        std.debug.print("Honey requires at least one source file to compile\n", .{});
        std.process.exit(1);
    }

    var src = try honey.Source.init.fromFile(alloc, parsed.source_files.items[0]);
    defer src.deinit(alloc);

    std.debug.print("\n[::Source Code::]\n\n", .{});
    std.debug.print("{s}\n", .{src.contents});

    var lexer = honey.Lexer.init(&src);
    defer lexer.deinit(alloc);

    const tokens = try lexer.scan(alloc);

    var parser = honey.Parser.init(tokens, &src);
    defer parser.deinit(alloc);

    const ast = try parser.parse(alloc);

    const rendered = try ast.render(alloc, src.contents);
    defer alloc.free(rendered);

    std.debug.print("\n[::Rendered AST::]\n\n", .{});
    std.debug.print("{s}", .{rendered});
}
