const std = @import("std");
const mem = std.mem;

const StringPool = @import("../util/StringPool.zig");
const Source = @import("../source/Source.zig");
const Lexer = @import("../lexer/Lexer.zig");
const Token = @import("../lexer/Token.zig");
const Parser = @import("Parser.zig");

/// helper: parse source code and return ast render.
fn parse(alloc: mem.Allocator, src_str: []const u8) ![]const u8 {
    var src = try Source.init.fromStr(alloc, src_str, 0);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var lexer = Lexer.init(.{ .src = &src, .str_pool = &str_pool });
    defer lexer.deinit(alloc);

    const tokens = try lexer.scan(alloc);

    var parser = Parser.init(.{ .src = &src, .tokens = tokens });
    defer parser.deinit(alloc);

    const ast = try parser.parse(alloc);
    return try ast.render(alloc, src.contents, &str_pool);
}

test "parse const decl" {
    const alloc = std.testing.allocator;
    const src_str =
        \\pi :: 3.14
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "parse var decl" {
    const alloc = std.testing.allocator;
    const src_str =
        \\x = 0xbeaf
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "parse func decl" {
    const alloc = std.testing.allocator;
    const src_str =
        \\main :: func() int {
        \\    return 0xff
        \\}
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "parse arithmetic expr" {
    const alloc = std.testing.allocator;
    const src_str =
        \\x = 1 + 2 * 3
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}
