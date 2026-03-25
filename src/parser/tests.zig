const std = @import("std");
const mem = std.mem;

const Source = @import("../source/Source.zig");
const Lexer = @import("../lexer/Lexer.zig");
const Token = @import("../lexer/Token.zig");
const Parser = @import("Parser.zig");

/// helper: parse source code and return ast render.
fn parse(alloc: mem.Allocator, src_str: []const u8) ![]const u8 {
    var src = try Source.init.fromStr(alloc, src_str);
    defer src.deinit(alloc);

    var lexer = Lexer.init(&src);
    defer lexer.deinit(alloc);

    const tokens = try lexer.scan(alloc);

    var parser = Parser.init(tokens, &src);
    defer parser.deinit(alloc);

    const ast = try parser.parse(alloc);
    return try ast.render(alloc, src.contents);
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
