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

    try lexer.scan(alloc);
    const tokens = lexer.tokens.slice();

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

test "parse simple if statement with grouped condition" {
    const alloc = std.testing.allocator;
    const src_str =
        \\main :: func() void {
        \\    if (true) {
        \\        a + b
        \\    }
        \\}
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "parse simple if statement with non-grouped condition" {
    const alloc = std.testing.allocator;
    const src_str =
        \\main :: func() void {
        \\    if true {
        \\        a + b
        \\    }
        \\}
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

const HIR = @import("HIR.zig");

/// helper: parse source and lower to HIR, return instruction tags.
fn lowerToTags(alloc: mem.Allocator, src_str: []const u8) ![]const HIR.Inst.Tag {
    var src = try Source.init.fromStr(alloc, src_str, 0);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var lexer = Lexer.init(.{ .src = &src, .str_pool = &str_pool });
    defer lexer.deinit(alloc);

    try lexer.scan(alloc);
    const tokens = lexer.tokens.slice();

    var parser = Parser.init(.{ .src = &src, .tokens = tokens });
    defer parser.deinit(alloc);

    const ast = try parser.parse(alloc);

    var hir = HIR.init(.{ .ast = &ast, .str_pool = &str_pool });
    defer hir.deinit(alloc);

    const root: HIR.Inst.Ref = @enumFromInt(0);
    _ = try hir.lower(alloc, root);

    return try alloc.dupe(HIR.Inst.Tag, hir.insts.items(.tag));
}

test "lower const decl" {
    const alloc = std.testing.allocator;
    const tags = try lowerToTags(alloc,
        \\x :: 42
        \\
    );
    defer alloc.free(tags);

    // x :: 42  →  %0 = int(42), %1 = decl_const(x, value=%0)
    try std.testing.expectEqualSlices(HIR.Inst.Tag, &.{
        .int_literal,
        .decl_const,
    }, tags);
}

test "lower arithmetic" {
    const alloc = std.testing.allocator;
    const tags = try lowerToTags(alloc,
        \\x :: 1 + 2
        \\
    );
    defer alloc.free(tags);

    // x :: 1 + 2  →  %0 = int(1), %1 = int(2), %2 = add(%0, %1), %3 = decl_const(x, value=%2)
    try std.testing.expectEqualSlices(HIR.Inst.Tag, &.{
        .int_literal,
        .int_literal,
        .add,
        .decl_const,
    }, tags);
}

test "parse namespace decl" {
    const alloc = std.testing.allocator;
    const src_str =
        \\person {
        \\    x :: 1
        \\}
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "lower namespace decl" {
    const alloc = std.testing.allocator;
    const tags = try lowerToTags(alloc,
        \\person {
        \\    x :: 1
        \\}
        \\
    );
    defer alloc.free(tags);

    try std.testing.expectEqualSlices(HIR.Inst.Tag, &.{
        .int_literal,
        .decl_const,
        .block,
        .decl_namespace,
    }, tags);
}
