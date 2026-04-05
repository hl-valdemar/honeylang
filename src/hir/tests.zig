const std = @import("std");
const mem = std.mem;

const StringPool = @import("../util/StringPool.zig");
const Source = @import("../source/Source.zig");
const Lexer = @import("../lexer/Lexer.zig");
const Parser = @import("../parser/Parser.zig");
const HIR = @import("HIR.zig");

/// helper: parse source and lower to HIR, return instruction tags.
fn lowerToTags(alloc: mem.Allocator, src_str: []const u8) ![]const HIR.Inst.Tag {
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

    var hir = HIR.init();
    defer hir.deinit(alloc);

    const root: HIR.Inst.Ref = @enumFromInt(0);
    _ = try hir.lower(alloc, root, &ast, &str_pool);

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
