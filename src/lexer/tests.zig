const std = @import("std");

const StringPool = @import("../util/StringPool.zig");
const Source = @import("../source/Source.zig");
const Lexer = @import("Lexer.zig");

test "scan const decl" {
    const alloc = std.testing.allocator;

    var src = try Source.init.fromStr(alloc, "pi :: 3.14", 0);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var lexer = Lexer.init(.{ .src = &src, .str_pool = &str_pool });
    defer lexer.deinit(alloc);

    const tokens = try lexer.scan(alloc);

    try std.testing.expectEqual(tokens.items(.tag)[0], .identifier);
    try std.testing.expectEqual(tokens.items(.tag)[1], .double_colon);
    try std.testing.expectEqual(tokens.items(.tag)[2], .number);
}

test "scan arithmetic expression" {
    const alloc = std.testing.allocator;

    var src = try Source.init.fromStr(alloc, "a + 2", 0);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var lexer = Lexer.init(.{ .src = &src, .str_pool = &str_pool });
    defer lexer.deinit(alloc);

    const tokens = try lexer.scan(alloc);

    try std.testing.expectEqual(tokens.items(.tag)[0], .identifier);
    try std.testing.expectEqual(tokens.items(.tag)[1], .plus);
    try std.testing.expectEqual(tokens.items(.tag)[2], .number);
}
