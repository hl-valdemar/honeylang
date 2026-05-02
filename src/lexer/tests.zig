const std = @import("std");

const StringPool = @import("../util/StringPool.zig");
const Source = @import("../source/Source.zig");
const Diagnostic = @import("../diagnostic/Store.zig");
const Lexer = @import("Lexer.zig");

test "scan const decl" {
    const alloc = std.testing.allocator;

    var src = try Source.init.fromStr(alloc, "pi :: 3.14", 0);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = Diagnostic.init();
    defer diagnostics.deinit(alloc);

    var lexer = Lexer.init(.{ .src = &src, .str_pool = &str_pool, .shared_alloc = alloc, .diagnostics = &diagnostics });
    defer lexer.deinit(alloc);

    try lexer.scan(alloc);
    const tokens = lexer.tokens.slice();

    try std.testing.expectEqual(tokens.items(.tag)[0], .identifier);
    try std.testing.expectEqual(tokens.items(.tag)[1], .double_colon);
    try std.testing.expectEqual(tokens.items(.tag)[2], .float);
}

test "scan arithmetic expression" {
    const alloc = std.testing.allocator;

    var src = try Source.init.fromStr(alloc, "a + 2", 0);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = Diagnostic.init();
    defer diagnostics.deinit(alloc);

    var lexer = Lexer.init(.{ .src = &src, .str_pool = &str_pool, .shared_alloc = alloc, .diagnostics = &diagnostics });
    defer lexer.deinit(alloc);

    try lexer.scan(alloc);
    const tokens = lexer.tokens.slice();

    try std.testing.expectEqual(tokens.items(.tag)[0], .identifier);
    try std.testing.expectEqual(tokens.items(.tag)[1], .plus);
    try std.testing.expectEqual(tokens.items(.tag)[2], .int);
}
