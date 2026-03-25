const std = @import("std");

const Source = @import("../source/Source.zig");
const Lexer = @import("Lexer.zig");

test "scan const decl" {
    const alloc = std.testing.allocator;

    var src = try Source.init.fromStr(alloc, "pi :: 3.14");
    defer src.deinit(alloc);

    var lexer = Lexer.init(&src);
    defer lexer.deinit(alloc);

    const tokens = try lexer.scan(alloc);

    try std.testing.expectEqual(tokens.tags[0], .identifier);
    try std.testing.expectEqual(tokens.tags[1], .double_colon);
    try std.testing.expectEqual(tokens.tags[2], .number);
}
