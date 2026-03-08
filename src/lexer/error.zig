const std = @import("std");

tag: Tag,
start: Token.Index,
end: Token.Index,

pub const info = std.EnumArray(Tag, @import("../root.zig").ErrorInfo).init(.{
    .unrecognized_character = .{
        .code = "L001",
        .message = "unrecognized character",
        .help = "character doesn't fit any tokens in honeylang",
        .severity = .err,
    },
    .multiple_decimal_points = .{
        .code = "L002",
        .message = "multiple decimal points",
        .help = "remove the last decimal point",
        .severity = .err,
    },
    .empty_hex_literal = .{
        .code = "L003",
        .message = "empty hexadecimal literal",
        .help = "hex notation should be followed by a hexadecimal value",
        .severity = .err,
    },
    .empty_bin_literal = .{
        .code = "L004",
        .message = "empty binary literal",
        .help = "binary notation should be followed by a binary value",
        .severity = .err,
    },
});

const Token = @import("Token.zig");

pub const Tag = enum {
    unrecognized_character,
    multiple_decimal_points,
    empty_hex_literal,
    empty_bin_literal,
};
