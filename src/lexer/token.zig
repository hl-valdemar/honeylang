const std = @import("std");

pub const Index = @import("../root.zig").BaseIndex;

pub const keywords = std.StaticStringMap(Kind).initComptime(.{
    .{ "mut", .mut },
    .{ "func", .func },
    .{ "return", .@"return" },
});

pub const Kind = enum {
    // complex tokens
    identifier,
    number,

    // keywords
    mut,
    func,
    @"return",

    // single-char tokens
    colon,
    comma,
    equal,
    left_paren,
    right_paren,
    left_bracket,
    right_bracket,
    left_curly,
    right_curly,

    // double-char tokens
    double_colon,

    // special tokens
    newline,
    eof,
};

pub const Token = struct {
    kind: Kind,
    start: Index,
    end: Index,
};

pub const List = std.MultiArrayList(Token);
