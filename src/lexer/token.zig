const std = @import("std");

const source = @import("../source/source.zig");

pub const Token = struct {
    kind: Kind,
    src_id: source.Id,
    start: source.SourceIndex,
    len: u8, // allows for tokens of 255 characters
};

pub const TokenList = std.ArrayList(Token);

pub const TokenIndex = u32;

pub const Kind = enum {
    // literals
    identifier,
    number,
    bool,

    // keywords
    func,
    struct_,
    defer_,
    return_,
    mut,

    // assignment
    colon,
    double_colon,
    equal,
    plus_equal,
    minus_equal,
    star_equal,
    slash_equal,

    // arithmetic
    plus,
    minus,
    star,
    slash,

    // comparative
    double_equal,
    not_equal,
    less,
    greater,
    less_equal,
    greater_equal,

    // logical
    if_,
    else_,
    not,
    and_,
    or_,
    xor,

    // enclosing
    left_paren,
    right_paren,
    left_curly,
    right_curly,

    // other
    comma,
    dot,
    at,
    eof,
};
