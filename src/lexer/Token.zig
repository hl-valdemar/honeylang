const std = @import("std");
const StringPool = @import("../root.zig").StringPool;

pub const keywords = std.StaticStringMap(Tag).initComptime(.{
    .{ "mut", .mut },
    .{ "if", .@"if" },
    .{ "else", .@"else" },
    .{ "func", .func },
    .{ "return", .@"return" },
});

tag: Tag,
start: Ref,
str_id: StringPool.ID,

pub const Ref = u32;

pub const Tag = enum {
    // complex tokens
    identifier,
    int,
    float,

    // keywords
    mut,
    @"if",
    @"else",
    @"and",
    @"or",
    func,
    @"return",

    // single-char tokens
    plus,
    minus,
    star,
    slash,
    bang,
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

    // call-conv tokens
    cc_honey,
    cc_c,

    // special tokens
    newline,
    invalid,
    eof,
};
