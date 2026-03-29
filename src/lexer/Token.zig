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
    number,

    // keywords
    mut,
    @"if",
    @"else",
    func,
    @"return",

    // arithmetic tokens
    plus,
    minus,
    times,
    div,

    // logic tokens
    not,
    @"and",
    @"or",

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

    // call-conv tokens
    cc_honey,
    cc_c,

    // special tokens
    newline,
    invalid,
    eof,
};
