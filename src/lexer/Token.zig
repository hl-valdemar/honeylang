const std = @import("std");
const StringPool = @import("../root.zig").StringPool;

pub const keywords = std.StaticStringMap(Tag).initComptime(.{
    .{ "mut", .mut },
    .{ "func", .func },
    .{ "return", .@"return" },
});

tag: Tag,
start: Idx,
str_id: StringPool.ID,

pub const Idx = @import("../root.zig").BaseIdx;

pub const Tag = enum {
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

    // call-conv tokens
    cc_honey,
    cc_c,

    // special tokens
    newline,
    invalid,
    eof,
};
