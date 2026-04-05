const std = @import("std");
const mem = std.mem;
const ascii = std.ascii;

src: *const Source,
str_pool: *StringPool,
pos: Token.Ref,
tokens: Tokens,
errors: Errors,

const Self = @This();

const StringPool = @import("../root.zig").StringPool;
const Source = @import("../source/Source.zig");
const Token = @import("Token.zig");
const Error = @import("Error.zig");

pub const Tokens = std.MultiArrayList(Token);
const Errors = std.MultiArrayList(Error);

pub const Context = struct {
    src: *const Source,
    str_pool: *StringPool,
};

pub fn init(ctx: Context) Self {
    return .{
        .src = ctx.src,
        .str_pool = ctx.str_pool,
        .pos = 0,
        .tokens = .{},
        .errors = .{},
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.tokens.deinit(alloc);
    self.errors.deinit(alloc);
}

pub fn scan(self: *Self, alloc: mem.Allocator) !Tokens.Slice {
    while (true) {
        const tok = nextToken(self.src.contents, &self.pos);

        if (tok.err) |err_tag| {
            try self.errors.append(alloc, .{ .tag = err_tag, .start = tok.start, .end = tok.end });
        }

        if (tok.tag == .invalid) continue;

        const str_id = str_id: switch (tok.tag) {
            .identifier, .int, .float, .mut, .@"if", .@"else", .@"return", .func => {
                break :str_id try self.str_pool.intern(alloc, self.src.contents[tok.start..tok.end]);
            },
            else => break :str_id StringPool.ID.none,
        };

        try self.tokens.append(alloc, .{ .tag = tok.tag, .start = tok.start, .str_id = str_id });
        if (tok.tag == .eof) break;
    }

    return self.tokens.slice();
}

const ScannedToken = struct {
    tag: Token.Tag,
    start: Token.Ref,
    end: Token.Ref,
    err: ?Error.Tag = null,

    pub fn from(tag: Token.Tag, start: Token.Ref, end: Token.Ref) ScannedToken {
        return .{ .tag = tag, .start = start, .end = end };
    }

    pub fn fromErr(tag: Token.Tag, start: Token.Ref, end: Token.Ref, err: Error.Tag) ScannedToken {
        return .{ .tag = tag, .start = start, .end = end, .err = err };
    }
};

pub fn nextToken(source: []const u8, pos: *Token.Ref) ScannedToken {
    // skip whitespace (except newlines) and comments
    while (pos.* < source.len) {
        const c = source[pos.*];
        if (ascii.isWhitespace(c) and c != '\n') {
            pos.* += 1;
            continue;
        }
        if (c == '#') {
            pos.* += 1;
            while (pos.* < source.len and source[pos.*] != '\n') pos.* += 1;
            continue;
        }
        break;
    }

    if (pos.* >= source.len) return .{ .tag = .eof, .start = pos.*, .end = pos.* };

    const start = pos.*;
    const c = source[pos.*];

    // identifiers and keywords
    if (ascii.isAlphabetic(c) or c == '_') {
        pos.* += 1;
        while (pos.* < source.len and (ascii.isAlphanumeric(source[pos.*]) or source[pos.*] == '_'))
            pos.* += 1;

        const ident = source[start..pos.*];
        const tag: Token.Tag = Token.keywords.get(ident) orelse .identifier;
        return .{ .tag = tag, .start = start, .end = pos.* };
    }

    // numbers
    if (ascii.isDigit(c)) return scanNumber(source, pos);

    // single and double char tokens
    pos.* += 1;
    return switch (c) {
        // arithmetic
        '+' => .from(.plus, start, pos.*),
        '-' => .from(.minus, start, pos.*),
        '*' => .from(.star, start, pos.*),
        '/' => .from(.slash, start, pos.*),

        // paren-type
        '(' => .from(.left_paren, start, pos.*),
        ')' => .from(.right_paren, start, pos.*),
        '[' => .from(.left_bracket, start, pos.*),
        ']' => .from(.right_bracket, start, pos.*),
        '{' => .from(.left_curly, start, pos.*),
        '}' => .from(.right_curly, start, pos.*),

        // other
        ',' => .from(.comma, start, pos.*),
        '=' => .from(.equal, start, pos.*),
        '\n' => .from(.newline, start, pos.*),

        // double-char
        ':' => blk: {
            if (pos.* < source.len and source[pos.*] == ':') {
                pos.* += 1;
                break :blk .from(.double_colon, start, pos.*);
            }
            break :blk .from(.colon, start, pos.*);
        },
        else => .fromErr(.invalid, start, pos.*, .unrecognized_character),
    };
}

fn scanNumber(source: []const u8, pos: *Token.Ref) ScannedToken {
    if (source[pos.*] == '0' and pos.* + 1 < source.len) {
        if (source[pos.* + 1] == 'x') return scanHex(source, pos);
        if (source[pos.* + 1] == 'b') return scanBin(source, pos);
    }
    return scanDec(source, pos);
}

fn scanHex(source: []const u8, pos: *Token.Ref) ScannedToken {
    const start = pos.*;
    pos.* += 2; // skip '0x'

    const digit_start = pos.*;
    while (pos.* < source.len) {
        const c = source[pos.*];
        if (ascii.isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')) {
            pos.* += 1;
        } else break;
    }

    const err: ?Error.Tag = if (pos.* == digit_start) .empty_hex_literal else null;
    return .{ .tag = .int, .start = start, .end = pos.*, .err = err };
}

fn scanBin(source: []const u8, pos: *Token.Ref) ScannedToken {
    const start = pos.*;
    pos.* += 2; // skip '0b'

    const digit_start = pos.*;
    while (pos.* < source.len) {
        const c = source[pos.*];
        if (c == '0' or c == '1') {
            pos.* += 1;
        } else break;
    }

    const err: ?Error.Tag = if (pos.* == digit_start) .empty_bin_literal else null;
    return .{ .tag = .int, .start = start, .end = pos.*, .err = err };
}

fn scanDec(source: []const u8, pos: *Token.Ref) ScannedToken {
    const start = pos.*;
    var has_decimal = false;
    var err: ?Error.Tag = null;

    while (pos.* < source.len) {
        const c = source[pos.*];
        if (ascii.isDigit(c)) {
            pos.* += 1;
        } else if (c == '.' and !has_decimal) {
            if (pos.* + 1 < source.len and ascii.isDigit(source[pos.* + 1])) {
                has_decimal = true;
                pos.* += 1;
            } else break;
        } else if (c == '.' and has_decimal) {
            if (err == null) err = .multiple_decimal_points;
            pos.* += 1;
        } else break;
    }

    const tag: Token.Tag = if (has_decimal) .float else .int;
    return .{ .tag = tag, .start = start, .end = pos.*, .err = err };
}
