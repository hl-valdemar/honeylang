const std = @import("std");
const mem = std.mem;
const ascii = std.ascii;

src: *const Source,
pos: Token.Index,
tokens: TokenList,
errors: ErrorList,

const Self = @This();

const Source = @import("../source/Source.zig");
const Token = @import("Token.zig");
const Error = @import("Error.zig");

const TokenList = std.MultiArrayList(Token);
const ErrorList = std.MultiArrayList(Error);

pub fn init(src: *const Source) Self {
    return .{
        .src = src,
        .pos = 0,
        .tokens = .{},
        .errors = .{},
    };
}

pub fn deinit(self: *Self, gpa: mem.Allocator) void {
    self.tokens.deinit(gpa);
    self.errors.deinit(gpa);
}

pub fn scan(self: *Self, gpa: mem.Allocator) !void {
    while (self.peek()) |c| {
        // skip whitespace (except for newlines)
        if (ascii.isWhitespace(c) and c != '\n') {
            self.advance();
            continue;
        }

        // skip comments
        if (c == '#') {
            self.advance();
            while (self.peek()) |next| {
                if (next == '\n')
                    break;

                self.advance();
            }
            continue;
        }

        const start = self.pos;
        if (ascii.isAlphabetic(c) or c == '_') { // scan identifiers
            const tok = self.scanIdent();
            try self.tokens.append(gpa, tok);
        } else if (ascii.isDigit(c)) { // scan numbers
            const tok = try self.scanNum(gpa);
            try self.tokens.append(gpa, tok);
        } else switch (c) {
            // single-char tokens
            ',' => {
                self.advance();
                try self.pushToken(gpa, .comma, start);
            },
            '(' => {
                self.advance();
                try self.pushToken(gpa, .left_paren, start);
            },
            ')' => {
                self.advance();
                try self.pushToken(gpa, .right_paren, start);
            },
            '[' => {
                self.advance();
                try self.pushToken(gpa, .left_bracket, start);
            },
            ']' => {
                self.advance();
                try self.pushToken(gpa, .right_bracket, start);
            },
            '{' => {
                self.advance();
                try self.pushToken(gpa, .left_curly, start);
            },
            '}' => {
                self.advance();
                try self.pushToken(gpa, .right_curly, start);
            },
            '=' => {
                self.advance();
                try self.pushToken(gpa, .equal, start);
            },

            // double-char tokens
            ':' => {
                self.advance();
                const next = self.peek();
                if (next != null and next.? == ':') {
                    self.advance();
                    try self.pushToken(gpa, .double_colon, start);
                } else {
                    try self.pushToken(gpa, .colon, start);
                }
            },

            // special tokens
            '\n' => {
                self.advance();
                try self.pushToken(gpa, .newline, start);
            },

            else => {
                // unknown character encountered
                self.advance();
                try self.pushError(gpa, .unrecognized_character, start);
            },
        }
    }

    try self.pushToken(gpa, .eof, self.pos);
}

fn pushToken(self: *Self, gpa: mem.Allocator, tag: Token.Tag, start: Token.Index) !void {
    try self.tokens.append(gpa, .{ .tag = tag, .start = start, .end = self.pos });
}

fn pushError(self: *Self, gpa: mem.Allocator, tag: Error.Tag, start: Token.Index) !void {
    try self.errors.append(gpa, .{ .tag = tag, .start = start, .end = self.pos });
}

fn scanIdent(self: *Self) Token.Token {
    const start = self.pos;

    while (self.peek()) |c| {
        if (!ascii.isAlphanumeric(c) and c != '_')
            break;

        self.advance();
    }

    const ident = self.src.contents[start..self.pos];
    const tag: Token.Tag = Token.keywords.get(ident) orelse .identifier;
    return .{ .tag = tag, .start = start, .end = self.pos };
}

fn scanNum(self: *Self, gpa: mem.Allocator) !Token.Token {
    const c = self.peek();
    if (c != null and c.? == '0') {
        const next = self.peekBy(1);
        if (next != null and next.? == 'x') {
            return try self.scanHex(gpa);
        } else if (next != null and next.? == 'b') {
            return try self.scanBin(gpa);
        }
    }

    return try self.scanDec(gpa);
}

fn scanHex(self: *Self, gpa: mem.Allocator) !Token.Token {
    const start = self.pos;
    self.advanceBy(2); // consume '0' and 'x'

    const digit_start = self.pos;
    while (self.peek()) |c| {
        if (ascii.isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')) {
            self.advance();
        } else break;
    }

    if (self.pos == digit_start)
        try self.errors.append(gpa, .{ .tag = .empty_hex_literal, .start = start, .end = self.pos });

    return .{ .tag = .number, .start = start, .end = self.pos };
}

fn scanBin(self: *Self, gpa: mem.Allocator) !Token.Token {
    const start = self.pos;
    self.advanceBy(2); // consume '0' and 'b'

    const digit_start = self.pos;
    while (self.peek()) |c| {
        if (c == '0' or c == '1') {
            self.advance();
        } else break;
    }

    if (self.pos == digit_start)
        try self.errors.append(gpa, .{ .tag = .empty_bin_literal, .start = start, .end = self.pos });

    return .{ .tag = .number, .start = start, .end = self.pos };
}

fn scanDec(self: *Self, gpa: mem.Allocator) !Token.Token {
    const start = self.pos;

    var has_decimal = false;
    var has_error = false;

    while (self.peek()) |c| {
        if (ascii.isDigit(c)) {
            self.advance();
        } else if (c == '.' and !has_decimal) {
            const next = self.peekBy(1);
            if (next != null and ascii.isDigit(next.?)) {
                has_decimal = true;
                self.advance();
            } else break;
        } else if (c == '.' and has_decimal) {
            if (!has_error) {
                try self.errors.append(gpa, .{ .tag = .multiple_decimal_points, .start = self.pos, .end = self.pos + 1 });
                has_error = true;
            }
            self.advance();
        } else break;
    }

    return .{ .tag = .number, .start = start, .end = self.pos };
}

fn peek(self: *const Self) ?u8 {
    if (self.pos < self.src.contents.len)
        return self.src.contents[self.pos];

    return null;
}

fn peekBy(self: *const Self, n: Token.Index) ?u8 {
    if (self.pos + n < self.src.contents.len)
        return self.src.contents[self.pos + n];

    return null;
}

fn advance(self: *Self) void {
    self.pos += 1;
}

fn advanceBy(self: *Self, n: Token.Index) void {
    self.pos += n;
}
