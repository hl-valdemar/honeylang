const std = @import("std");
const mem = std.mem;
const ascii = std.ascii;

const token = @import("token.zig");
const err = @import("error.zig");

pos: token.Index,
src: *const @import("../source/Source.zig"),
tokens: token.List,
errors: err.List,

const Self = @This();

pub const ScanResult = struct {
    tokens: token.List,
    errors: err.List,
};

pub fn init(src: *const @import("../source/Source.zig")) Self {
    return .{
        .pos = 0,
        .src = src,
        .tokens = .{},
        .errors = .{},
    };
}

pub fn deinit(self: *Self, gpa: mem.Allocator) void {
    self.tokens.deinit(gpa);
    self.errors.deinit(gpa);
}

pub fn scan(self: *Self, gpa: mem.Allocator) !ScanResult {
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

    return .{
        .tokens = self.tokens,
        .errors = self.errors,
    };
}

fn pushToken(self: *Self, gpa: mem.Allocator, kind: token.Kind, start: token.Index) !void {
    try self.tokens.append(gpa, .{ .kind = kind, .start = start, .end = self.pos });
}

fn pushError(self: *Self, gpa: mem.Allocator, kind: err.Kind, start: token.Index) !void {
    try self.errors.append(gpa, .{ .kind = kind, .start = start, .end = self.pos });
}

fn scanIdent(self: *Self) token.Token {
    const start = self.pos;

    while (self.peek()) |c| {
        if (!ascii.isAlphanumeric(c) and c != '_')
            break;

        self.advance();
    }

    const ident = self.src.contents[start..self.pos];
    const kind: token.Kind = token.keywords.get(ident) orelse .identifier;
    return .{ .kind = kind, .start = start, .end = self.pos };
}

fn scanNum(self: *Self, gpa: mem.Allocator) !token.Token {
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

fn scanHex(self: *Self, gpa: mem.Allocator) !token.Token {
    const start = self.pos;
    self.advanceBy(2); // consume '0' and 'x'

    const digit_start = self.pos;
    while (self.peek()) |c| {
        if (ascii.isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')) {
            self.advance();
        } else break;
    }

    if (self.pos == digit_start)
        try self.errors.append(gpa, .{ .kind = .empty_hex_literal, .start = start, .end = self.pos });

    return .{ .kind = .number, .start = start, .end = self.pos };
}

fn scanBin(self: *Self, gpa: mem.Allocator) !token.Token {
    const start = self.pos;
    self.advanceBy(2); // consume '0' and 'b'

    const digit_start = self.pos;
    while (self.peek()) |c| {
        if (c == '0' or c == '1') {
            self.advance();
        } else break;
    }

    if (self.pos == digit_start)
        try self.errors.append(gpa, .{ .kind = .empty_bin_literal, .start = start, .end = self.pos });

    return .{ .kind = .number, .start = start, .end = self.pos };
}

fn scanDec(self: *Self, gpa: mem.Allocator) !token.Token {
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
                try self.errors.append(gpa, .{ .kind = .multiple_decimal_points, .start = self.pos, .end = self.pos + 1 });
                has_error = true;
            }
            self.advance();
        } else break;
    }

    return .{ .kind = .number, .start = start, .end = self.pos };
}

fn peek(self: *const Self) ?u8 {
    if (self.pos < self.src.contents.len)
        return self.src.contents[self.pos];

    return null;
}

fn peekBy(self: *const Self, n: token.Index) ?u8 {
    if (self.pos + n < self.src.contents.len)
        return self.src.contents[self.pos + n];

    return null;
}

fn advance(self: *Self) void {
    self.pos += 1;
}

fn advanceBy(self: *Self, n: token.Index) void {
    self.pos += n;
}
