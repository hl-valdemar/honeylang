const std = @import("std");
const mem = std.mem;
const ascii = std.ascii;

pos: Index,
src: *const @import("../source/Source.zig"),
tokens: @import("token.zig").Tokens,
errors: @import("error.zig").ScanErrors,

const Self = @This();

pub const Index = @import("../root.zig").BaseIndex;

pub const ScanResult = struct {
    tokens: @import("token.zig").Tokens,
    errors: @import("error.zig").ScanErrors,
};

pub fn init(gpa: mem.Allocator, src: *const @import("../source/Source.zig")) !Self {
    return .{
        .pos = 0,
        .src = src,
        .tokens = try @import("token.zig").Tokens.init(gpa),
        .errors = try @import("error.zig").ScanErrors.init(gpa),
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
        }

        const start = self.pos;
        if (ascii.isAlphabetic(c) or c == '_') { // scan identifiers
            const tok = self.scanIdent();
            try self.tokens.push(gpa, tok.kind, tok.start, tok.end);
        } else if (ascii.isDigit(c)) { // scan numbers
            const tok = try self.scanNum(gpa);
            try self.tokens.push(gpa, tok.kind, tok.start, tok.end);
        } else switch (c) {
            // single-char tokens
            ',' => {
                self.advance();
                try self.tokens.push(gpa, .comma, start, self.pos);
            },
            '(' => {
                self.advance();
                try self.tokens.push(gpa, .left_paren, start, self.pos);
            },
            ')' => {
                self.advance();
                try self.tokens.push(gpa, .right_paren, start, self.pos);
            },
            '[' => {
                self.advance();
                try self.tokens.push(gpa, .left_bracket, start, self.pos);
            },
            ']' => {
                self.advance();
                try self.tokens.push(gpa, .right_bracket, start, self.pos);
            },
            '{' => {
                self.advance();
                try self.tokens.push(gpa, .left_curly, start, self.pos);
            },
            '}' => {
                self.advance();
                try self.tokens.push(gpa, .right_curly, start, self.pos);
            },
            '=' => {
                self.advance();
                try self.tokens.push(gpa, .equal, start, self.pos);
            },

            // double-char tokens
            ':' => {
                self.advance();
                const next = self.peek();
                if (next != null and next.? == ':') {
                    self.advance();
                    try self.tokens.push(gpa, .double_colon, start, self.pos);
                } else {
                    try self.tokens.push(gpa, .colon, start, self.pos);
                }
            },

            // special tokens
            '\n' => {
                self.advance();
                try self.tokens.push(gpa, .newline, start, self.pos);
            },

            else => {
                // unknown character encountered
                self.advance();
                try self.errors.push(gpa, .unrecognized_character, start, self.pos);
            },
        }
    }

    try self.tokens.push(gpa, .eof, self.pos, self.pos);

    return .{
        .tokens = self.tokens,
        .errors = self.errors,
    };
}

fn scanIdent(self: *Self) @import("token.zig").TokenDesc {
    const start = self.pos;

    while (self.peek()) |c| {
        if (!ascii.isAlphanumeric(c) and c != '_')
            break;

        self.advance();
    }

    // handle the eventual keyword
    const ident = self.src.contents[start..self.pos];
    if (@import("token.zig").keywords.get(ident)) |kind|
        return .{
            .kind = kind,
            .start = start,
            .end = self.pos,
        };

    return .{
        .kind = .identifier,
        .start = start,
        .end = self.pos,
    };
}

fn scanNum(self: *Self, gpa: mem.Allocator) !@import("token.zig").TokenDesc {
    // check for hex (0x) or binary (0b) prefix
    const c = self.peek();
    if (c != null and c.? == '0') {
        const next = self.peekBy(1);
        if (next != null and next.? == 'x') {
            return try self.scanHex(gpa);
        } else if (next != null and next.? == 'b') {
            return try self.scanBin(gpa);
        }
    }

    // check for decimal
    return try self.scanDec(gpa);
}

fn scanHex(self: *Self, gpa: mem.Allocator) !@import("token.zig").TokenDesc {
    const start = self.pos;
    self.advanceBy(2); // consume '0' and 'x'

    const digit_start = self.pos;
    while (self.peek()) |c| {
        if (ascii.isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')) {
            self.advance();
        } else break;
    }

    if (self.pos == digit_start)
        try self.errors.push(gpa, .empty_hex_literal, start, self.pos);

    return .{
        .kind = .number,
        .start = start,
        .end = self.pos,
    };
}

fn scanBin(self: *Self, gpa: mem.Allocator) !@import("token.zig").TokenDesc {
    const start = self.pos;
    self.advanceBy(2); // consume '0' and 'b'

    const digit_start = self.pos;
    while (self.peek()) |c| {
        if (c == '0' or c == '1') {
            self.advance();
        } else break;
    }

    if (self.pos == digit_start)
        try self.errors.push(gpa, .empty_bin_literal, start, self.pos);

    return .{
        .kind = .number,
        .start = start,
        .end = self.pos,
    };
}

fn scanDec(self: *Self, gpa: mem.Allocator) !@import("token.zig").TokenDesc {
    const start = self.pos;

    var has_decimal = false;
    var has_error = false;

    while (self.peek()) |c| {
        if (ascii.isDigit(c)) {
            self.advance();
        } else if (c == '.' and !has_decimal) {
            // check if next char is also a digit
            const next = self.peekBy(1);
            if (next != null and ascii.isDigit(next.?)) {
                has_decimal = true;
                self.advance();
            } else break;
        } else if (c == '.' and has_decimal) {
            // multiple decimal points
            if (!has_error) {
                try self.errors.push(gpa, .multiple_decimal_points, self.pos, self.pos + 1);
                has_error = true;
            }
            self.advance();
        } else break;
    }

    return .{
        .kind = .number,
        .start = start,
        .end = self.pos,
    };
}

fn peek(self: *const Self) ?u8 {
    if (self.pos < self.src.contents.len)
        return self.src.contents[self.pos];

    return null;
}

fn peekBy(self: *Self, n: Index) ?u8 {
    if (self.pos + n < self.src.contents.len)
        return self.src.contents[self.pos + n];

    return null;
}

fn advance(self: *Self) void {
    self.pos += 1;
}

fn advanceBy(self: *Self, n: Index) void {
    self.pos += n;
}
