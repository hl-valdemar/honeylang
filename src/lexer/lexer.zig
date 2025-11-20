const std = @import("std");
const mem = @import("std").mem;
const ascii = @import("std").ascii;

const source = @import("../source/source.zig");

var pos: source.Idx = 0;

pub fn scan(allocator: mem.Allocator, src: *const source.SourceCode) !Tokens {
    var lexer: Lexer = .{ .src = src, .pos = 0 };
    return lexer.scan(allocator);
}

const Lexer = struct {
    src: *const source.SourceCode,
    pos: source.Idx,

    fn scan(self: *Lexer, allocator: mem.Allocator) !Tokens {
        var tokens = try Tokens.init(allocator);

        while (true) {
            const current = self.peek() orelse return tokens;

            if (ascii.isAlphabetic(current)) {
                const ident = self.scan_ident();
                try tokens.append(allocator, ident);
            } else if (ascii.isDigit(current)) {
                const num = self.scan_number();
                try tokens.append(allocator, num);
            }

            switch (current) {
                ':' => {
                    const start = self.pos;

                    self.advance();
                    const next = self.peek();

                    if (next != null and next.? == ':') {
                        self.advance();
                        const end = self.pos;

                        try tokens.append(allocator, .{
                            .kind = .double_colon,
                            .src_range = .{
                                .start = start,
                                .end = end,
                            },
                        });
                    } else {
                        const end = self.pos;

                        try tokens.append(allocator, .{
                            .kind = .colon,
                            .src_range = .{
                                .start = start,
                                .end = end,
                            },
                        });
                    }
                },
                else => self.advance(),
            }
        }

        return tokens;
    }

    fn scan_ident(self: *Lexer) Token {
        const start = self.pos;

        while (self.peek() != null) {
            const next = self.peek() orelse unreachable;
            if (!ascii.isAlphanumeric(next) and next != '_') break;

            // otherwise, consume
            self.advance();
        }
        const end = self.pos;

        // return token information
        return .{
            .kind = .identifier,
            .src_range = source.Range.from(start, end),
        };
    }

    fn scan_number(self: *Lexer) Token {
        var has_decimal = false;

        const start = self.pos;
        while (self.peek() != null) {
            const next = self.peek() orelse unreachable;
            if (!ascii.isDigit(next) and next != '.') break;

            if (next == '.') {
                if (has_decimal) {
                    // number can't have multiple decimal points
                    break;
                } else {
                    has_decimal = true;
                }
            }

            // otherwise, consume
            self.advance();
        }
        const end = self.pos;

        return .{
            .kind = .number,
            .src_range = source.Range.from(start, end),
        };
    }

    fn peek(self: *const Lexer) ?u8 {
        return self.src.get(self.pos);
    }

    fn peek_offset(self: *const Lexer, offset: source.Idx) ?u8 {
        return self.src.get(self.pos + offset);
    }

    fn advance(self: *Lexer) void {
        self.pos += 1;
    }
};

const Token = struct {
    kind: TokenKind,
    src_range: source.Range,
};

pub const Tokens = struct {
    kinds: std.ArrayList(TokenKind),
    src_ranges: std.ArrayList(source.Range),
    len: usize,

    pub fn init(allocator: mem.Allocator) !Tokens {
        return .{
            .kinds = try std.ArrayList(TokenKind).initCapacity(allocator, 10),
            .src_ranges = try std.ArrayList(source.Range).initCapacity(allocator, 10),
            .len = 0,
        };
    }

    pub fn deinit(self: *Tokens, allocator: mem.Allocator) void {
        self.kinds.deinit(allocator);
        self.src_ranges.deinit(allocator);
    }

    pub fn append(self: *Tokens, allocator: mem.Allocator, token: Token) !void {
        std.debug.assert(self.kinds.items.len == self.src_ranges.items.len);
        try self.kinds.append(allocator, token.kind);
        try self.src_ranges.append(allocator, token.src_range);
        self.len += 1;
    }

    pub fn get(self: *const Tokens, idx: source.Idx) ?Token {
        std.debug.assert(self.kinds.items.len == self.src_ranges.items.len);
        if (idx < self.kinds.items.len) {
            const kind = self.kinds.items[idx];
            const range = self.src_ranges.items[idx];

            return .{
                .kind = kind,
                .src_range = range,
            };
        }
        return null;
    }
};

pub const TokenKind = enum {
    // literal
    identifier,
    number,

    // assignment
    colon,
    double_colon,

    // other
    comma,
};

