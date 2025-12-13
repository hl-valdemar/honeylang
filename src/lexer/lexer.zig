const std = @import("std");
const mem = @import("std").mem;
const ascii = @import("std").ascii;

const SourceIndex = @import("../source/source.zig").SourceIndex;
const SourceCode = @import("../source/source.zig").SourceCode;

const Token = @import("token.zig").Token;
const TokenKind = @import("token.zig").Kind;
const TokenList = @import("token.zig").TokenList;

const keywords = std.StaticStringMap(TokenKind).initComptime(.{
    // declaration-related
    .{ "func", .func },

    // statement-related
    .{ "mut", .mut },
    .{ "defer", .defer_ },
    .{ "return", .return_ },

    // logic-related
    .{ "not", .not },
    .{ "and", .and_ },
    .{ "or", .or_ },
    .{ "xor", .xor },
    .{ "true", .boolean },
    .{ "false", .boolean },
});

pub fn scan(allocator: mem.Allocator, src: *const SourceCode) !TokenList {
    var lexer = Lexer.init(src);
    return lexer.scan(allocator);
}

const Lexer = struct {
    src: *const SourceCode,
    pos: SourceIndex,

    fn init(src: *const SourceCode) Lexer {
        return .{ .src = src, .pos = 0 };
    }

    fn scan(self: *Lexer, allocator: mem.Allocator) !TokenList {
        var tokens = try TokenList.initCapacity(allocator, 10);

        while (self.peek()) |current| {
            // skip whitespace
            if (ascii.isWhitespace(current)) {
                self.advance();
                continue;
            }

            // skip comments
            if (current == '#') {
                while (self.peek()) |c| {
                    self.advance();
                    if (c == '\n') break;
                }
                continue;
            }

            const start = self.pos;

            if (ascii.isAlphabetic(current) or current == '_') {
                try tokens.append(allocator, self.scanIdent());
            } else if (ascii.isDigit(current)) {
                try tokens.append(allocator, self.scanNumber());
            } else switch (current) {
                ',' => {
                    self.advance();
                    try tokens.append(allocator, self.makeToken(.comma, start, self.pos));
                },
                '(' => {
                    self.advance();
                    try tokens.append(allocator, self.makeToken(.left_paren, start, self.pos));
                },
                ')' => {
                    self.advance();
                    try tokens.append(allocator, self.makeToken(.right_paren, start, self.pos));
                },
                '{' => {
                    self.advance();
                    try tokens.append(allocator, self.makeToken(.left_curly, start, self.pos));
                },
                '}' => {
                    self.advance();
                    try tokens.append(allocator, self.makeToken(.right_curly, start, self.pos));
                },
                ':' => {
                    self.advance();
                    if (self.peek() == ':') {
                        self.advance();
                        try tokens.append(allocator, self.makeToken(.double_colon, start, self.pos));
                    } else {
                        try tokens.append(allocator, self.makeToken(.colon, start, self.pos));
                    }
                },
                '+' => {
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try tokens.append(allocator, self.makeToken(.plus_equal, start, self.pos));
                    } else {
                        try tokens.append(allocator, self.makeToken(.plus, start, self.pos));
                    }
                },
                '=' => {
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try tokens.append(allocator, self.makeToken(.double_equal, start, self.pos));
                    } else {
                        try tokens.append(allocator, self.makeToken(.equal, start, self.pos));
                    }
                },
                else => return error.UnexpectedCharacter,
            }
        }

        try tokens.append(allocator, self.makeToken(.eof, self.pos, self.pos));
        return tokens;
    }

    fn scanIdent(self: *Lexer) Token {
        const start = self.pos;

        while (self.peek()) |c| {
            if (!ascii.isAlphanumeric(c) and c != '_') break;
            self.advance();
        }

        // handle potential keyword
        const val = self.src.getSlice(start, self.pos);
        if (keywords.get(val)) |kind| {
            return self.makeToken(kind, start, self.pos);
        }

        return self.makeToken(.identifier, start, self.pos);
    }

    fn scanNumber(self: *Lexer) Token {
        const start = self.pos;
        var has_decimal = false;

        while (self.peek()) |c| {
            if (ascii.isDigit(c)) {
                self.advance();
            } else if (c == '.' and !has_decimal) {
                has_decimal = true;
                self.advance();
            } else {
                break;
            }
        }

        return self.makeToken(.number, start, self.pos);
    }

    fn peek(self: *const Lexer) ?u8 {
        return self.src.get(self.pos);
    }

    fn check(self: *const Lexer, c: u8) bool {
        const current = self.src.get(self.pos) orelse return false;
        return current == c;
    }

    fn advance(self: *Lexer) void {
        self.pos += 1;
    }

    fn makeToken(
        self: *const Lexer,
        kind: TokenKind,
        start: SourceIndex,
        end: SourceIndex,
    ) Token {
        return .{
            .kind = kind,
            .src_id = self.src.id,
            .start = start,
            .len = @intCast(end - start),
        };
    }
};
