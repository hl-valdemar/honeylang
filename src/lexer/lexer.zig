const std = @import("std");
const mem = @import("std").mem;
const ascii = @import("std").ascii;

const SourceIndex = @import("../source/source.zig").SourceIndex;
const SourceCode = @import("../source/source.zig").SourceCode;

const Token = @import("token.zig").Token;
const TokenKind = @import("token.zig").Kind;
const TokenList = @import("token.zig").TokenList;

const lexer_error = @import("error.zig");
const LexerErrorKind = lexer_error.LexerErrorKind;
const ErrorList = lexer_error.ErrorList;

pub const error_printer = @import("error_printer.zig");

const keywords = std.StaticStringMap(TokenKind).initComptime(.{
    // declaration-related
    .{ "func", .func },

    // abi modifiers
    .{ "c", .abi_c },
    .{ "cobol", .abi_cobol },
    .{ "fortran", .abi_fortran },

    // statement-related
    .{ "mut", .mut },
    .{ "defer", .defer_ },
    .{ "return", .return_ },

    // logic-related
    .{ "if", .if_ },
    .{ "else", .else_ },
    .{ "not", .not },
    .{ "and", .and_ },
    .{ "or", .or_ },
    .{ "xor", .xor },
    .{ "true", .bool },
    .{ "false", .bool },
});

pub const LexerResult = struct {
    tokens: TokenList,
    errors: ErrorList,
};

pub fn scan(allocator: mem.Allocator, src: *const SourceCode) !LexerResult {
    var lexer = try Lexer.init(allocator, src);
    return lexer.scan();
}

const Lexer = struct {
    allocator: mem.Allocator,
    src: *const SourceCode,
    pos: SourceIndex,
    errors: ErrorList,

    fn init(allocator: mem.Allocator, src: *const SourceCode) !Lexer {
        return .{
            .allocator = allocator,
            .src = src,
            .pos = 0,
            .errors = try ErrorList.init(allocator),
        };
    }

    fn scan(self: *Lexer) !LexerResult {
        var tokens = try TokenList.initCapacity(self.allocator, 10);

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
                try tokens.append(self.allocator, self.scanIdent());
            } else if (ascii.isDigit(current)) {
                try tokens.append(self.allocator, self.scanNumber());
            } else switch (current) {
                ',' => {
                    self.advance();
                    try tokens.append(self.allocator, self.makeToken(.comma, start, self.pos));
                },
                '@' => {
                    self.advance();
                    try tokens.append(self.allocator, self.makeToken(.at, start, self.pos));
                },
                '(' => {
                    self.advance();
                    try tokens.append(self.allocator, self.makeToken(.left_paren, start, self.pos));
                },
                ')' => {
                    self.advance();
                    try tokens.append(self.allocator, self.makeToken(.right_paren, start, self.pos));
                },
                '{' => {
                    self.advance();
                    try tokens.append(self.allocator, self.makeToken(.left_curly, start, self.pos));
                },
                '}' => {
                    self.advance();
                    try tokens.append(self.allocator, self.makeToken(.right_curly, start, self.pos));
                },
                '<' => {
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try tokens.append(self.allocator, self.makeToken(.less_equal, start, self.pos));
                    } else {
                        try tokens.append(self.allocator, self.makeToken(.less, start, self.pos));
                    }
                },
                '>' => {
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try tokens.append(self.allocator, self.makeToken(.greater_equal, start, self.pos));
                    } else {
                        try tokens.append(self.allocator, self.makeToken(.greater, start, self.pos));
                    }
                },
                '+' => {
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try tokens.append(self.allocator, self.makeToken(.plus_equal, start, self.pos));
                    } else {
                        try tokens.append(self.allocator, self.makeToken(.plus, start, self.pos));
                    }
                },
                '-' => {
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try tokens.append(self.allocator, self.makeToken(.minus_equal, start, self.pos));
                    } else {
                        try tokens.append(self.allocator, self.makeToken(.minus, start, self.pos));
                    }
                },
                '*' => {
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try tokens.append(self.allocator, self.makeToken(.star_equal, start, self.pos));
                    } else {
                        try tokens.append(self.allocator, self.makeToken(.star, start, self.pos));
                    }
                },
                '/' => {
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try tokens.append(self.allocator, self.makeToken(.slash_equal, start, self.pos));
                    } else {
                        try tokens.append(self.allocator, self.makeToken(.slash, start, self.pos));
                    }
                },
                '!' => {
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try tokens.append(self.allocator, self.makeToken(.not_equal, start, self.pos));
                    } else {
                        // standalone '!' is unexpected
                        try self.errors.addWithChar(.unexpected_character, '!', start, self.pos);
                    }
                },
                '=' => {
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try tokens.append(self.allocator, self.makeToken(.double_equal, start, self.pos));
                    } else {
                        try tokens.append(self.allocator, self.makeToken(.equal, start, self.pos));
                    }
                },
                ':' => {
                    self.advance();
                    if (self.peek() == ':') {
                        self.advance();
                        try tokens.append(self.allocator, self.makeToken(.double_colon, start, self.pos));
                    } else {
                        try tokens.append(self.allocator, self.makeToken(.colon, start, self.pos));
                    }
                },
                else => {
                    // record error and skip the character
                    try self.errors.addWithChar(.unexpected_character, current, start, start + 1);
                    self.advance();
                },
            }
        }

        try tokens.append(self.allocator, self.makeToken(.eof, self.pos, self.pos));

        return .{
            .tokens = tokens,
            .errors = self.errors,
        };
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
        var has_error = false;

        while (self.peek()) |c| {
            if (ascii.isDigit(c)) {
                self.advance();
            } else if (c == '.' and !has_decimal) {
                // check if next char is also a digit (to distinguish from method calls)
                if (self.peekOffset(1)) |next| {
                    if (ascii.isDigit(next)) {
                        has_decimal = true;
                        self.advance();
                    } else {
                        break;
                    }
                } else {
                    has_decimal = true;
                    self.advance();
                }
            } else if (c == '.' and has_decimal) {
                // multiple decimal points
                if (!has_error) {
                    self.errors.addWithChar(.multiple_decimal_points, c, self.pos, self.pos + 1) catch {};
                    has_error = true;
                }
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

    fn peekOffset(self: *const Lexer, offset: SourceIndex) ?u8 {
        return self.src.get(self.pos + offset);
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
