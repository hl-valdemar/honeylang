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

pub const error_printer = @import("error-printer.zig");

const keywords = std.StaticStringMap(TokenKind).initComptime(.{
    // declaration-related
    .{ "func", .func },
    .{ "struct", .@"struct" },

    // statement-related
    .{ "mut", .mut },
    .{ "defer", .@"defer" },
    .{ "return", .@"return" },

    // control flow
    .{ "if", .@"if" },
    .{ "else", .@"else" },
    .{ "while", .@"while" },
    .{ "break", .@"break" },
    .{ "continue", .@"continue" },
.{ "and", .@"and" },
    .{ "or", .@"or" },
    .{ "xor", .xor },
    .{ "namespace", .namespace },
    .{ "pub", .@"pub" },
    .{ "import", .import },
    .{ "opaque", .@"opaque" },
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
                '.' => {
                    self.advance();
                    if (self.peek() == '.') {
                        self.advance();
                        if (self.peek() == '.') {
                            self.advance();
                            try tokens.append(self.allocator, self.makeToken(.ellipsis, start, self.pos));
                        } else {
                            try tokens.append(self.allocator, self.makeToken(.dot_dot, start, self.pos));
                        }
                    } else {
                        try tokens.append(self.allocator, self.makeToken(.dot, start, self.pos));
                    }
                },
                '@' => {
                    self.advance();
                    try tokens.append(self.allocator, self.makeToken(.at, start, self.pos));
                },
                '&' => {
                    self.advance();
                    if (self.peek() == '&') {
                        self.advance();
                        try tokens.append(self.allocator, self.makeToken(.@"and", start, self.pos));
                        try self.errors.addSimple(.use_and_instead, start, self.pos);
                    } else {
                        try tokens.append(self.allocator, self.makeToken(.ampersand, start, self.pos));
                    }
                },
                '^' => {
                    self.advance();
                    try tokens.append(self.allocator, self.makeToken(.caret, start, self.pos));
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
                '[' => {
                    self.advance();
                    try tokens.append(self.allocator, self.makeToken(.left_bracket, start, self.pos));
                },
                ']' => {
                    self.advance();
                    try tokens.append(self.allocator, self.makeToken(.right_bracket, start, self.pos));
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
                        try tokens.append(self.allocator, self.makeToken(.not, start, self.pos));
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
                '"' => {
                    try tokens.append(self.allocator, try self.scanString());
                },
                '\'' => {
                    try tokens.append(self.allocator, try self.scanChar());
                },
                '|' => {
                    self.advance();
                    if (self.peek() == '|') {
                        self.advance();
                        try tokens.append(self.allocator, self.makeToken(.@"or", start, self.pos));
                        try self.errors.addSimple(.use_or_instead, start, self.pos);
                    } else {
                        try self.errors.addWithChar(.unexpected_character, '|', start, self.pos);
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

        // check for hex (0x/0X) or binary (0b/0B) prefix
        if (self.peek()) |c| {
            if (c == '0') {
                if (self.peekOffset(1)) |next| {
                    if (next == 'x' or next == 'X') {
                        self.advance(); // consume '0'
                        self.advance(); // consume 'x'/'X'
                        while (self.peek()) |h| {
                            if (ascii.isDigit(h) or (h >= 'a' and h <= 'f') or (h >= 'A' and h <= 'F')) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        return self.makeToken(.number, start, self.pos);
                    } else if (next == 'b' or next == 'B') {
                        self.advance(); // consume '0'
                        self.advance(); // consume 'b'/'B'
                        while (self.peek()) |h| {
                            if (h == '0' or h == '1') {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        return self.makeToken(.number, start, self.pos);
                    }
                }
            }
        }

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

    fn scanString(self: *Lexer) !Token {
        const quote_start = self.pos;
        self.advance(); // skip opening "

        const content_start = self.pos;

        while (self.peek()) |c| {
            if (c == '"') {
                const content_end = self.pos;
                self.advance(); // skip closing "
                return self.makeToken(.string_literal, content_start, content_end);
            }
            if (c == '\n') break; // unterminated
            self.advance();
        }

        // unterminated string
        try self.errors.addSimple(.unterminated_string, quote_start, self.pos);
        return self.makeToken(.string_literal, content_start, self.pos);
    }

    fn scanChar(self: *Lexer) !Token {
        const quote_start = self.pos;
        self.advance(); // skip opening '

        const content_start = self.pos;

        while (self.peek()) |c| {
            if (c == '\'') {
                const content_end = self.pos;
                self.advance(); // skip closing '
                return self.makeToken(.char_literal, content_start, content_end);
            }
            if (c == '\n') break; // unterminated
            if (c == '\\') {
                self.advance(); // skip backslash
            }
            self.advance();
        }

        // unterminated char literal
        try self.errors.addSimple(.unterminated_string, quote_start, self.pos);
        return self.makeToken(.char_literal, content_start, self.pos);
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
