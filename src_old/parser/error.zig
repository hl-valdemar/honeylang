const std = @import("std");
const mem = @import("std").mem;

const SourceIndex = @import("../source/source.zig").SourceIndex;
const TokenKind = @import("../lexer/token.zig").Kind;

pub const ParseErrorKind = enum {
    // structural errors
    unexpected_eof,
    unexpected_token,
    unclosed_brace,
    unclosed_paren,

    // declaration errors
    expected_identifier,
    expected_declaration,
    expected_type_annotation,

    // expression errors
    expected_expression,
    expected_operand,

    // statement errors
    expected_statement,
    expected_block,

    // specific token errors
    expected_colon,
    expected_double_colon,
    expected_equal,
    expected_left_paren,
    expected_right_paren,
    expected_left_curly,
    expected_right_curly,
    expected_comma_or_close,
    expected_string_literal,
};

pub const ParseError = struct {
    kind: ParseErrorKind,
    start: SourceIndex,
    end: SourceIndex,
    expected: ?TokenKind = null, // for "expected X, got Y" messages
    found: ?TokenKind = null,
};

pub const ErrorList = struct {
    allocator: mem.Allocator,
    errors: std.ArrayList(ParseError),

    pub fn init(allocator: mem.Allocator) !ErrorList {
        return .{
            .allocator = allocator,
            .errors = try std.ArrayList(ParseError).initCapacity(allocator, 4),
        };
    }

    pub fn deinit(self: *ErrorList) void {
        self.errors.deinit(self.allocator);
    }

    pub fn add(self: *ErrorList, err: ParseError) !void {
        try self.errors.append(self.allocator, err);
    }

    pub fn addSimple(
        self: *ErrorList,
        kind: ParseErrorKind,
        start: SourceIndex,
        end: SourceIndex,
    ) !void {
        try self.errors.append(self.allocator, .{
            .kind = kind,
            .start = start,
            .end = end,
        });
    }

    pub fn addExpected(
        self: *ErrorList,
        kind: ParseErrorKind,
        expected: TokenKind,
        found: ?TokenKind,
        start: SourceIndex,
        end: SourceIndex,
    ) !void {
        try self.errors.append(self.allocator, .{
            .kind = kind,
            .start = start,
            .end = end,
            .expected = expected,
            .found = found,
        });
    }

    pub fn hasErrors(self: *const ErrorList) bool {
        return self.errors.items.len > 0;
    }

    pub fn count(self: *const ErrorList) usize {
        return self.errors.items.len;
    }
};
