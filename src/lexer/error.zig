const std = @import("std");
const mem = @import("std").mem;

const SourceIndex = @import("../source/source.zig").SourceIndex;

pub const LexerErrorKind = enum {
    // character errors
    unexpected_character,
    invalid_character,

    // number literal errors
    invalid_number_literal,
    multiple_decimal_points,

    // string literal errors
    unterminated_string,
    invalid_escape_sequence,

    // general errors
    unexpected_eof,
};

pub const LexerError = struct {
    kind: LexerErrorKind,
    start: SourceIndex,
    end: SourceIndex,
    character: ?u8 = null, // the problematic character, if applicable
};

pub const ErrorList = struct {
    allocator: mem.Allocator,
    errors: std.ArrayList(LexerError),

    pub fn init(allocator: mem.Allocator) !ErrorList {
        return .{
            .allocator = allocator,
            .errors = try std.ArrayList(LexerError).initCapacity(allocator, 4),
        };
    }

    pub fn deinit(self: *ErrorList) void {
        self.errors.deinit(self.allocator);
    }

    pub fn add(self: *ErrorList, err: LexerError) !void {
        try self.errors.append(self.allocator, err);
    }

    pub fn addSimple(
        self: *ErrorList,
        kind: LexerErrorKind,
        start: SourceIndex,
        end: SourceIndex,
    ) !void {
        try self.errors.append(self.allocator, .{
            .kind = kind,
            .start = start,
            .end = end,
        });
    }

    pub fn addWithChar(
        self: *ErrorList,
        kind: LexerErrorKind,
        character: u8,
        start: SourceIndex,
        end: SourceIndex,
    ) !void {
        try self.errors.append(self.allocator, .{
            .kind = kind,
            .start = start,
            .end = end,
            .character = character,
        });
    }

    pub fn hasErrors(self: *const ErrorList) bool {
        return self.errors.items.len > 0;
    }

    pub fn count(self: *const ErrorList) usize {
        return self.errors.items.len;
    }
};
