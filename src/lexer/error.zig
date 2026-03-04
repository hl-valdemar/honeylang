const std = @import("std");
const mem = std.mem;

const scan_error_kind_names = std.EnumArray(
    ScanErrorKind,
    @import("../root.zig").ErrorInfo,
).init(.{
    .unrecognized_character = .{
        .code = "L001",
        .message = "unrecognized character",
        .help = "character doesn't fit any tokens in honeylang",
        .severity = .err,
    },
    .multiple_decimal_points = .{
        .code = "L002",
        .message = "multiple decimal points",
        .help = "remove the last decimal point",
        .severity = .err,
    },
    .empty_hex_literal = .{
        .code = "L003",
        .message = "empty hexadecimal literal",
        .help = "hex notation should be followed by a hexadecimal value",
        .severity = .err,
    },
    .empty_bin_literal = .{
        .code = "L004",
        .message = "empty binary literal",
        .help = "binary notation should be followed by a binary value",
        .severity = .err,
    },
});

pub const ScanErrorKind = enum {
    unrecognized_character,
    multiple_decimal_points,
    empty_hex_literal,
    empty_bin_literal,
};

pub const ScanErrorDesc = struct {
    kind: ScanErrorKind,
    start: @import("Lexer.zig").Index,
    end: @import("Lexer.zig").Index,
};

pub const ScanErrors = struct {
    kinds: std.ArrayListUnmanaged(ScanErrorKind),
    starts: std.ArrayListUnmanaged(@import("Lexer.zig").Index),
    ends: std.ArrayListUnmanaged(@import("Lexer.zig").Index),

    pub fn init(gpa: mem.Allocator) !ScanErrors {
        const init_err_cap = 10;
        return .{
            .kinds = try std.ArrayListUnmanaged(ScanErrorKind).initCapacity(gpa, init_err_cap),
            .starts = try std.ArrayListUnmanaged(@import("Lexer.zig").Index).initCapacity(gpa, init_err_cap),
            .ends = try std.ArrayListUnmanaged(@import("Lexer.zig").Index).initCapacity(gpa, init_err_cap),
        };
    }

    pub fn deinit(self: *ScanErrors, gpa: mem.Allocator) void {
        self.kinds.deinit(gpa);
        self.starts.deinit(gpa);
        self.ends.deinit(gpa);
    }

    pub fn push(
        self: *ScanErrors,
        gpa: mem.Allocator,
        kind: ScanErrorKind,
        start: @import("Lexer.zig").Index,
        end: @import("Lexer.zig").Index,
    ) !void {
        self.assertHealth();
        try self.kinds.append(gpa, kind);
        try self.starts.append(gpa, start);
        try self.ends.append(gpa, end);
    }

    pub fn pop(self: *ScanErrors) ?ScanErrorDesc {
        self.assertHealth();

        if (self.len() > 0)
            return .{
                .kind = self.kinds.pop() orelse unreachable,
                .start = self.starts.pop() orelse unreachable,
                .end = self.ends.pop() orelse unreachable,
            };

        return null;
    }

    pub fn get(self: *const ScanErrors, idx: usize) ?ScanErrorDesc {
        self.assertHealth();

        if (self.len() < idx)
            return .{
                .kind = self.kinds.items[idx],
                .starts = self.starts.items[idx],
                .ends = self.ends.items[idx],
            };

        return null;
    }

    pub fn len(self: *const ScanErrors) usize {
        self.assertHealth();
        return self.kinds.items.len;
    }

    fn assertHealth(self: *const ScanErrors) void {
        const kinds_starts = self.kinds.items.len == self.starts.items.len;
        const starts_ends = self.starts.items.len == self.ends.items.len;
        std.debug.assert(kinds_starts and starts_ends);
    }
};
