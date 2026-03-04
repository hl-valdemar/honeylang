const std = @import("std");
const mem = std.mem;

pub const keywords = std.StaticStringMap(TokenKind).initComptime(.{
    .{ "mut", .mut },
    .{ "func", .func },
    .{ "return", .@"return" },
});

pub const TokenKind = enum {
    // complex tokens
    identifier,
    number,

    // keywords
    mut,
    func,
    @"return",

    // single-char tokens
    colon,
    comma,
    equal,
    left_paren,
    right_paren,
    left_bracket,
    right_bracket,
    left_curly,
    right_curly,

    // double-char tokens
    double_colon,

    // special tokens
    newline,
    eof,
};

pub const TokenDesc = struct {
    kind: TokenKind,
    start: @import("Lexer.zig").Index,
    end: @import("Lexer.zig").Index,
};

pub const Tokens = struct {
    kinds: std.ArrayListUnmanaged(TokenKind),
    starts: std.ArrayListUnmanaged(@import("Lexer.zig").Index),
    ends: std.ArrayListUnmanaged(@import("Lexer.zig").Index),

    pub fn init(gpa: mem.Allocator) !Tokens {
        const init_tok_cap = 1000;
        return .{
            .kinds = try std.ArrayListUnmanaged(TokenKind).initCapacity(gpa, init_tok_cap),
            .starts = try std.ArrayListUnmanaged(@import("Lexer.zig").Index).initCapacity(gpa, init_tok_cap),
            .ends = try std.ArrayListUnmanaged(@import("Lexer.zig").Index).initCapacity(gpa, init_tok_cap),
        };
    }

    pub fn deinit(self: *Tokens, gpa: mem.Allocator) void {
        self.kinds.deinit(gpa);
        self.starts.deinit(gpa);
        self.ends.deinit(gpa);
    }

    pub fn push(
        self: *Tokens,
        gpa: mem.Allocator,
        kind: TokenKind,
        start: @import("Lexer.zig").Index,
        end: @import("Lexer.zig").Index,
    ) !void {
        self.assertHealth();
        try self.kinds.append(gpa, kind);
        try self.starts.append(gpa, start);
        try self.ends.append(gpa, end);
    }

    pub fn pop(self: *Tokens) ?TokenDesc {
        self.assertHealth();

        if (self.len() > 0)
            return .{
                .kind = self.kinds.pop() orelse unreachable,
                .start = self.starts.pop() orelse unreachable,
                .end = self.ends.pop() orelse unreachable,
            };

        return null;
    }

    pub fn get(self: *const Tokens, idx: usize) ?TokenDesc {
        self.assertHealth();

        if (self.len() < idx)
            return .{
                .kind = self.kinds.items[idx],
                .starts = self.starts.items[idx],
                .ends = self.ends.items[idx],
            };

        return null;
    }

    pub fn len(self: *const Tokens) usize {
        self.assertHealth();
        return self.kinds.items.len;
    }

    fn assertHealth(self: *const Tokens) void {
        const kinds_starts = self.kinds.items.len == self.starts.items.len;
        const starts_ends = self.starts.items.len == self.ends.items.len;
        std.debug.assert(kinds_starts and starts_ends);
    }
};
