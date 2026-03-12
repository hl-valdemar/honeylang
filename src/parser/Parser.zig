const std = @import("std");
const mem = std.mem;

const Source = @import("../source/Source.zig");
const Lexer = @import("../lexer/Lexer.zig");

tokens: Lexer.TokensResult,
src: *const Source,

const Self = @This();

pub fn init(tokens: Lexer.TokensResult, src: *const Source) Self {
    return Self{ .tokens = tokens, .src = src };
}

pub fn deinit(self: *const Self, gpa: mem.Allocator) void {
    _ = self;
    _ = gpa;
}

pub fn parse(self: *Self, gpa: mem.Allocator) !void {
    _ = self;
    _ = gpa;
}
