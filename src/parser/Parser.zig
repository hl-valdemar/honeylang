const std = @import("std");
const mem = std.mem;

const Source = @import("../source/Source.zig");
const Lexer = @import("../lexer/Lexer.zig");

lexer: *const Lexer,
src: *const Source,

const Self = @This();

pub fn init(lexer: *const Lexer, src: *const Source) Self {
    return Self{ .src = src, .lexer = lexer };
}

pub fn deinit(self: *const Self, gpa: mem.Allocator) void {
    _ = self;
    _ = gpa;
}

pub fn parse(self: *Self, gpa: mem.Allocator) !void {
    _ = self;
    _ = gpa;
}
