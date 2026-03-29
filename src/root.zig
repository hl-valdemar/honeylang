// packages
pub const Args = @import("util/Args.zig");
pub const StringPool = @import("util/StringPool.zig");
pub const SourceManager = @import("source/Manager.zig");
pub const Lexer = @import("lexer/Lexer.zig");
pub const Parser = @import("parser/Parser.zig");
pub const HIR = @import("ir/HIR.zig");

const std = @import("std");

// agnostic types
pub const BaseRef = enum(u32) {
    none = std.math.maxInt(u32),
    _,
};

pub const Severity = enum {
    fatal,
    err,
    warning,
};

pub const ErrorInfo = struct {
    code: []const u8,
    message: []const u8,
    help: []const u8,
    severity: Severity = .err,
};

test {
    _ = @import("util/StringPool.zig");
    _ = @import("lexer/tests.zig");
    _ = @import("parser/tests.zig");
    _ = @import("ir/tests.zig");
}
