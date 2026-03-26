// packages
pub const Args = @import("util/Args.zig");
pub const StringPool = @import("util/StringPool.zig");
pub const SourceManager = @import("source/Manager.zig");
pub const Lexer = @import("lexer/Lexer.zig");
pub const Parser = @import("parser/Parser.zig");

// agnostic types
pub const BaseIdx = u32;

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
}
