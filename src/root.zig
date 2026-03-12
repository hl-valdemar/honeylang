// packages
pub const args = @import("util/args.zig");

// agnostic types
pub const BaseIndex = u32;

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

// specific types
pub const Source = @import("source/Source.zig");
pub const Lexer = @import("lexer/Lexer.zig");
pub const Parser = @import("parser/Parser.zig");
