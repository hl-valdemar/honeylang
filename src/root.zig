// packages
pub const compiler = @import("compiler.zig");

// types
pub const Args = @import("util/Args.zig");
pub const StringPool = @import("util/StringPool.zig");
pub const SourceManager = @import("source/Manager.zig");
pub const Diagnostic = @import("diagnostic/Store.zig");
pub const Lexer = @import("lexer/Lexer.zig");
pub const Parser = @import("parser/Parser.zig");
pub const Sema = @import("sema/Sema.zig");
pub const Optimizer = @import("optimizer/Optimizer.zig");

const std = @import("std");

// agnostic types
pub const Payload = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    pub fn from(ref: anytype) Payload {
        return @enumFromInt(@intFromEnum(ref));
    }

    pub fn fromIndex(index: usize) Payload {
        return @enumFromInt(@as(u32, @intCast(index)));
    }

    pub fn to(ref: Payload, comptime Ref: type) Ref {
        return @enumFromInt(@intFromEnum(ref));
    }
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
    _ = @import("compiler.zig");
    _ = @import("util/Args.zig");
    _ = @import("util/StringPool.zig");
    _ = @import("lexer/tests.zig");
    _ = @import("parser/tests.zig");
    _ = @import("sema/tests.zig");
    _ = @import("optimizer/tests.zig");
}
