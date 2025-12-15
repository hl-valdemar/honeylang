const std = @import("std");
const mem = @import("std").mem;

const SourceIndex = @import("../source/source.zig").SourceIndex;
const SourceCode = @import("../source/source.zig").SourceCode;

pub const SemanticErrorKind = enum {
    unknown_type,
    duplicate_symbol,
    undefined_symbol,
    type_mismatch,
};

pub const SemanticError = struct {
    kind: SemanticErrorKind,
    start: SourceIndex,
    end: SourceIndex,
};

pub const ErrorList = struct {
    allocator: mem.Allocator,
    errors: std.ArrayList(SemanticError),

    pub fn init(allocator: mem.Allocator) !ErrorList {
        const capacity = 0;
        return .{
            .allocator = allocator,
            .errors = try std.ArrayList(SemanticError).initCapacity(allocator, capacity),
        };
    }

    pub fn deinit(self: *ErrorList) void {
        self.errors.deinit();
    }

    pub fn add(self: *ErrorList, err: SemanticError) !void {
        try self.errors.append(self.allocator, err);
    }

    pub fn hasErrors(self: *const ErrorList) bool {
        return self.errors.items.len > 0;
    }

    pub fn count(self: *const ErrorList) usize {
        return self.errors.items.len;
    }
};
