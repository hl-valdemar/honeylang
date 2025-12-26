const std = @import("std");
const mem = @import("std").mem;

const SourceIndex = @import("../source/source.zig").SourceIndex;
const SourceCode = @import("../source/source.zig").SourceCode;

pub const SemanticErrorKind = enum {
    // symbol errors
    unknown_type,
    duplicate_symbol,
    undefined_symbol,

    // type checking errors
    type_mismatch,
    invalid_operand_type,
    cannot_negate_unsigned,
    logical_op_requires_bool,
    arithmetic_op_requires_numeric,
    comparison_requires_compatible,
    argument_count_mismatch,
    argument_type_mismatch,
    return_type_mismatch,
    assignment_to_immutable,
    condition_not_bool,
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
