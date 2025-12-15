const std = @import("std");
const mem = @import("std").mem;

const SymbolIndex = @import("../semantic/symbols.zig").SymbolIndex;

pub const ComptimeErrorKind = enum {
    circular_dependency,
    division_by_zero,
    overflow,
    evaluation_failed,
};

pub const ComptimeError = struct {
    kind: ComptimeErrorKind,
    symbol_idx: SymbolIndex,
};

pub const ComptimeErrorList = struct {
    allocator: mem.Allocator,
    errors: std.ArrayList(ComptimeError),

    pub fn init(allocator: mem.Allocator) !ComptimeErrorList {
        const capacity = 0;
        return .{
            .allocator = allocator,
            .errors = try std.ArrayList(ComptimeError).initCapacity(allocator, capacity),
        };
    }

    pub fn deinit(self: *ComptimeErrorList) void {
        self.errors.deinit(self.allocator);
    }

    pub fn add(self: *ComptimeErrorList, err: ComptimeError) !void {
        try self.errors.append(self.allocator, err);
    }

    pub fn hasErrors(self: *const ComptimeErrorList) bool {
        return self.errors.items.len > 0;
    }
};
