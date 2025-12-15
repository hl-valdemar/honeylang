const mem = @import("std").mem;

pub const ErrorList = struct {
    allocator: mem.Allocator,

    pub fn init(allocator: mem.Allocator) ErrorList {
        return .{ .allocator = allocator };
    }

    pub fn add() !void {}
};
