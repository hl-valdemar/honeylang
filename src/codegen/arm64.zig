const std = @import("std");
const mem = @import("std").mem;

/// ARM64 assembly emitter.
pub const Arm64Emitter = struct {
    allocator: mem.Allocator,
    buffer: std.ArrayList(u8),
    indent: []const u8,

    pub fn init(allocator: mem.Allocator) !Arm64Emitter {
        return .{
            .allocator = allocator,
            .buffer = try std.ArrayList(u8).initCapacity(allocator, 4096), // 4 KB
            .indent = "  ",
        };
    }

    pub fn deinit(self: *Arm64Emitter) void {
        self.buffer.deinit(self.allocator);
    }

    pub fn getOutput(self: *const Arm64Emitter) []const u8 {
        return self.buffer.items;
    }

    // DIRECTIVES

    pub fn emitDirective(self: *Arm64Emitter, directive: []const u8) !void {
        try self.buffer.appendSlice(self.allocator, directive);
        try self.buffer.append(self.allocator, '\n');
    }

    pub fn emitLabel(self: *Arm64Emitter, name: []const u8) !void {
        try self.buffer.appendSlice(self.allocator, name);
        try self.buffer.appendSlice(self.allocator, ":\n");
    }

    pub fn emitGlobal(self: *Arm64Emitter, name: []const u8) !void {
        try self.buffer.appendSlice(self.allocator, ".global ");
        try self.buffer.appendSlice(self.allocator, name);
        try self.buffer.append(self.allocator, '\n');
    }

    pub fn emitComment(self: *Arm64Emitter, comment: []const u8, prefix: ?[]const u8) !void {
        if (prefix) |p| try self.buffer.appendSlice(self.allocator, p);
        try self.buffer.appendSlice(self.allocator, "// ");
        try self.buffer.appendSlice(self.allocator, comment);
        try self.buffer.append(self.allocator, '\n');
    }

    pub fn emitNewline(self: *Arm64Emitter) !void {
        try self.buffer.append(self.allocator, '\n');
    }

    // INSTRUCTIONS

    pub fn emitRet(self: *Arm64Emitter) !void {
        try self.buffer.appendSlice(self.allocator, self.indent);
        try self.buffer.appendSlice(self.allocator, "ret\n");
    }

    pub fn emitMovImmediate(self: *Arm64Emitter) !void {
        _ = self;
        // TODO: move immediate value to register (params)
    }
};
