const std = @import("std");
const mem = @import("std").mem;

pub const Register64 = enum(u4) { x0, x1, x2, x3, x4, x5, x6, x7 };
pub const Register32 = enum(u4) { w0, w1, w2, w3, w4, w5, w6, w7 };

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

    pub fn directive(self: *Arm64Emitter, dir: []const u8) !void {
        try self.buffer.appendSlice(self.allocator, dir);
        try self.buffer.append(self.allocator, '\n');
    }

    pub fn label(self: *Arm64Emitter, name: []const u8) !void {
        try self.buffer.appendSlice(self.allocator, name);
        try self.buffer.appendSlice(self.allocator, ":\n");
    }

    pub fn global(self: *Arm64Emitter, name: []const u8) !void {
        try self.buffer.appendSlice(self.allocator, ".global ");
        try self.buffer.appendSlice(self.allocator, name);
        try self.buffer.append(self.allocator, '\n');
    }

    pub fn comment(self: *Arm64Emitter, msg: []const u8, prefix: ?[]const u8) !void {
        if (prefix) |p| try self.buffer.appendSlice(self.allocator, p);
        try self.buffer.appendSlice(self.allocator, "; ");
        try self.buffer.appendSlice(self.allocator, msg);
        try self.buffer.append(self.allocator, '\n');
    }

    pub fn newline(self: *Arm64Emitter) !void {
        try self.buffer.append(self.allocator, '\n');
    }

    // INSTRUCTIONS

    pub fn ret(self: *Arm64Emitter) !void {
        try self.buffer.appendSlice(self.allocator, self.indent);
        try self.buffer.appendSlice(self.allocator, "ret\n");
    }

    // mov w<reg>, #<imm> (32-bit signed)
    pub fn emitMovImmI32(self: *Arm64Emitter, reg: Register32, value: i32) !void {
        var buf: [32]u8 = undefined;
        const instr = std.fmt.bufPrint(&buf, "mov w{d}, #{d}\n", .{ reg, value }) catch unreachable;

        try self.buffer.appendSlice(self.allocator, self.indent);
        try self.buffer.appendSlice(self.allocator, instr);
    }

    // mov w<reg>, #<imm> (32-bit unsigned)
    pub fn emitMovImmU32(self: *Arm64Emitter, reg: Register32, value: u32) !void {
        var buf: [32]u8 = undefined;
        const instr = std.fmt.bufPrint(&buf, "mov w{d}, #{d}\n", .{ reg, value }) catch unreachable;

        try self.buffer.appendSlice(self.allocator, self.indent);
        try self.buffer.appendSlice(self.allocator, instr);
    }

    // COMPOUND

    pub fn funcPrologue(self: *Arm64Emitter) !void {
        // c calling convention prologue:
        // - save frame pointer and link register

        // TODO: support more calling conventions

        try self.buffer.appendSlice(self.allocator, self.indent);
        try self.buffer.appendSlice(self.allocator, "stp x29, x30, [sp, #-16]!\n");
        try self.buffer.appendSlice(self.allocator, self.indent);
        try self.buffer.appendSlice(self.allocator, "mov x29, sp\n");
    }

    pub fn funcEpilogue(self: *Arm64Emitter) !void {
        // c calling convention epilogue:
        // - restore frame pointer and link register

        try self.buffer.appendSlice(self.allocator, self.indent);
        try self.buffer.appendSlice(self.allocator, "ldp x29, x30, [sp], #16\n");
    }
};
