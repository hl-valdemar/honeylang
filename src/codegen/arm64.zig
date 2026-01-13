const std = @import("std");
const mem = @import("std").mem;

pub const Register = u4;

/// Simple linear register allocator.
/// Uses x9-x15 as scratch registers (caller-saved temporaries).
pub const Arm64Registers = struct {
    // Bitmask: bit N set = register xN is available
    // x9-x15 are our scratch registers
    available: u16 = INITIAL_MASK,

    const INITIAL_MASK: u16 = 0b0111_1111_0000_0000; // bits 9-15 set

    pub fn alloc(self: *Arm64Registers) ?Register {
        if (self.available == 0) return null;
        const reg: Register = @intCast(@ctz(self.available));
        self.available &= ~(@as(u16, 1) << reg);
        return reg;
    }

    pub fn free(self: *Arm64Registers, reg: Register) void {
        self.available |= @as(u16, 1) << reg;
    }

    pub fn reset(self: *Arm64Registers) void {
        self.available = INITIAL_MASK;
    }

    /// Free a register only if it's a scratch register (x9-x15).
    /// Safe to call with x0-x7 (will be ignored).
    pub fn safeFree(self: *Arm64Registers, reg: Register) void {
        if (reg >= 9 and reg <= 15) {
            self.free(reg);
        }
    }

    /// Format register name for assembly output (64 bit access).
    pub fn name64(reg: Register) []const u8 {
        const names = [_][]const u8{
            "x0", "x1", "x2",  "x3",  "x4",  "x5",  "x6",  "x7",
            "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
        };
        return names[reg];
    }

    /// Format register name for assembly output (32 bit access).
    pub fn name32(reg: Register) []const u8 {
        const names = [_][]const u8{
            "w0", "w1", "w2",  "w3",  "w4",  "w5",  "w6",  "w7",
            "w8", "w9", "w10", "w11", "w12", "w13", "w14", "w15",
        };
        return names[reg];
    }
};

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

    // mov x<reg>, #<imm> (64-bit)
    pub fn movImm64(self: *Arm64Emitter, dst: Register, value: i64) !void {
        var buf: [32]u8 = undefined;
        const reg_name = Arm64Registers.name64(dst);
        const instr = std.fmt.bufPrint(&buf, "mov {s}, #{d}\n", .{ reg_name, value }) catch unreachable;

        try self.buffer.appendSlice(self.allocator, self.indent);
        try self.buffer.appendSlice(self.allocator, instr);
    }

    // mov x<reg>, x<reg> (64-bit)
    pub fn movReg64(self: *Arm64Emitter, dst: Register, src: Register) !void {
        if (dst == src) return; // noop

        var buf: [32]u8 = undefined;
        const instr = std.fmt.bufPrint(&buf, "mov {s}, {s}\n", .{
            Arm64Registers.name64(dst),
            Arm64Registers.name64(src),
        }) catch unreachable;

        try self.buffer.appendSlice(self.allocator, self.indent);
        try self.buffer.appendSlice(self.allocator, instr);
    }

    // mov w<reg>, #<imm> (32-bit)
    pub fn movImm32(self: *Arm64Emitter, dst: Register, value: i32) !void {
        var buf: [32]u8 = undefined;
        const reg_name = Arm64Registers.name32(dst);
        const instr = std.fmt.bufPrint(&buf, "mov {s}, #{d}\n", .{ reg_name, value }) catch unreachable;

        try self.buffer.appendSlice(self.allocator, self.indent);
        try self.buffer.appendSlice(self.allocator, instr);
    }

    // mov w<reg>, w<reg> (32-bit)
    pub fn movReg32(self: *Arm64Emitter, dst: Register, src: Register) !void {
        if (dst == src) return; // noop

        var buf: [32]u8 = undefined;
        const instr = std.fmt.bufPrint(&buf, "mov {s}, {s}\n", .{
            Arm64Registers.name32(dst),
            Arm64Registers.name32(src),
        }) catch unreachable;

        try self.buffer.appendSlice(self.allocator, self.indent);
        try self.buffer.appendSlice(self.allocator, instr);
    }

    pub fn addReg32(self: *Arm64Emitter, dst: Register, left: Register, right: Register) !void {
        var buf: [32]u8 = undefined;
        const instr = std.fmt.bufPrint(&buf, "add {s}, {s}, {s}\n", .{
            Arm64Registers.name32(dst),
            Arm64Registers.name32(left),
            Arm64Registers.name32(right),
        }) catch unreachable;

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
