const std = @import("std");
const mem = std.mem;

/// Virtual register - unlimited during MIR generation,
/// mapped to physical registers during lowering.
pub const VReg = u16;

/// Operand width for instructions.
pub const Width = enum {
    w32,
    w64,

    pub fn bits(self: Width) u8 {
        return switch (self) {
            .w32 => 32,
            .w64 => 64,
        };
    }
};

/// Binary operation type.
pub const BinOp = enum {
    add,
    sub,
    mul,
    div_s, // signed division
    div_u, // unsigned division
    mod_s,
    mod_u,
    bit_and,
    bit_or,
    bit_xor,
    shl,
    shr_s, // arithmetic shift right
    shr_u, // logical shift right
};

/// Comparison operation type.
pub const CmpOp = enum {
    eq,
    ne,
    lt_s,
    lt_u,
    le_s,
    le_u,
    gt_s,
    gt_u,
    ge_s,
    ge_u,
};

/// Machine IR instruction - architecture independent.
pub const MInst = union(enum) {
    /// Load immediate value into register.
    mov_imm: struct {
        dst: VReg,
        value: i64,
        width: Width,
    },

    /// Copy register to register.
    mov_reg: struct {
        dst: VReg,
        src: VReg,
        width: Width,
    },

    /// Binary arithmetic/logic operation.
    binop: struct {
        op: BinOp,
        dst: VReg,
        lhs: VReg,
        rhs: VReg,
        width: Width,
    },

    /// Comparison operation (result is 0 or 1).
    cmp: struct {
        op: CmpOp,
        dst: VReg,
        lhs: VReg,
        rhs: VReg,
        width: Width,
    },

    /// Return from function with optional value.
    ret: struct {
        value: ?VReg,
        width: Width,
    },

    /// Function prologue (save frame pointer, etc).
    prologue,

    /// Function epilogue (restore frame pointer, etc).
    epilogue,
};

/// A function in MIR form.
pub const MIRFunction = struct {
    name: []const u8,
    is_c_calling_conv: bool,
    instructions: std.ArrayListUnmanaged(MInst),
    next_vreg: VReg,
    allocator: mem.Allocator,

    pub fn init(allocator: mem.Allocator, name: []const u8, is_c_calling_conv: bool) MIRFunction {
        return .{
            .name = name,
            .is_c_calling_conv = is_c_calling_conv,
            .instructions = .{},
            .next_vreg = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *MIRFunction) void {
        self.instructions.deinit(self.allocator);
    }

    /// Allocate a new virtual register.
    pub fn allocVReg(self: *MIRFunction) VReg {
        const reg = self.next_vreg;
        self.next_vreg += 1;
        return reg;
    }

    /// Emit an instruction.
    pub fn emit(self: *MIRFunction, inst: MInst) !void {
        try self.instructions.append(self.allocator, inst);
    }

    /// Emit a mov_imm instruction and return the destination vreg.
    pub fn emitMovImm(self: *MIRFunction, value: i64, width: Width) !VReg {
        const dst = self.allocVReg();
        try self.emit(.{ .mov_imm = .{ .dst = dst, .value = value, .width = width } });
        return dst;
    }

    /// Emit a mov_reg instruction and return the destination vreg.
    pub fn emitMovReg(self: *MIRFunction, src: VReg, width: Width) !VReg {
        const dst = self.allocVReg();
        try self.emit(.{ .mov_reg = .{ .dst = dst, .src = src, .width = width } });
        return dst;
    }

    /// Emit a binary operation and return the destination vreg.
    pub fn emitBinOp(self: *MIRFunction, op: BinOp, lhs: VReg, rhs: VReg, width: Width) !VReg {
        const dst = self.allocVReg();
        try self.emit(.{ .binop = .{ .op = op, .dst = dst, .lhs = lhs, .rhs = rhs, .width = width } });
        return dst;
    }

    /// Emit a return instruction.
    pub fn emitRet(self: *MIRFunction, value: ?VReg, width: Width) !void {
        try self.emit(.{ .ret = .{ .value = value, .width = width } });
    }
};

/// Collection of MIR functions for a compilation unit.
pub const MIRModule = struct {
    functions: std.ArrayListUnmanaged(MIRFunction),
    allocator: mem.Allocator,

    pub fn init(allocator: mem.Allocator) MIRModule {
        return .{
            .functions = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *MIRModule) void {
        for (self.functions.items) |*func| {
            func.deinit();
        }
        self.functions.deinit(self.allocator);
    }

    pub fn addFunction(self: *MIRModule, name: []const u8, is_c_calling_conv: bool) !*MIRFunction {
        try self.functions.append(self.allocator, MIRFunction.init(self.allocator, name, is_c_calling_conv));
        return &self.functions.items[self.functions.items.len - 1];
    }
};
