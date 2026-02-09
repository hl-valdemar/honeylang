const std = @import("std");
const mem = std.mem;

const CallingConvention = @import("../parser/ast.zig").CallingConvention;
const SymbolIndex = @import("../semantic/symbols.zig").SymbolIndex;

/// Virtual register - unlimited during MIR generation,
/// mapped to physical registers during lowering.
pub const VReg = u16;

/// Operand width for instructions.
pub const Width = enum {
    w32,
    w64,
    ptr, // pointer type (for struct params passed by reference)

    pub fn bits(self: Width) u8 {
        return switch (self) {
            .w32 => 32,
            .w64, .ptr => 64,
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

/// Index into global variable parallel arrays.
pub const GlobalIndex = u16;

/// Label identifier for branch targets.
pub const LabelId = u16;

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

    /// Load from global variable.
    load_global: struct {
        dst: VReg,
        global_idx: GlobalIndex,
        width: Width,
    },

    /// Store to global variable.
    store_global: struct {
        src: VReg,
        global_idx: GlobalIndex,
        width: Width,
    },

    /// Load from local variable (stack slot).
    load_local: struct {
        dst: VReg,
        offset: i16, // negative offset from fp
        width: Width,
    },

    /// Store to local variable (stack slot).
    store_local: struct {
        src: VReg,
        offset: i16, // negative offset from fp
        width: Width,
    },

    /// Store function argument to stack slot.
    /// Used at function entry to spill argument registers.
    store_arg: struct {
        arg_idx: u8, // argument index (0-7 for x0-x7)
        offset: i16, // stack offset from fp
        width: Width,
    },

    /// Function prologue (save frame pointer, etc).
    prologue,

    /// Function epilogue (restore frame pointer, etc).
    epilogue,

    /// Call a function.
    call: struct {
        dst: ?VReg, // destination for return value (null for void)
        func_name: []const u8, // function name
        args: []const VReg, // argument registers (in order)
        arg_widths: []const Width, // argument types (parallel to args)
        call_conv: CallingConvention,
        width: Width, // return value width
    },

    /// Label (branch target).
    label: LabelId,

    /// Conditional branch: if vreg != 0 goto true_label, else goto false_label.
    br_cond: struct {
        cond: VReg,
        true_label: LabelId,
        false_label: LabelId,
    },

    /// Unconditional branch.
    br: LabelId,

    /// Load a field from a struct pointer via GEP.
    load_field: struct {
        dst: VReg,
        base: VReg, // pointer to struct
        struct_idx: u32, // index into TypeRegistry.struct_types
        field_idx: u32, // field index within the struct
        width: Width, // width of the loaded field
    },

    /// Allocate a struct on the stack.
    alloca_struct: struct {
        dst: VReg, // pointer to allocated struct
        struct_idx: u32, // index into TypeRegistry.struct_types
    },

    /// Store a value to a struct field via GEP.
    store_field: struct {
        base: VReg, // pointer to struct
        value: VReg, // value to store
        struct_idx: u32, // index into TypeRegistry.struct_types
        field_idx: u32, // field index within the struct
        width: Width, // width of the stored field
    },
};

/// Function parameter metadata.
pub const ParamInfo = struct {
    name: []const u8,
    width: Width,
};

/// External function declaration (no body).
pub const ExternFunc = struct {
    name: []const u8,
    call_conv: CallingConvention,
    return_width: ?Width, // null means void
    param_widths: []const Width,
};

/// A function in MIR form.
pub const MIRFunction = struct {
    name: []const u8,
    call_conv: CallingConvention,
    return_width: ?Width, // null means void
    params: []const ParamInfo,
    instructions: std.ArrayListUnmanaged(MInst),
    next_vreg: VReg,
    next_label: LabelId = 0,
    allocator: mem.Allocator,
    frame_size: u16 = 0, // stack frame size for locals (16-byte aligned)

    pub fn init(
        allocator: mem.Allocator,
        name: []const u8,
        call_conv: CallingConvention,
        return_width: ?Width,
        params: []const ParamInfo,
    ) MIRFunction {
        return .{
            .name = name,
            .call_conv = call_conv,
            .return_width = return_width,
            .params = params,
            .instructions = .{},
            .next_vreg = 0,
            .next_label = 0,
            .allocator = allocator,
            .frame_size = 0,
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

    /// Emit a load from global variable and return the destination vreg.
    pub fn emitLoadGlobal(self: *MIRFunction, global_idx: GlobalIndex, width: Width) !VReg {
        const dst = self.allocVReg();
        try self.emit(.{ .load_global = .{ .dst = dst, .global_idx = global_idx, .width = width } });
        return dst;
    }

    /// Emit a store to global variable.
    pub fn emitStoreGlobal(self: *MIRFunction, src: VReg, global_idx: GlobalIndex, width: Width) !void {
        try self.emit(.{ .store_global = .{ .src = src, .global_idx = global_idx, .width = width } });
    }

    /// Emit a load from local variable and return the destination vreg.
    pub fn emitLoadLocal(self: *MIRFunction, offset: i16, width: Width) !VReg {
        const dst = self.allocVReg();
        try self.emit(.{ .load_local = .{ .dst = dst, .offset = offset, .width = width } });
        return dst;
    }

    /// Emit a store to local variable.
    pub fn emitStoreLocal(self: *MIRFunction, src: VReg, offset: i16, width: Width) !void {
        try self.emit(.{ .store_local = .{ .src = src, .offset = offset, .width = width } });
    }

    /// Emit a store from argument register to stack slot.
    pub fn emitStoreArg(self: *MIRFunction, arg_idx: u8, offset: i16, width: Width) !void {
        try self.emit(.{ .store_arg = .{ .arg_idx = arg_idx, .offset = offset, .width = width } });
    }

    /// Allocate a new label ID.
    pub fn allocLabel(self: *MIRFunction) LabelId {
        const id = self.next_label;
        self.next_label += 1;
        return id;
    }

    /// Emit a label.
    pub fn emitLabel(self: *MIRFunction, id: LabelId) !void {
        try self.emit(.{ .label = id });
    }

    /// Emit a conditional branch.
    pub fn emitBrCond(self: *MIRFunction, cond: VReg, true_label: LabelId, false_label: LabelId) !void {
        try self.emit(.{ .br_cond = .{ .cond = cond, .true_label = true_label, .false_label = false_label } });
    }

    /// Emit an unconditional branch.
    pub fn emitBr(self: *MIRFunction, target: LabelId) !void {
        try self.emit(.{ .br = target });
    }

    /// Emit a load_field instruction (GEP + load from struct pointer).
    pub fn emitLoadField(self: *MIRFunction, base: VReg, struct_idx: u32, field_idx: u32, width: Width) !VReg {
        const dst = self.allocVReg();
        try self.emit(.{ .load_field = .{
            .dst = dst,
            .base = base,
            .struct_idx = struct_idx,
            .field_idx = field_idx,
            .width = width,
        } });
        return dst;
    }

    /// Emit an alloca_struct instruction and return the pointer vreg.
    pub fn emitAllocaStruct(self: *MIRFunction, struct_idx: u32) !VReg {
        const dst = self.allocVReg();
        try self.emit(.{ .alloca_struct = .{
            .dst = dst,
            .struct_idx = struct_idx,
        } });
        return dst;
    }

    /// Emit a store_field instruction (GEP + store to struct field).
    pub fn emitStoreField(self: *MIRFunction, base: VReg, value: VReg, struct_idx: u32, field_idx: u32, width: Width) !void {
        try self.emit(.{ .store_field = .{
            .base = base,
            .value = value,
            .struct_idx = struct_idx,
            .field_idx = field_idx,
            .width = width,
        } });
    }

    /// Emit a function call and return the destination vreg (null for void).
    pub fn emitCall(
        self: *MIRFunction,
        func_name: []const u8,
        args: []const VReg,
        arg_widths: []const Width,
        call_conv: CallingConvention,
        return_width: ?Width,
    ) !?VReg {
        const dst: ?VReg = if (return_width != null) self.allocVReg() else null;
        const args_copy = try self.allocator.dupe(VReg, args);
        const widths_copy = try self.allocator.dupe(Width, arg_widths);
        try self.emit(.{ .call = .{
            .dst = dst,
            .func_name = func_name,
            .args = args_copy,
            .arg_widths = widths_copy,
            .call_conv = call_conv,
            .width = return_width orelse .w32,
        } });
        return dst;
    }
};

/// Global variable storage (struct of arrays).
pub const GlobalVars = struct {
    names: std.ArrayListUnmanaged([]const u8),
    widths: std.ArrayListUnmanaged(Width),
    init_values: std.ArrayListUnmanaged(?i64), // null if needs runtime init
    sym_indices: std.ArrayListUnmanaged(SymbolIndex),
    name_map: std.StringHashMapUnmanaged(GlobalIndex),

    pub fn init() GlobalVars {
        return .{
            .names = .{},
            .widths = .{},
            .init_values = .{},
            .sym_indices = .{},
            .name_map = .{},
        };
    }

    pub fn deinit(self: *GlobalVars, allocator: mem.Allocator) void {
        self.names.deinit(allocator);
        self.widths.deinit(allocator);
        self.init_values.deinit(allocator);
        self.sym_indices.deinit(allocator);
        self.name_map.deinit(allocator);
    }

    pub fn add(
        self: *GlobalVars,
        allocator: mem.Allocator,
        name: []const u8,
        width: Width,
        init_value: ?i64,
        sym_idx: SymbolIndex,
    ) !GlobalIndex {
        const idx: GlobalIndex = @intCast(self.names.items.len);
        try self.names.append(allocator, name);
        try self.widths.append(allocator, width);
        try self.init_values.append(allocator, init_value);
        try self.sym_indices.append(allocator, sym_idx);
        try self.name_map.put(allocator, name, idx);
        return idx;
    }

    pub fn lookup(self: *const GlobalVars, name: []const u8) ?GlobalIndex {
        return self.name_map.get(name);
    }

    pub fn count(self: *const GlobalVars) usize {
        return self.names.items.len;
    }

    pub fn getName(self: *const GlobalVars, idx: GlobalIndex) []const u8 {
        return self.names.items[idx];
    }

    pub fn getWidth(self: *const GlobalVars, idx: GlobalIndex) Width {
        return self.widths.items[idx];
    }

    pub fn getInitValue(self: *const GlobalVars, idx: GlobalIndex) ?i64 {
        return self.init_values.items[idx];
    }

    pub fn needsRuntimeInit(self: *const GlobalVars, idx: GlobalIndex) bool {
        return self.init_values.items[idx] == null;
    }
};

/// Collection of MIR functions for a compilation unit.
pub const MIRModule = struct {
    functions: std.ArrayListUnmanaged(MIRFunction),
    extern_functions: std.ArrayListUnmanaged(ExternFunc),
    globals: GlobalVars,
    allocator: mem.Allocator,

    pub fn init(allocator: mem.Allocator) MIRModule {
        return .{
            .functions = .{},
            .extern_functions = .{},
            .globals = GlobalVars.init(),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *MIRModule) void {
        for (self.functions.items) |*func| {
            func.deinit();
        }
        self.functions.deinit(self.allocator);
        self.extern_functions.deinit(self.allocator);
        self.globals.deinit(self.allocator);
    }

    pub fn addFunction(
        self: *MIRModule,
        name: []const u8,
        call_conv: CallingConvention,
        return_width: ?Width,
        params: []const ParamInfo,
    ) !*MIRFunction {
        try self.functions.append(self.allocator, MIRFunction.init(self.allocator, name, call_conv, return_width, params));
        return &self.functions.items[self.functions.items.len - 1];
    }

    pub fn addExternFunction(
        self: *MIRModule,
        name: []const u8,
        call_conv: CallingConvention,
        return_width: ?Width,
        param_widths: []const Width,
    ) !void {
        try self.extern_functions.append(self.allocator, .{
            .name = name,
            .call_conv = call_conv,
            .return_width = return_width,
            .param_widths = param_widths,
        });
    }
};
