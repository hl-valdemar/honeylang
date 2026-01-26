const std = @import("std");
const mem = std.mem;

const ComptimeResult = @import("../comptime/comptime.zig").ComptimeResult;
const SymbolTable = @import("../semantic/symbols.zig").SymbolTable;
const SymbolIndex = @import("../semantic/symbols.zig").SymbolIndex;
const TypeId = @import("../semantic/types.zig").TypeId;
const PrimitiveType = @import("../semantic/types.zig").PrimitiveType;
const Ast = @import("../parser/ast.zig").Ast;
const NodeIndex = @import("../parser/ast.zig").NodeIndex;
const NodeKind = @import("../parser/ast.zig").NodeKind;
const TokenList = @import("../lexer/token.zig").TokenList;
const SourceCode = @import("../source/source.zig").SourceCode;

const mir = @import("mir.zig");
const MIRModule = mir.MIRModule;
const MIRFunction = mir.MIRFunction;
const MInst = mir.MInst;
const VReg = mir.VReg;
const Width = mir.Width;
const BinOp = mir.BinOp;
const GlobalIndex = mir.GlobalIndex;

const arm64 = @import("aarch64.zig");
const llvm = @import("llvm.zig");

pub const linker = @import("linker.zig");

pub const Arch = enum {
    aarch64,
    x86_64,
    llvm, // meta-target that defers to LLVM toolchain
};

pub const Os = enum {
    darwin,
    linux,
};

pub const Target = struct {
    arch: Arch,
    os: Os,

    /// Get the LLVM target triple for this target.
    pub fn getLLVMTriple(self: Target) []const u8 {
        const builtin = @import("builtin");

        // For .llvm pseudo-arch, use the native CPU architecture
        const effective_arch: Arch = if (self.arch == .llvm)
            switch (builtin.cpu.arch) {
                .aarch64 => .aarch64,
                .x86_64 => .x86_64,
                else => .x86_64, // fallback
            }
        else
            self.arch;

        return switch (self.os) {
            .darwin => switch (effective_arch) {
                .aarch64 => "arm64-apple-darwin",
                .x86_64, .llvm => "x86_64-apple-darwin",
            },
            .linux => switch (effective_arch) {
                .aarch64 => "aarch64-unknown-linux-gnu",
                .x86_64, .llvm => "x86_64-unknown-linux-gnu",
            },
        };
    }

    /// Check if this target requires LLVM (no native backend).
    pub fn requiresLLVM(self: Target) bool {
        return self.arch == .x86_64 or self.arch == .llvm;
    }
};

pub fn generate(
    allocator: mem.Allocator,
    target: Target,
    comptime_result: *const ComptimeResult,
    symbols: *const SymbolTable,
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
) !CodeGenResult {
    // generate MIR (architecture-independent)
    var ctx = CodeGenContext.init(
        allocator,
        comptime_result,
        symbols,
        ast,
        tokens,
        src,
    );

    try ctx.generate();

    // lower MIR to assembly/IR (architecture-specific)
    const output = switch (target.arch) {
        .aarch64 => LoweringResult{
            .output = try arm64.lower(allocator, &ctx.mir, target.os),
            .is_llvm_ir = false,
        },
        .x86_64, .llvm => LoweringResult{
            .output = try llvm.lower(allocator, &ctx.mir, target),
            .is_llvm_ir = true,
        },
    };

    return .{
        .output = output.output,
        .is_llvm_ir = output.is_llvm_ir,
        .mir = ctx.mir,
    };
}

pub const CodeGenContext = struct {
    allocator: mem.Allocator,
    comptime_result: *const ComptimeResult,
    symbols: *const SymbolTable,
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    mir: MIRModule,
    current_func: ?*MIRFunction,

    pub fn init(
        allocator: mem.Allocator,
        comptime_result: *const ComptimeResult,
        symbols: *const SymbolTable,
        ast: *const Ast,
        tokens: *const TokenList,
        src: *const SourceCode,
    ) CodeGenContext {
        return .{
            .allocator = allocator,
            .comptime_result = comptime_result,
            .symbols = symbols,
            .ast = ast,
            .tokens = tokens,
            .src = src,
            .mir = MIRModule.init(allocator),
            .current_func = null,
        };
    }

    pub fn deinit(self: *CodeGenContext) void {
        self.mir.deinit();
    }

    pub fn generate(self: *CodeGenContext) !void {
        const program = self.ast.getProgram(self.ast.root);
        const declarations = self.ast.getExtra(program.declarations);

        // Phase 1: Collect global variables
        var globals_needing_init = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 16);
        defer globals_needing_init.deinit(self.allocator);

        for (declarations) |decl_idx| {
            const kind = self.ast.getKind(decl_idx);
            if (kind == .var_decl) {
                try self.collectGlobalVar(decl_idx, &globals_needing_init);
            }
        }

        // Phase 2: Generate __honey_init (always, for runtime startup compatibility)
        try self.generateGlobalInit(globals_needing_init.items);

        // Phase 3: Generate user functions
        for (declarations) |decl_idx| {
            try self.generateDeclaration(decl_idx);
        }
    }

    fn collectGlobalVar(self: *CodeGenContext, node_idx: NodeIndex, needs_init: *std.ArrayList(NodeIndex)) !void {
        const var_decl = self.ast.getVarDecl(node_idx);
        const name_ident = self.ast.getIdentifier(var_decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        const sym_idx = self.symbols.lookup(name) orelse return;
        const type_id = self.symbols.getTypeId(sym_idx);
        const width = typeIdToWidth(type_id);

        // Try to evaluate initializer as a literal constant
        const init_value = self.tryEvaluateLiteral(var_decl.value);

        // Register global in MIR
        _ = try self.mir.globals.add(
            self.allocator,
            name,
            width,
            init_value,
            sym_idx,
        );

        // If needs runtime init, add to list
        if (init_value == null) {
            try needs_init.append(self.allocator, node_idx);
        }
    }

    fn tryEvaluateLiteral(self: *CodeGenContext, node_idx: NodeIndex) ?i64 {
        const kind = self.ast.getKind(node_idx);
        if (kind != .literal) return null;

        const lit = self.ast.getLiteral(node_idx);
        const token = self.tokens.items[lit.token_idx];
        const value_str = self.src.getSlice(token.start, token.start + token.len);

        return switch (token.kind) {
            .bool => if (std.mem.eql(u8, value_str, "true")) 1 else 0,
            .number => std.fmt.parseInt(i64, value_str, 10) catch null,
            else => null,
        };
    }

    fn generateGlobalInit(self: *CodeGenContext, var_nodes: []const NodeIndex) !void {
        // Create __honey_init function
        self.current_func = try self.mir.addFunction("__honey_init", .c);
        try self.current_func.?.emit(.prologue);

        for (var_nodes) |node_idx| {
            const var_decl = self.ast.getVarDecl(node_idx);
            const name_ident = self.ast.getIdentifier(var_decl.name);
            const name_token = self.tokens.items[name_ident.token_idx];
            const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

            const global_idx = self.mir.globals.lookup(name) orelse continue;
            const width = self.mir.globals.getWidth(global_idx);

            // Evaluate initializer expression
            const value_reg = try self.generateExpression(var_decl.value) orelse continue;

            // Store to global
            try self.current_func.?.emitStoreGlobal(value_reg, global_idx, width);
        }

        // Return void (emit ret with null value)
        try self.current_func.?.emitRet(null, .w32);
        self.current_func = null;
    }

    fn generateDeclaration(self: *CodeGenContext, node_idx: NodeIndex) !void {
        const kind = self.ast.getKind(node_idx);

        switch (kind) {
            .func_decl => try self.generateFunction(node_idx),
            .const_decl, .var_decl => {
                // global constants/variables handled at data section
                // for now, skip (comptime evaluates constants)
            },
            else => {},
        }
    }

    fn generateFunction(self: *CodeGenContext, node_idx: NodeIndex) !void {
        const func = self.ast.getFuncDecl(node_idx);

        // skip external functions (no body)
        if (func.body == null) return;

        // get func name
        const name_ident = self.ast.getIdentifier(func.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const func_name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        // calling conv should be c if main (for darwin)
        const call_conv = if (std.mem.eql(u8, func_name, "main")) .c else func.call_conv;

        // create MIR function
        self.current_func = try self.mir.addFunction(func_name, call_conv);

        // emit function prologue and body
        try self.current_func.?.emit(.prologue);
        const has_return = try self.generateBlock(func.body.?);

        if (!has_return) {
            // implicit return 0 for main, void return otherwise
            const zero = try self.current_func.?.emitMovImm(0, .w32);
            try self.current_func.?.emitRet(zero, .w32);
        }

        self.current_func = null;
    }

    fn generateBlock(self: *CodeGenContext, node_idx: NodeIndex) !bool {
        var has_return = false;

        const block = self.ast.getBlock(node_idx);
        const statements = self.ast.getExtra(block.statements);
        const deferred = self.ast.getExtra(block.deferred);

        for (statements) |stmt_idx| {
            const kind = try self.generateStatement(stmt_idx);
            has_return = has_return or (kind == .return_stmt);
        }

        // execute deferred statements in reverse order
        var i = deferred.len;
        while (i > 0) {
            i -= 1;
            _ = try self.generateStatement(deferred[i]);
        }

        return has_return;
    }

    fn generateStatement(self: *CodeGenContext, node_idx: NodeIndex) !?NodeKind {
        const kind = self.ast.getKind(node_idx);
        switch (kind) {
            .return_stmt => try self.generateReturn(node_idx),
            else => return null,
        }
        return kind;
    }

    fn generateReturn(self: *CodeGenContext, node_idx: NodeIndex) !void {
        const ret = self.ast.getReturn(node_idx);
        const func = self.current_func.?;

        const result_reg = try self.generateExpression(ret.expr);

        // TODO: determine width from return type
        try func.emitRet(result_reg, .w32);
    }

    fn generateExpression(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?VReg {
        const kind = self.ast.getKind(node_idx);
        return switch (kind) {
            .literal => try self.generateLiteral(node_idx),
            .identifier => try self.generateIdentifier(node_idx),
            .binary_op => try self.generateBinaryOp(node_idx),
            else => null,
        };
    }

    fn generateLiteral(self: *CodeGenContext, node_idx: NodeIndex) !?VReg {
        const func = self.current_func.?;
        const lit = self.ast.getLiteral(node_idx);
        const token = self.tokens.items[lit.token_idx];
        const value_str = self.src.getSlice(token.start, token.start + token.len);

        switch (token.kind) {
            .bool => {
                const value: i64 = if (std.mem.eql(u8, value_str, "true")) 1 else 0;
                return try func.emitMovImm(value, .w32);
            },
            .number => {
                // FIXME: determine actual type from semantic analysis
                const value = std.fmt.parseInt(i64, value_str, 10) catch 0;
                return try func.emitMovImm(value, .w32);
            },
            else => return null,
        }
    }

    fn generateIdentifier(self: *CodeGenContext, node_idx: NodeIndex) !?VReg {
        const ident = self.ast.getIdentifier(node_idx);
        const token = self.tokens.items[ident.token_idx];
        const name = self.src.getSlice(token.start, token.start + token.len);

        const sym_idx = self.symbols.lookup(name) orelse return null;
        const type_id = self.symbols.getTypeId(sym_idx);
        const kind = self.symbols.getKind(sym_idx);

        return switch (kind) {
            .constant => self.generateConstLoad(sym_idx, type_id),
            .variable => self.generateVarLoad(name, type_id),
            .function => error.UnsupportedFeature,
        };
    }

    fn generateVarLoad(self: *CodeGenContext, name: []const u8, type_id: TypeId) !?VReg {
        const func = self.current_func.?;
        const global_idx = self.mir.globals.lookup(name) orelse return null;
        const width = typeIdToWidth(type_id);
        return try func.emitLoadGlobal(global_idx, width);
    }

    fn typeIdToWidth(type_id: TypeId) Width {
        return switch (type_id) {
            .primitive => |prim| switch (prim) {
                .i64, .u64, .f64 => .w64,
                else => .w32,
            },
            else => .w32,
        };
    }

    fn generateConstLoad(self: *CodeGenContext, sym_idx: SymbolIndex, type_id: TypeId) !?VReg {
        const eval_state = self.comptime_result.eval_states.items[sym_idx];
        if (eval_state != .evaluated) return null;

        const value_str = self.comptime_result.getEvalLiteral(sym_idx) orelse return null;

        return switch (type_id) {
            .primitive => |prim| self.generatePrimitiveImmediate(prim, value_str),
            else => null,
        };
    }

    fn generatePrimitiveImmediate(self: *CodeGenContext, prim: PrimitiveType, value_str: []const u8) !?VReg {
        const func = self.current_func.?;

        switch (prim) {
            .bool => {
                const val: i64 = if (std.mem.eql(u8, value_str, "true")) 1 else 0;
                return try func.emitMovImm(val, .w32);
            },
            .i8, .i16, .i32, .u8, .u16, .u32 => {
                const val = std.fmt.parseInt(i64, value_str, 10) catch 0;
                return try func.emitMovImm(val, .w32);
            },
            .i64, .u64 => {
                const val = std.fmt.parseInt(i64, value_str, 10) catch 0;
                return try func.emitMovImm(val, .w64);
            },
            .f16, .f32, .f64 => {
                return error.UnsupportedFeature;
            },
            .void => return null,
        }
    }

    fn generateBinaryOp(self: *CodeGenContext, node_idx: NodeIndex) !?VReg {
        const func = self.current_func.?;
        const binary = self.ast.getBinaryOp(node_idx);

        const left_reg = try self.generateExpression(binary.left) orelse return null;
        const right_reg = try self.generateExpression(binary.right) orelse return null;

        const mir_op: BinOp = switch (binary.op) {
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .div => .div_s, // TODO: signed vs unsigned based on type
            else => return error.UnsupportedFeature,
        };

        // TODO: determine width from type
        return try func.emitBinOp(mir_op, left_reg, right_reg, .w32);
    }
};

pub const CodeGenError = error{
    OutOfMemory,
    UnsupportedFeature,
};

pub const CodeGenResult = struct {
    output: []const u8, // assembly or LLVM IR depending on target
    is_llvm_ir: bool,
    mir: MIRModule,
};

const LoweringResult = struct {
    output: []const u8,
    is_llvm_ir: bool,
};
