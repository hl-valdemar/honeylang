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

const arm64 = @import("aarch64.zig");

pub const linker = @import("linker.zig");

pub const Target = enum {
    aarch64,
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

    // lower MIR to assembly (architecture-specific)
    const assembly = switch (target) {
        .aarch64 => try arm64.lower(allocator, &ctx.module),
    };

    return .{
        .assembly = assembly,
        .mir = ctx.module,
    };
}

pub const CodeGenContext = struct {
    allocator: mem.Allocator,
    comptime_result: *const ComptimeResult,
    symbols: *const SymbolTable,
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    module: MIRModule,
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
            .module = MIRModule.init(allocator),
            .current_func = null,
        };
    }

    pub fn deinit(self: *CodeGenContext) void {
        self.module.deinit();
    }

    pub fn generate(self: *CodeGenContext) !void {
        const program = self.ast.getProgram(self.ast.root);
        const declarations = self.ast.getExtra(program.declarations);

        for (declarations) |decl_idx| {
            try self.generateDeclaration(decl_idx);
        }
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

        // calling conv should be c if main
        const call_conv = if (std.mem.eql(u8, func_name, "main")) .c else func.call_conv;

        // create MIR function
        self.current_func = try self.module.addFunction(func_name, call_conv);

        // emit body
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
            .variable => error.UnsupportedFeature,
            .function => error.UnsupportedFeature,
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
    assembly: []const u8,
    mir: MIRModule,
};
