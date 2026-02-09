const std = @import("std");
const mem = std.mem;

const ComptimeResult = @import("../comptime/comptime.zig").ComptimeResult;
const SymbolTable = @import("../semantic/symbols.zig").SymbolTable;
const SymbolIndex = @import("../semantic/symbols.zig").SymbolIndex;
const TypeId = @import("../semantic/types.zig").TypeId;
const PrimitiveType = @import("../semantic/types.zig").PrimitiveType;
const TypeRegistry = @import("../semantic/types.zig").TypeRegistry;
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

const llvm = @import("llvm.zig");

pub const linker = @import("linker.zig");

/// Local variable storage for a function (struct of arrays).
pub const LocalVars = struct {
    offsets: std.ArrayListUnmanaged(i16),
    widths: std.ArrayListUnmanaged(Width),
    name_map: std.StringHashMapUnmanaged(u16), // name -> index
    next_offset: i16 = -8, // first local at fp-8

    pub fn init() LocalVars {
        return .{
            .offsets = .{},
            .widths = .{},
            .name_map = .{},
            .next_offset = -8,
        };
    }

    pub fn deinit(self: *LocalVars, allocator: mem.Allocator) void {
        self.offsets.deinit(allocator);
        self.widths.deinit(allocator);
        self.name_map.deinit(allocator);
    }

    pub fn add(
        self: *LocalVars,
        allocator: mem.Allocator,
        name: []const u8,
        width: Width,
    ) !i16 {
        const idx: u16 = @intCast(self.offsets.items.len);
        const offset = self.next_offset;

        try self.offsets.append(allocator, offset);
        try self.widths.append(allocator, width);
        try self.name_map.put(allocator, name, idx);

        // Advance offset for next local (8-byte slots for simplicity)
        self.next_offset -= 8;

        return offset;
    }

    pub const LocalInfo = struct {
        offset: i16,
        width: Width,
    };

    pub fn lookup(self: *const LocalVars, name: []const u8) ?LocalInfo {
        const idx = self.name_map.get(name) orelse return null;
        return .{
            .offset = self.offsets.items[idx],
            .width = self.widths.items[idx],
        };
    }

    /// Get frame size aligned to 16 bytes.
    pub fn frameSize(self: *const LocalVars) u16 {
        if (self.offsets.items.len == 0) return 0;
        // next_offset is negative, e.g., -24 means we used 16 bytes
        const used: u16 = @intCast(-self.next_offset - 8 + 8); // bytes used
        // Round up to 16-byte alignment
        return (used + 15) & ~@as(u16, 15);
    }

    pub fn reset(self: *LocalVars) void {
        self.offsets.clearRetainingCapacity();
        self.widths.clearRetainingCapacity();
        self.name_map.clearRetainingCapacity();
        self.next_offset = -8;
    }
};

pub const Arch = enum {
    aarch64,
    x86_64,
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
        return switch (self.os) {
            .darwin => switch (self.arch) {
                .aarch64 => "arm64-apple-darwin",
                .x86_64 => "x86_64-apple-darwin",
            },
            .linux => switch (self.arch) {
                .aarch64 => "aarch64-unknown-linux-gnu",
                .x86_64 => "x86_64-unknown-linux-gnu",
            },
        };
    }
};

pub fn generate(
    allocator: mem.Allocator,
    target: Target,
    comptime_result: *const ComptimeResult,
    symbols: *const SymbolTable,
    types: *const TypeRegistry,
    node_types: *const std.AutoHashMapUnmanaged(NodeIndex, TypeId),
    skip_nodes: *const std.AutoHashMapUnmanaged(NodeIndex, void),
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
) !CodeGenResult {
    // generate MIR (architecture-independent)
    var ctx = CodeGenContext.init(
        allocator,
        comptime_result,
        symbols,
        types,
        node_types,
        skip_nodes,
        ast,
        tokens,
        src,
    );

    try ctx.generate();

    // lower MIR to LLVM IR
    const output = try llvm.lower(allocator, &ctx.mir, types, target);

    return .{
        .output = output,
        .mir = ctx.mir,
    };
}

pub const CodeGenContext = struct {
    allocator: mem.Allocator,
    comptime_result: *const ComptimeResult,
    symbols: *const SymbolTable,
    types: *const TypeRegistry,
    node_types: *const std.AutoHashMapUnmanaged(NodeIndex, TypeId),
    skip_nodes: *const std.AutoHashMapUnmanaged(NodeIndex, void),
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    mir: MIRModule,
    current_func: ?*MIRFunction,
    locals: LocalVars,

    pub fn init(
        allocator: mem.Allocator,
        comptime_result: *const ComptimeResult,
        symbols: *const SymbolTable,
        types: *const TypeRegistry,
        node_types: *const std.AutoHashMapUnmanaged(NodeIndex, TypeId),
        skip_nodes: *const std.AutoHashMapUnmanaged(NodeIndex, void),
        ast: *const Ast,
        tokens: *const TokenList,
        src: *const SourceCode,
    ) CodeGenContext {
        return .{
            .allocator = allocator,
            .comptime_result = comptime_result,
            .symbols = symbols,
            .types = types,
            .node_types = node_types,
            .skip_nodes = skip_nodes,
            .ast = ast,
            .tokens = tokens,
            .src = src,
            .mir = MIRModule.init(allocator),
            .current_func = null,
            .locals = LocalVars.init(),
        };
    }

    pub fn deinit(self: *CodeGenContext) void {
        self.mir.deinit();
        self.locals.deinit(self.allocator);
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
        // skip unused global variables
        if (self.skip_nodes.contains(node_idx)) return;

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
        // Create __honey_init function (void, no params)
        self.current_func = try self.mir.addFunction("__honey_init", .c, null, &.{});
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
        // skip declarations marked as unused by semantic analysis
        if (self.skip_nodes.contains(node_idx)) return;

        const kind = self.ast.getKind(node_idx);

        switch (kind) {
            .func_decl => {
                const func = self.ast.getFuncDecl(node_idx);
                if (func.body == null) {
                    // extern function — collect as declaration
                    try self.collectExternFunction(node_idx);
                } else {
                    try self.generateFunction(node_idx);
                }
            },
            .const_decl, .var_decl => {
                // global constants/variables handled at data section
                // for now, skip (comptime evaluates constants)
            },
            .struct_decl => {
                // struct declarations are type definitions, handled at LLVM level
            },
            else => {},
        }
    }

    fn collectExternFunction(self: *CodeGenContext, node_idx: NodeIndex) !void {
        const func = self.ast.getFuncDecl(node_idx);

        // get func name
        const name_ident = self.ast.getIdentifier(func.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const func_name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        // resolve return type from node_types (stored by semantic analysis)
        const return_width: ?Width = resolveReturnWidth(self.node_types, func.return_type);

        // resolve parameter widths
        const param_nodes = self.ast.getExtra(func.params);
        const param_count = param_nodes.len / 2;
        var param_widths = try std.ArrayList(Width).initCapacity(self.allocator, param_count);
        defer param_widths.deinit(self.allocator);

        var i: usize = 0;
        while (i < param_nodes.len) : (i += 2) {
            const param_name_idx = param_nodes[i];
            const param_type = self.node_types.get(param_name_idx) orelse .unresolved;
            try param_widths.append(self.allocator, typeIdToWidth(param_type));
        }

        try self.mir.addExternFunction(
            func_name,
            func.call_conv,
            return_width,
            try self.allocator.dupe(Width, param_widths.items),
        );
    }

    fn generateFunction(self: *CodeGenContext, node_idx: NodeIndex) !void {
        const func = self.ast.getFuncDecl(node_idx);

        // get func name
        const name_ident = self.ast.getIdentifier(func.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const func_name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        // calling conv should be c if main (for darwin)
        const call_conv = if (std.mem.eql(u8, func_name, "main")) .c else func.call_conv;

        // resolve return type from node_types (stored by semantic analysis)
        const return_width: ?Width = resolveReturnWidth(self.node_types, func.return_type);

        // build parameter info
        const param_nodes = self.ast.getExtra(func.params);
        const param_count = param_nodes.len / 2;
        var param_list = try std.ArrayList(mir.ParamInfo).initCapacity(self.allocator, param_count);
        defer param_list.deinit(self.allocator);

        {
            var i: usize = 0;
            while (i < param_nodes.len) : (i += 2) {
                const param_name_idx = param_nodes[i];
                const param_ident = self.ast.getIdentifier(param_name_idx);
                const param_token = self.tokens.items[param_ident.token_idx];
                const param_name = self.src.getSlice(param_token.start, param_token.start + param_token.len);
                const param_type = self.node_types.get(param_name_idx) orelse .unresolved;
                try param_list.append(self.allocator, .{ .name = param_name, .width = typeIdToWidth(param_type) });
            }
        }

        // reset locals for new function
        self.locals.reset();

        // create MIR function
        self.current_func = try self.mir.addFunction(
            func_name,
            call_conv,
            return_width,
            try self.allocator.dupe(mir.ParamInfo, param_list.items),
        );

        // emit function prologue
        try self.current_func.?.emit(.prologue);

        // register parameters as locals and spill to stack
        const func_params = self.current_func.?.params;
        for (func_params, 0..) |param, arg_i| {
            const offset = try self.locals.add(self.allocator, param.name, param.width);
            try self.current_func.?.emitStoreArg(@intCast(arg_i), offset, param.width);
        }

        // generate function body
        const has_return = try self.generateBlock(func.body.?);

        if (!has_return) {
            if (return_width) |rw| {
                // non-void function: implicit return 0
                const zero = try self.current_func.?.emitMovImm(0, rw);
                try self.current_func.?.emitRet(zero, rw);
            } else {
                // void function: implicit return void
                try self.current_func.?.emitRet(null, .w32);
            }
        }

        // set frame size for locals
        self.current_func.?.frame_size = self.locals.frameSize();

        self.current_func = null;
    }

    fn generateBlock(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!bool {
        var has_return = false;

        const block = self.ast.getBlock(node_idx);
        const statements = self.ast.getExtra(block.statements);
        const deferred = self.ast.getExtra(block.deferred);

        for (statements) |stmt_idx| {
            const kind = self.ast.getKind(stmt_idx);

            if (kind == .return_stmt) {
                // Execute deferred statements BEFORE returning (LIFO order)
                try self.generateDeferredStatements(deferred);
                try self.generateReturn(stmt_idx);
                has_return = true;
            } else {
                _ = try self.generateStatement(stmt_idx);
            }
        }

        // If no explicit return, still execute deferred at block end
        if (!has_return and deferred.len > 0) {
            try self.generateDeferredStatements(deferred);
        }

        return has_return;
    }

    /// Generate deferred statements in reverse order (LIFO)
    fn generateDeferredStatements(self: *CodeGenContext, deferred: []const NodeIndex) CodeGenError!void {
        var i = deferred.len;
        while (i > 0) {
            i -= 1;
            _ = try self.generateStatement(deferred[i]);
        }
    }

    fn generateStatement(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?NodeKind {
        const kind = self.ast.getKind(node_idx);
        switch (kind) {
            .return_stmt => try self.generateReturn(node_idx),
            .var_decl => try self.generateLocalVarDecl(node_idx),
            .defer_stmt => {
                // unwrap defer and generate the inner statement
                const defer_node = self.ast.getDefer(node_idx);
                _ = try self.generateStatement(defer_node.stmt);
            },
            .assignment => try self.generateAssignment(node_idx),
            .if_stmt => try self.generateIf(node_idx),
            else => return null,
        }
        return kind;
    }

    fn generateLocalVarDecl(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!void {
        // skip declarations marked as unused by semantic analysis
        if (self.skip_nodes.contains(node_idx)) return;

        const var_decl = self.ast.getVarDecl(node_idx);
        const name_ident = self.ast.getIdentifier(var_decl.name);
        const name_token = self.tokens.items[name_ident.token_idx];
        const name = self.src.getSlice(name_token.start, name_token.start + name_token.len);

        // Get resolved type from sema — .unresolved here means a used-but-unresolvable
        // type (already reported as error S018). Use w32 as error recovery so the
        // compiler can still produce an executable.
        const type_id = self.node_types.get(node_idx) orelse .unresolved;
        const width = typeIdToWidth(type_id);

        // Allocate stack slot
        const offset = try self.locals.add(self.allocator, name, width);

        // Generate initializer expression
        const value_reg = try self.generateExpression(var_decl.value) orelse return;

        // Store to local
        try self.current_func.?.emitStoreLocal(value_reg, offset, width);
    }

    fn generateAssignment(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!void {
        const assign = self.ast.getAssignment(node_idx);
        const target_ident = self.ast.getIdentifier(assign.target);
        const target_token = self.tokens.items[target_ident.token_idx];
        const name = self.src.getSlice(target_token.start, target_token.start + target_token.len);

        // Generate the value expression (for `x += 4` this is the desugared `x + 4`)
        const value_reg = try self.generateExpression(assign.value) orelse return;

        // Try local first, then global
        if (self.locals.lookup(name)) |local| {
            try self.current_func.?.emitStoreLocal(value_reg, local.offset, local.width);
        } else if (self.mir.globals.lookup(name)) |global_idx| {
            const width = self.mir.globals.getWidth(global_idx);
            try self.current_func.?.emitStoreGlobal(value_reg, global_idx, width);
        }
    }

    fn generateIf(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!void {
        const if_node = self.ast.getIf(node_idx);
        const func = self.current_func.?;

        const end_label = func.allocLabel();

        // Generate the if guard
        const guard_reg = try self.generateExpression(if_node.if_guard) orelse return;

        const then_label = func.allocLabel();
        // Determine where to jump on false: first else-if, else block, or end
        const else_if_count = if_node.elseIfCount();
        const first_false_label = func.allocLabel();

        try func.emitBrCond(guard_reg, then_label, first_false_label);

        // Then block
        try func.emitLabel(then_label);
        const then_has_return = try self.generateBlock(if_node.if_block);
        if (!then_has_return) {
            try func.emitBr(end_label);
        }

        // Else-if chains
        var next_false_label = first_false_label;
        for (0..else_if_count) |i| {
            try func.emitLabel(next_false_label);

            const pair = if_node.getElseIf(self.ast, i) orelse continue;
            const elif_guard_reg = try self.generateExpression(pair.guard) orelse continue;

            const elif_then_label = func.allocLabel();
            next_false_label = func.allocLabel();

            try func.emitBrCond(elif_guard_reg, elif_then_label, next_false_label);

            try func.emitLabel(elif_then_label);
            const elif_has_return = try self.generateBlock(pair.block);
            if (!elif_has_return) {
                try func.emitBr(end_label);
            }
        }

        // Else block or fall-through
        try func.emitLabel(if (else_if_count > 0) next_false_label else first_false_label);
        if (if_node.else_block) |else_block| {
            const else_has_return = try self.generateBlock(else_block);
            if (!else_has_return) {
                try func.emitBr(end_label);
            }
        } else {
            try func.emitBr(end_label);
        }

        // End label — code after the if continues here
        try func.emitLabel(end_label);
    }

    fn generateReturn(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!void {
        const ret = self.ast.getReturn(node_idx);
        const func = self.current_func.?;
        const ret_width = func.return_width orelse .w32;

        if (self.ast.getKind(ret.expr) == .void_literal) {
            try func.emitRet(null, ret_width);
            return;
        }

        const result_reg = try self.generateExpression(ret.expr);
        try func.emitRet(result_reg, ret_width);
    }

    fn generateExpression(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?VReg {
        const kind = self.ast.getKind(node_idx);
        return switch (kind) {
            .literal => try self.generateLiteral(node_idx),
            .identifier => try self.generateIdentifier(node_idx),
            .binary_op => try self.generateBinaryOp(node_idx),
            .call_expr => try self.generateCallExpr(node_idx),
            .field_access => try self.generateFieldAccess(node_idx),
            .struct_literal => try self.generateStructLiteral(node_idx),
            else => null,
        };
    }

    fn generateLiteral(self: *CodeGenContext, node_idx: NodeIndex) !?VReg {
        const func = self.current_func.?;
        const lit = self.ast.getLiteral(node_idx);
        const token = self.tokens.items[lit.token_idx];
        const value_str = self.src.getSlice(token.start, token.start + token.len);

        // Resolve width from semantic analysis; bare unresolved literals default to w32
        const width = if (self.node_types.get(node_idx)) |tid| typeIdToWidth(tid) else Width.w32;

        switch (token.kind) {
            .bool => {
                const value: i64 = if (std.mem.eql(u8, value_str, "true")) 1 else 0;
                return try func.emitMovImm(value, width);
            },
            .number => {
                const value = std.fmt.parseInt(i64, value_str, 10) catch 0;
                return try func.emitMovImm(value, width);
            },
            else => return null,
        }
    }

    fn generateIdentifier(self: *CodeGenContext, node_idx: NodeIndex) !?VReg {
        const ident = self.ast.getIdentifier(node_idx);
        const token = self.tokens.items[ident.token_idx];
        const name = self.src.getSlice(token.start, token.start + token.len);

        // Check locals first (for function-scoped variables)
        if (self.locals.lookup(name)) |local| {
            return try self.current_func.?.emitLoadLocal(local.offset, local.width);
        }

        // Then check symbols (globals/constants)
        const sym_idx = self.symbols.lookup(name) orelse return null;
        const type_id = self.symbols.getTypeId(sym_idx);
        const kind = self.symbols.getKind(sym_idx);

        return switch (kind) {
            .constant => self.generateConstLoad(sym_idx, type_id),
            .variable => self.generateVarLoad(name, type_id),
            .function, .type_ => error.UnsupportedFeature,
        };
    }

    fn generateVarLoad(self: *CodeGenContext, name: []const u8, type_id: TypeId) !?VReg {
        const func = self.current_func.?;
        const global_idx = self.mir.globals.lookup(name) orelse return null;
        const width = typeIdToWidth(type_id);
        return try func.emitLoadGlobal(global_idx, width);
    }

    /// Map a resolved TypeId to a MIR operand width.
    /// .unresolved reaching codegen means semantic analysis reported an error (S018)
    /// but the compiler still produces an executable. w32 is used as error recovery.
    /// Struct types are passed by pointer, so they use .ptr width.
    fn typeIdToWidth(type_id: TypeId) Width {
        return switch (type_id) {
            .primitive => |prim| switch (prim) {
                .i64, .u64, .f64 => .w64,
                else => .w32,
            },
            .struct_type => .ptr,
            .unresolved => .w32,
            else => .w32,
        };
    }

    /// Resolve the return width for a function from the return type node.
    /// Returns null for void, w32/w64 for value types, w32 as fallback.
    fn resolveReturnWidth(
        node_types: *const std.AutoHashMapUnmanaged(NodeIndex, TypeId),
        return_type_node: NodeIndex,
    ) ?Width {
        const ret_type = node_types.get(return_type_node) orelse return .w32;
        if (ret_type == .primitive and ret_type.primitive == .void) return null;
        return typeIdToWidth(ret_type);
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

        // Resolve width from semantic analysis; default to w32
        const width = if (self.node_types.get(node_idx)) |tid| typeIdToWidth(tid) else Width.w32;
        return try func.emitBinOp(mir_op, left_reg, right_reg, width);
    }

    fn generateCallExpr(self: *CodeGenContext, node_idx: NodeIndex) !?VReg {
        const func = self.current_func.?;
        const call = self.ast.getCallExpr(node_idx);

        // 1. Get function name from identifier
        const func_ident = self.ast.getIdentifier(call.func);
        const func_token = self.tokens.items[func_ident.token_idx];
        const func_name = self.src.getSlice(func_token.start, func_token.start + func_token.len);

        // 2. Look up function to get calling convention
        const sym_idx = self.symbols.lookup(func_name) orelse return null;
        const value_node = self.symbols.getValueNode(sym_idx);
        const func_decl = self.ast.getFuncDecl(value_node);
        const call_conv = func_decl.call_conv;

        // 3. Generate argument expressions
        const arg_nodes = self.ast.getExtra(call.args);
        var arg_regs = try std.ArrayList(VReg).initCapacity(self.allocator, arg_nodes.len);
        defer arg_regs.deinit(self.allocator);

        for (arg_nodes) |arg_idx| {
            const arg_reg = try self.generateExpression(arg_idx) orelse {
                // If arg generation fails, use 0 as placeholder
                const zero = try func.emitMovImm(0, .w32);
                try arg_regs.append(self.allocator, zero);
                continue;
            };
            try arg_regs.append(self.allocator, arg_reg);
        }

        // 4. Determine return width from type (default to w32)
        const return_type = self.node_types.get(node_idx);
        const return_width: ?Width = if (return_type) |rt| blk: {
            if (rt == .unresolved) break :blk .w32;
            if (rt == .primitive and rt.primitive == .void) break :blk null;
            break :blk typeIdToWidth(rt);
        } else .w32;

        // 5. Emit call instruction
        return try func.emitCall(func_name, arg_regs.items, call_conv, return_width);
    }

    fn generateFieldAccess(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?VReg {
        const func = self.current_func.?;
        const access = self.ast.getFieldAccess(node_idx);

        // generate the base object expression (produces a pointer for struct types)
        const base_reg = try self.generateExpression(access.object) orelse return null;

        // get the object's type from semantic analysis
        const object_type = self.node_types.get(access.object) orelse return null;
        if (object_type != .struct_type) return null;

        const struct_idx = object_type.struct_type;
        const struct_type = self.types.getStructType(object_type) orelse return null;

        // get field name
        const field_ident = self.ast.getIdentifier(access.field);
        const field_token = self.tokens.items[field_ident.token_idx];
        const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);

        // find field index
        for (struct_type.fields, 0..) |field, i| {
            if (mem.eql(u8, field.name, field_name)) {
                const field_width = typeIdToWidth(field.type_id);
                return try func.emitLoadField(base_reg, struct_idx, @intCast(i), field_width);
            }
        }

        return null;
    }

    fn generateStructLiteral(self: *CodeGenContext, node_idx: NodeIndex) CodeGenError!?VReg {
        const func = self.current_func.?;
        const lit = self.ast.getStructLiteral(node_idx);

        // get struct type from semantic analysis
        const type_id = self.node_types.get(node_idx) orelse return null;
        if (type_id != .struct_type) return null;

        const struct_idx = type_id.struct_type;
        const struct_type = self.types.getStructType(type_id) orelse return null;

        // allocate struct on stack
        const base = try func.emitAllocaStruct(struct_idx);

        // store each field value
        const field_data = self.ast.getExtra(lit.fields);
        var fi: usize = 0;
        while (fi < field_data.len) : (fi += 2) {
            const field_name_idx = field_data[fi];
            const field_value_idx = field_data[fi + 1];

            // get field name
            const field_ident = self.ast.getIdentifier(field_name_idx);
            const field_token = self.tokens.items[field_ident.token_idx];
            const field_name = self.src.getSlice(field_token.start, field_token.start + field_token.len);

            // find field index in struct definition
            for (struct_type.fields, 0..) |field, i| {
                if (mem.eql(u8, field.name, field_name)) {
                    const value_reg = try self.generateExpression(field_value_idx) orelse continue;
                    const field_width = typeIdToWidth(field.type_id);
                    try func.emitStoreField(base, value_reg, struct_idx, @intCast(i), field_width);
                    break;
                }
            }
        }

        // return pointer to the struct
        return base;
    }
};

pub const CodeGenError = error{
    OutOfMemory,
    UnsupportedFeature,
};

pub const CodeGenResult = struct {
    output: []const u8, // LLVM IR text
    mir: MIRModule,
};
