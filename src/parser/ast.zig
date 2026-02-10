const std = @import("std");
const mem = std.mem;

pub const SourceIndex = @import("../source/source.zig").SourceIndex;

pub const NodeIndex = u32;

pub const NodeKind = enum {
    // top-level
    program,

    // declarations
    const_decl,
    func_decl,
    var_decl,
    struct_decl,

    // expressions
    binary_op,
    unary_op,
    call_expr,
    field_access,
    struct_literal,
    identifier,
    literal,
    void_literal,
    address_of,
    deref,

    // type expressions
    pointer_type,

    // statements
    block,
    return_stmt,
    defer_stmt,
    if_stmt,
    assignment,

    // parse error
    err,
};

pub const Range = struct {
    start: u32,
    len: u32,

    pub fn empty() Range {
        return .{ .start = 0, .len = 0 };
    }
};

pub const Location = struct {
    start: SourceIndex,
    end: SourceIndex,
};

pub const Program = struct {
    declarations: Range, // into extra_data
};

pub const ConstDecl = struct {
    name: NodeIndex,
    type_id: ?NodeIndex,
    value: NodeIndex,
};

/// Function calling convention for interop.
/// Default is `honey`.
pub const CallingConvention = enum {
    honey, // default
    c,
    cobol,
    fortran,
};

pub const FuncDecl = struct {
    name: NodeIndex,
    params: Range,
    return_type: NodeIndex,
    body: ?NodeIndex, // null for external functions
    call_conv: CallingConvention,
};

pub const VarDecl = struct {
    name: NodeIndex,
    type_id: ?NodeIndex,
    value: NodeIndex,
    is_mutable: bool,
};

pub const StructDecl = struct {
    name: NodeIndex,
    fields: Range, // pairs of (name_ident, type_ident) in extra_data
    call_conv: CallingConvention,
};

pub const BinaryOp = struct {
    op: Op,
    left: NodeIndex,
    right: NodeIndex,

    pub const Op = enum {
        // arithmetic
        add,
        sub,
        mul,
        div,

        // comparison
        equal,
        not_equal,
        less,
        greater,
        less_equal,
        greater_equal,

        // logical
        and_,
        or_,
    };

    pub fn isArithmetic(self: *const BinaryOp) bool {
        return switch (self.op) {
            .add, .sub, .mul, .div => true,
            else => false,
        };
    }

    pub fn isComparison(self: *const BinaryOp) bool {
        return switch (self.op) {
            .equal, .not_equal, .less, .greater, .less_equal, .greater_equal => true,
            else => false,
        };
    }

    pub fn isLogical(self: *const BinaryOp) bool {
        return switch (self.op) {
            .and_, .or_ => true,
            else => false,
        };
    }
};

pub const UnaryOp = struct {
    op: Op,
    operand: NodeIndex,

    pub const Op = enum {
        negate,
        not,
    };
};

pub const CallExpr = struct {
    func: NodeIndex,
    args: Range,
};

pub const FieldAccess = struct {
    object: NodeIndex, // expression being accessed
    field: NodeIndex, // identifier node for the field name
};

pub const StructLiteral = struct {
    type_name: NodeIndex, // identifier for the struct type
    fields: Range, // pairs of (field_name_ident, value_expr) in extra_data
};

pub const Identifier = struct {
    token_idx: u32,
};

pub const Literal = struct {
    token_idx: u32,
};

pub const Block = struct {
    statements: Range,
    deferred: Range,
};

pub const Return = struct {
    expr: NodeIndex,
};

pub const Defer = struct {
    stmt: NodeIndex,
};

pub const If = struct {
    if_guard: NodeIndex,
    if_block: NodeIndex,
    else_ifs: Range, // interleaved (guard, block) pairs in extra_data
    else_block: ?NodeIndex,

    /// Returns the number of else-if branches
    pub fn elseIfCount(self: *const If) usize {
        return self.else_ifs.len / 2;
    }

    /// A (guard, block) pair for an else-if branch
    pub const ElseIfPair = struct {
        guard: NodeIndex,
        block: NodeIndex,
    };

    /// Get a specific else-if pair by index.
    pub fn getElseIf(self: *const If, ast: *const Ast, idx: usize) ?ElseIfPair {
        if (idx >= self.elseIfCount()) return null;
        const data = ast.getExtra(self.else_ifs);
        const base = idx * 2;
        return .{
            .guard = data[base],
            .block = data[base + 1],
        };
    }
};

pub const Assignment = struct {
    target: NodeIndex,
    value: NodeIndex,
};

pub const AddressOf = struct {
    operand: NodeIndex,
};

pub const Deref = struct {
    operand: NodeIndex,
};

pub const PointerType = struct {
    pointee: NodeIndex,
    is_mutable: bool,
};

pub const Error = struct {
    msg: []const u8,
};

pub const Ast = struct {
    allocator: mem.Allocator,

    // parallel arrays, all same length (total number of nodes)
    kinds: std.ArrayList(NodeKind),
    starts: std.ArrayList(SourceIndex),
    ends: std.ArrayList(SourceIndex),
    data_indices: std.ArrayList(u32),

    // node-specific data arrays
    programs: std.ArrayList(Program),
    const_decls: std.ArrayList(ConstDecl),
    func_decls: std.ArrayList(FuncDecl),
    var_decls: std.ArrayList(VarDecl),
    struct_decls: std.ArrayList(StructDecl),
    binary_ops: std.ArrayList(BinaryOp),
    unary_ops: std.ArrayList(UnaryOp),
    call_exprs: std.ArrayList(CallExpr),
    field_accesses: std.ArrayList(FieldAccess),
    struct_literals: std.ArrayList(StructLiteral),
    identifiers: std.ArrayList(Identifier),
    literals: std.ArrayList(Literal),
    blocks: std.ArrayList(Block),
    returns: std.ArrayList(Return),
    defers: std.ArrayList(Defer),
    ifs: std.ArrayList(If),
    assignments: std.ArrayList(Assignment),
    address_ofs: std.ArrayList(AddressOf),
    derefs: std.ArrayList(Deref),
    pointer_types: std.ArrayList(PointerType),
    errors: std.ArrayList(Error),

    // auxiliary storage for variable-length children
    extra_data: std.ArrayList(NodeIndex),

    root: NodeIndex = 0,

    pub fn init(allocator: mem.Allocator) !Ast {
        const capacity = 10;
        return .{
            .allocator = allocator,

            .kinds = try std.ArrayList(NodeKind).initCapacity(allocator, capacity),
            .starts = try std.ArrayList(SourceIndex).initCapacity(allocator, capacity),
            .ends = try std.ArrayList(SourceIndex).initCapacity(allocator, capacity),
            .data_indices = try std.ArrayList(u32).initCapacity(allocator, capacity),

            .programs = try std.ArrayList(Program).initCapacity(allocator, capacity),
            .const_decls = try std.ArrayList(ConstDecl).initCapacity(allocator, capacity),
            .func_decls = try std.ArrayList(FuncDecl).initCapacity(allocator, capacity),
            .var_decls = try std.ArrayList(VarDecl).initCapacity(allocator, capacity),
            .struct_decls = try std.ArrayList(StructDecl).initCapacity(allocator, capacity),
            .binary_ops = try std.ArrayList(BinaryOp).initCapacity(allocator, capacity),
            .unary_ops = try std.ArrayList(UnaryOp).initCapacity(allocator, capacity),
            .call_exprs = try std.ArrayList(CallExpr).initCapacity(allocator, capacity),
            .field_accesses = try std.ArrayList(FieldAccess).initCapacity(allocator, capacity),
            .struct_literals = try std.ArrayList(StructLiteral).initCapacity(allocator, capacity),
            .identifiers = try std.ArrayList(Identifier).initCapacity(allocator, capacity),
            .literals = try std.ArrayList(Literal).initCapacity(allocator, capacity),
            .blocks = try std.ArrayList(Block).initCapacity(allocator, capacity),
            .returns = try std.ArrayList(Return).initCapacity(allocator, capacity),
            .defers = try std.ArrayList(Defer).initCapacity(allocator, capacity),
            .ifs = try std.ArrayList(If).initCapacity(allocator, capacity),
            .assignments = try std.ArrayList(Assignment).initCapacity(allocator, capacity),
            .address_ofs = try std.ArrayList(AddressOf).initCapacity(allocator, capacity),
            .derefs = try std.ArrayList(Deref).initCapacity(allocator, capacity),
            .pointer_types = try std.ArrayList(PointerType).initCapacity(allocator, capacity),
            .errors = try std.ArrayList(Error).initCapacity(allocator, capacity),

            .extra_data = try std.ArrayList(NodeIndex).initCapacity(allocator, capacity),
        };
    }

    pub fn deinit(self: *Ast) void {
        self.kinds.deinit(self.allocator);
        self.starts.deinit(self.allocator);
        self.ends.deinit(self.allocator);
        self.data_indices.deinit(self.allocator);

        self.programs.deinit(self.allocator);
        self.const_decls.deinit(self.allocator);
        self.func_decls.deinit(self.allocator);
        self.var_decls.deinit(self.allocator);
        self.struct_decls.deinit(self.allocator);
        self.binary_ops.deinit(self.allocator);
        self.unary_ops.deinit(self.allocator);
        self.call_exprs.deinit(self.allocator);
        self.field_accesses.deinit(self.allocator);
        self.struct_literals.deinit(self.allocator);
        self.identifiers.deinit(self.allocator);
        self.literals.deinit(self.allocator);
        self.blocks.deinit(self.allocator);
        self.returns.deinit(self.allocator);
        self.defers.deinit(self.allocator);
        self.ifs.deinit(self.allocator);
        self.assignments.deinit(self.allocator);
        self.address_ofs.deinit(self.allocator);
        self.derefs.deinit(self.allocator);
        self.pointer_types.deinit(self.allocator);
        self.errors.deinit(self.allocator);

        self.extra_data.deinit(self.allocator);
    }

    pub fn addProgram(
        self: *Ast,
        declarations: Range,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.programs.items.len);

        try self.kinds.append(self.allocator, .program);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.programs.append(self.allocator, .{ .declarations = declarations });

        return node_idx;
    }

    pub fn addConstDecl(
        self: *Ast,
        name: NodeIndex,
        type_id: ?NodeIndex,
        value: NodeIndex,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.const_decls.items.len);

        try self.kinds.append(self.allocator, .const_decl);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.const_decls.append(self.allocator, .{
            .name = name,
            .type_id = type_id,
            .value = value,
        });

        return node_idx;
    }

    pub fn addFuncDecl(
        self: *Ast,
        name: NodeIndex,
        params: Range,
        return_type: NodeIndex,
        body: ?NodeIndex,
        calling_conv: CallingConvention,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.func_decls.items.len);

        try self.kinds.append(self.allocator, .func_decl);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.func_decls.append(self.allocator, .{
            .name = name,
            .params = params,
            .return_type = return_type,
            .body = body,
            .call_conv = calling_conv,
        });

        return node_idx;
    }

    pub fn addVarDecl(
        self: *Ast,
        name: NodeIndex,
        type_id: ?NodeIndex,
        value: NodeIndex,
        is_mutable: bool,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.var_decls.items.len);

        try self.kinds.append(self.allocator, .var_decl);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.var_decls.append(self.allocator, .{
            .name = name,
            .type_id = type_id,
            .value = value,
            .is_mutable = is_mutable,
        });

        return node_idx;
    }

    pub fn addStructDecl(
        self: *Ast,
        name: NodeIndex,
        fields: Range,
        calling_conv: CallingConvention,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.struct_decls.items.len);

        try self.kinds.append(self.allocator, .struct_decl);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.struct_decls.append(self.allocator, .{
            .name = name,
            .fields = fields,
            .call_conv = calling_conv,
        });

        return node_idx;
    }

    pub fn addBinaryOp(
        self: *Ast,
        op: BinaryOp.Op,
        left: NodeIndex,
        right: NodeIndex,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.binary_ops.items.len);

        try self.kinds.append(self.allocator, .binary_op);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.binary_ops.append(self.allocator, .{
            .op = op,
            .left = left,
            .right = right,
        });

        return node_idx;
    }

    pub fn addUnaryOp(
        self: *Ast,
        op: UnaryOp.Op,
        operand: NodeIndex,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.unary_ops.items.len);

        try self.kinds.append(self.allocator, .unary_op);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.unary_ops.append(self.allocator, .{
            .op = op,
            .operand = operand,
        });

        return node_idx;
    }

    pub fn addCallExpr(
        self: *Ast,
        func: NodeIndex,
        args: Range,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.call_exprs.items.len);

        try self.kinds.append(self.allocator, .call_expr);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.call_exprs.append(self.allocator, .{
            .func = func,
            .args = args,
        });

        return node_idx;
    }

    pub fn addFieldAccess(
        self: *Ast,
        object: NodeIndex,
        field: NodeIndex,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.field_accesses.items.len);

        try self.kinds.append(self.allocator, .field_access);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.field_accesses.append(self.allocator, .{
            .object = object,
            .field = field,
        });

        return node_idx;
    }

    pub fn addStructLiteral(
        self: *Ast,
        type_name: NodeIndex,
        fields: Range,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.struct_literals.items.len);

        try self.kinds.append(self.allocator, .struct_literal);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.struct_literals.append(self.allocator, .{
            .type_name = type_name,
            .fields = fields,
        });

        return node_idx;
    }

    pub fn addIdentifier(
        self: *Ast,
        token_idx: u32,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.identifiers.items.len);

        try self.kinds.append(self.allocator, .identifier);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.identifiers.append(self.allocator, .{ .token_idx = token_idx });

        return node_idx;
    }

    pub fn addLiteral(
        self: *Ast,
        token_idx: u32,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.literals.items.len);

        try self.kinds.append(self.allocator, .literal);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.literals.append(self.allocator, .{ .token_idx = token_idx });

        return node_idx;
    }

    pub fn addVoidLiteral(
        self: *Ast,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);

        try self.kinds.append(self.allocator, .void_literal);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, 0);

        return node_idx;
    }

    pub fn addBlock(
        self: *Ast,
        statements: Range,
        deferred: Range,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.blocks.items.len);

        try self.kinds.append(self.allocator, .block);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.blocks.append(self.allocator, .{
            .statements = statements,
            .deferred = deferred,
        });

        return node_idx;
    }

    pub fn addReturn(
        self: *Ast,
        expr: NodeIndex,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.returns.items.len);

        try self.kinds.append(self.allocator, .return_stmt);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.returns.append(self.allocator, .{ .expr = expr });

        return node_idx;
    }

    pub fn addDefer(
        self: *Ast,
        stmt: NodeIndex,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.defers.items.len);

        try self.kinds.append(self.allocator, .defer_stmt);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.defers.append(self.allocator, .{ .stmt = stmt });

        return node_idx;
    }

    pub fn addIf(
        self: *Ast,
        if_guard: NodeIndex,
        if_block: NodeIndex,
        else_ifs: Range,
        else_block: ?NodeIndex,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.ifs.items.len);

        try self.kinds.append(self.allocator, .if_stmt);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.ifs.append(self.allocator, .{
            .if_guard = if_guard,
            .if_block = if_block,
            .else_ifs = else_ifs,
            .else_block = else_block,
        });

        return node_idx;
    }

    pub fn addAssignment(
        self: *Ast,
        target: NodeIndex,
        value: NodeIndex,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.assignments.items.len);

        try self.kinds.append(self.allocator, .assignment);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.assignments.append(self.allocator, .{
            .target = target,
            .value = value,
        });

        return node_idx;
    }

    pub fn addAddressOf(
        self: *Ast,
        operand: NodeIndex,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.address_ofs.items.len);

        try self.kinds.append(self.allocator, .address_of);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.address_ofs.append(self.allocator, .{ .operand = operand });

        return node_idx;
    }

    pub fn addDeref(
        self: *Ast,
        operand: NodeIndex,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.derefs.items.len);

        try self.kinds.append(self.allocator, .deref);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.derefs.append(self.allocator, .{ .operand = operand });

        return node_idx;
    }

    pub fn addPointerType(
        self: *Ast,
        pointee: NodeIndex,
        is_mutable: bool,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.pointer_types.items.len);

        try self.kinds.append(self.allocator, .pointer_type);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.pointer_types.append(self.allocator, .{
            .pointee = pointee,
            .is_mutable = is_mutable,
        });

        return node_idx;
    }

    pub fn addError(
        self: *Ast,
        msg: []const u8,
        start: SourceIndex,
        end: SourceIndex,
    ) !NodeIndex {
        const node_idx: NodeIndex = @intCast(self.kinds.items.len);
        const data_idx: NodeIndex = @intCast(self.errors.items.len);

        try self.kinds.append(self.allocator, .err);
        try self.starts.append(self.allocator, start);
        try self.ends.append(self.allocator, end);
        try self.data_indices.append(self.allocator, data_idx);
        try self.errors.append(self.allocator, .{ .msg = msg });

        return node_idx;
    }

    /// Recursively duplicate an expression node tree. Only supports node kinds
    /// that can appear as assignment targets (identifier, deref, field_access).
    pub fn duplicateExpr(self: *Ast, idx: NodeIndex) !NodeIndex {
        const start = self.getLocation(idx).start;
        const end = self.getLocation(idx).end;
        return switch (self.getKind(idx)) {
            .identifier => {
                const ident = self.getIdentifier(idx);
                return try self.addIdentifier(ident.token_idx, start, end);
            },
            .deref => {
                const deref = self.getDeref(idx);
                const dup_operand = try self.duplicateExpr(deref.operand);
                return try self.addDeref(dup_operand, start, end);
            },
            .field_access => {
                const fa = self.getFieldAccess(idx);
                const dup_object = try self.duplicateExpr(fa.object);
                const dup_field = try self.duplicateExpr(fa.field);
                return try self.addFieldAccess(dup_object, dup_field, start, end);
            },
            else => idx, // fallback: return original (shouldn't happen for assignment targets)
        };
    }

    pub fn getKind(self: *const Ast, idx: NodeIndex) NodeKind {
        return self.kinds.items[idx];
    }

    pub fn getLocation(self: *const Ast, idx: NodeIndex) Location {
        return .{
            .start = self.starts.items[idx],
            .end = self.ends.items[idx],
        };
    }

    pub fn getProgram(self: *const Ast, idx: NodeIndex) Program {
        std.debug.assert(self.kinds.items[idx] == .program);
        const data_idx = self.data_indices.items[idx];
        return self.programs.items[data_idx];
    }

    pub fn getConstDecl(self: *const Ast, idx: NodeIndex) ConstDecl {
        std.debug.assert(self.kinds.items[idx] == .const_decl);
        const data_idx = self.data_indices.items[idx];
        return self.const_decls.items[data_idx];
    }

    pub fn getFuncDecl(self: *const Ast, idx: NodeIndex) FuncDecl {
        std.debug.assert(self.kinds.items[idx] == .func_decl);
        const data_idx = self.data_indices.items[idx];
        return self.func_decls.items[data_idx];
    }

    pub fn getVarDecl(self: *const Ast, idx: NodeIndex) VarDecl {
        std.debug.assert(self.kinds.items[idx] == .var_decl);
        const data_idx = self.data_indices.items[idx];
        return self.var_decls.items[data_idx];
    }

    pub fn getStructDecl(self: *const Ast, idx: NodeIndex) StructDecl {
        std.debug.assert(self.kinds.items[idx] == .struct_decl);
        const data_idx = self.data_indices.items[idx];
        return self.struct_decls.items[data_idx];
    }

    pub fn getBinaryOp(self: *const Ast, idx: NodeIndex) BinaryOp {
        std.debug.assert(self.kinds.items[idx] == .binary_op);
        const data_idx = self.data_indices.items[idx];
        return self.binary_ops.items[data_idx];
    }

    pub fn getUnaryOp(self: *const Ast, idx: NodeIndex) UnaryOp {
        std.debug.assert(self.kinds.items[idx] == .unary_op);
        const data_idx = self.data_indices.items[idx];
        return self.unary_ops.items[data_idx];
    }

    pub fn getCallExpr(self: *const Ast, idx: NodeIndex) CallExpr {
        std.debug.assert(self.kinds.items[idx] == .call_expr);
        const data_idx = self.data_indices.items[idx];
        return self.call_exprs.items[data_idx];
    }

    pub fn getFieldAccess(self: *const Ast, idx: NodeIndex) FieldAccess {
        std.debug.assert(self.kinds.items[idx] == .field_access);
        const data_idx = self.data_indices.items[idx];
        return self.field_accesses.items[data_idx];
    }

    pub fn getStructLiteral(self: *const Ast, idx: NodeIndex) StructLiteral {
        std.debug.assert(self.kinds.items[idx] == .struct_literal);
        const data_idx = self.data_indices.items[idx];
        return self.struct_literals.items[data_idx];
    }

    pub fn getIdentifier(self: *const Ast, idx: NodeIndex) Identifier {
        std.debug.assert(self.kinds.items[idx] == .identifier);
        const data_idx = self.data_indices.items[idx];
        return self.identifiers.items[data_idx];
    }

    pub fn getLiteral(self: *const Ast, idx: NodeIndex) Literal {
        std.debug.assert(self.kinds.items[idx] == .literal);
        const data_idx = self.data_indices.items[idx];
        return self.literals.items[data_idx];
    }

    pub fn getBlock(self: *const Ast, idx: NodeIndex) Block {
        std.debug.assert(self.kinds.items[idx] == .block);
        const data_idx = self.data_indices.items[idx];
        return self.blocks.items[data_idx];
    }

    pub fn getReturn(self: *const Ast, idx: NodeIndex) Return {
        std.debug.assert(self.kinds.items[idx] == .return_stmt);
        const data_idx = self.data_indices.items[idx];
        return self.returns.items[data_idx];
    }

    pub fn getDefer(self: *const Ast, idx: NodeIndex) Defer {
        std.debug.assert(self.kinds.items[idx] == .defer_stmt);
        const data_idx = self.data_indices.items[idx];
        return self.defers.items[data_idx];
    }

    pub fn getIf(self: *const Ast, idx: NodeIndex) If {
        std.debug.assert(self.kinds.items[idx] == .if_stmt);
        const data_idx = self.data_indices.items[idx];
        return self.ifs.items[data_idx];
    }

    pub fn getAssignment(self: *const Ast, idx: NodeIndex) Assignment {
        std.debug.assert(self.kinds.items[idx] == .assignment);
        const data_idx = self.data_indices.items[idx];
        return self.assignments.items[data_idx];
    }

    pub fn getAddressOf(self: *const Ast, idx: NodeIndex) AddressOf {
        std.debug.assert(self.kinds.items[idx] == .address_of);
        const data_idx = self.data_indices.items[idx];
        return self.address_ofs.items[data_idx];
    }

    pub fn getDeref(self: *const Ast, idx: NodeIndex) Deref {
        std.debug.assert(self.kinds.items[idx] == .deref);
        const data_idx = self.data_indices.items[idx];
        return self.derefs.items[data_idx];
    }

    pub fn getPointerType(self: *const Ast, idx: NodeIndex) PointerType {
        std.debug.assert(self.kinds.items[idx] == .pointer_type);
        const data_idx = self.data_indices.items[idx];
        return self.pointer_types.items[data_idx];
    }

    pub fn getError(self: *const Ast, idx: NodeIndex) Error {
        std.debug.assert(self.kinds.items[idx] == .err);
        const data_idx = self.data_indices.items[idx];
        return self.errors.items[data_idx];
    }

    pub fn addExtra(self: *Ast, data: []const NodeIndex) !Range {
        const start: u32 = @intCast(self.extra_data.items.len);
        try self.extra_data.appendSlice(self.allocator, data);
        return .{ .start = start, .len = @intCast(data.len) };
    }

    pub fn getExtra(self: *const Ast, range: Range) []const NodeIndex {
        const start = range.start;
        const end = start + range.len;
        return self.extra_data.items[start..end];
    }

    pub fn nodeCount(self: *const Ast) usize {
        return self.kinds.items.len;
    }
};
