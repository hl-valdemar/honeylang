const std = @import("std");
const mem = std.mem;

ast: *const AST,
str_pool: *const StringPool,
diagnostics: *Diagnostic,
diagnostic_alloc: mem.Allocator,
/// indexed by Inst.Ref.
insts: std.MultiArrayList(Inst),
decls: std.MultiArrayList(DeclInfo),
funcs: std.MultiArrayList(FuncInfo),
branches: std.MultiArrayList(IfElseInfo),
ref_lists: std.ArrayList(Inst.Ref),
root_start: Payload,
root_end: Payload,

const Self = @This();

const Payload = @import("../root.zig").Payload;
const StringPool = @import("../util/StringPool.zig");
const Diagnostic = @import("../diagnostic/Store.zig");
const AST = @import("../parser/AST.zig");

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Ref = Payload;

    pub const Data = struct {
        a: Payload = @enumFromInt(0),
        b: Payload = @enumFromInt(0),
    };

    pub const Tag = enum {
        // literals

        /// data:
        /// * a: string pool id (value)
        int_literal,

        /// data:
        /// * a: string pool id (value)
        float_literal,

        /// data:
        /// * a: string pool id (value)
        str_literal,

        /// data:
        /// * a: diagnostic ref
        trap,

        // binary ops

        /// data:
        /// * a: left ref
        /// * b: right ref
        add,

        /// data:
        /// * a: left ref
        /// * b: right ref
        sub,

        /// data:
        /// * a: left ref
        /// * b: right ref
        mul,

        /// data:
        /// * a: left ref
        /// * b: right ref
        div,

        // unary ops

        /// data:
        /// * a: ref
        not,

        /// data:
        /// * a: ref
        negate,

        /// data:
        /// * a: ref to the decl/param instruction
        ref,

        // decls

        /// data:
        /// * a: string id (name)
        /// * b: decl ref
        decl_const,

        /// data:
        /// * a: string id (name)
        /// * b: decl ref
        decl_var,

        /// data:
        /// * a: string id (name)
        /// * b: ref to block
        decl_namespace,

        // functions

        /// data:
        /// * a: string id (name)
        /// * b: ref to type
        param,

        /// data:
        /// * a: ref to value (or .none)
        ret,

        /// data:
        /// * a, b: ref_lists range to refs
        block,

        /// data:
        /// * a: string id (name)
        /// * b: func ref
        decl_func,

        // if-statements

        /// data:
        /// * a: condition (ref)
        /// * b: body (Ref)
        if_simple,

        /// data:
        /// * a: condition (ref)
        /// * b: branch ref
        if_else,
    };
};

pub const DeclRef = enum(u32) {
    none = std.math.maxInt(u32),
    _,
};

pub const FuncRef = enum(u32) {
    none = std.math.maxInt(u32),
    _,
};

pub const BranchRef = enum(u32) {
    none = std.math.maxInt(u32),
    _,
};

pub const DeclInfo = struct {
    type: Inst.Ref, // .none when unresolved
    value: Inst.Ref,
};

pub const FuncInfo = struct {
    params_start: Payload,
    params_end: Payload,
    ret_type: Inst.Ref, // .none when void
    body: Inst.Ref, // ref to block instruction
    flags: u32,
};

pub const IfElseInfo = struct {
    body: Inst.Ref,
    else_node: Inst.Ref,
};

pub const Context = struct {
    ast: *const AST,
    str_pool: *const StringPool,
    diagnostic_alloc: mem.Allocator,
    diagnostics: *Diagnostic,
};

pub fn init(ctx: Context) Self {
    return .{
        .ast = ctx.ast,
        .str_pool = ctx.str_pool,
        .diagnostics = ctx.diagnostics,
        .diagnostic_alloc = ctx.diagnostic_alloc,
        .insts = .{},
        .decls = .{},
        .funcs = .{},
        .branches = .{},
        .ref_lists = .empty,
        .root_start = @enumFromInt(0),
        .root_end = @enumFromInt(0),
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.insts.deinit(alloc);
    self.decls.deinit(alloc);
    self.funcs.deinit(alloc);
    self.branches.deinit(alloc);
    self.ref_lists.deinit(alloc);
}

pub fn lower(self: *Self, alloc: mem.Allocator, node: AST.Node.Ref) !Inst.Ref {
    if (node == .none) return .none;

    const data = self.ast.nodeData(node);
    switch (self.ast.nodeTag(node)) {
        .root => {
            var decl_refs: std.ArrayList(Payload) = .empty;
            defer decl_refs.deinit(alloc);

            for (self.ast.refSlice(data.a, data.b)) |decl_ref| {
                const ref = try self.lower(alloc, decl_ref);
                try decl_refs.append(alloc, ref);
            }

            const refs = try self.appendRefList(alloc, decl_refs.items);
            self.root_start = refs.start;
            self.root_end = refs.end;
            return .none;
        },
        .const_decl => {
            const name_tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[name_tok];

            const @"type" = if (data.a != .none) try self.lower(alloc, data.a) else .none;
            const value = try self.lower(alloc, data.b);

            const decl_ref = try self.emitDeclInfo(alloc, .{
                .type = @"type",
                .value = value,
            });

            return self.emit(alloc, .decl_const, .{
                .a = Payload.from(name),
                .b = asPayload(decl_ref),
            });
        },
        .var_decl => {
            const name_tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[name_tok];

            const @"type" = if (data.a != .none) try self.lower(alloc, data.a) else .none;
            const value = try self.lower(alloc, data.b);

            const decl_ref = try self.emitDeclInfo(alloc, .{
                .type = @"type",
                .value = value,
            });

            return self.emit(alloc, .decl_var, .{
                .a = Payload.from(name),
                .b = asPayload(decl_ref),
            });
        },
        .func_decl => {
            const name_tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[name_tok];

            const func_decl = self.ast.funcInfo(AST.asFuncRef(data.b));

            const params = self.ast.refSlice(func_decl.params_start, func_decl.params_end);
            var param_refs: std.ArrayList(Payload) = .empty;
            defer param_refs.deinit(alloc);
            for (params) |param| {
                const ref = try self.lower(alloc, param);
                try param_refs.append(alloc, ref);
            }

            const params_range = try self.appendRefList(alloc, param_refs.items);

            const ret_type = try self.lower(alloc, func_decl.ret_type);
            const body = if (func_decl.body != .none) try self.lower(alloc, func_decl.body) else .none;

            const func_ref = try self.emitFuncInfo(alloc, .{
                .params_start = params_range.start,
                .params_end = params_range.end,
                .ret_type = ret_type,
                .body = body,
                .flags = func_decl.flags,
            });

            return self.emit(alloc, .decl_func, .{
                .a = Payload.from(name),
                .b = asPayload(func_ref),
            });
        },
        .namespace_decl => {
            const name_tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[name_tok];

            const block_ref = data.a.to(AST.Node.Ref);
            const block = try self.lower(alloc, block_ref);

            return self.emit(alloc, .decl_namespace, .{
                .a = Payload.from(name),
                .b = block,
            });
        },
        .if_simple => {
            const condition = try self.lower(alloc, data.a);
            const body = try self.lower(alloc, data.b);
            return self.emit(alloc, .if_simple, .{ .a = condition, .b = body });
        },
        .if_else => {
            const condition = try self.lower(alloc, data.a);
            const ast_info = self.ast.branchInfo(AST.asBranchRef(data.b));
            const body = try self.lower(alloc, ast_info.body);
            const else_node = try self.lower(alloc, ast_info.else_node);
            const branch_ref = try self.emitBranchInfo(alloc, .{
                .body = body,
                .else_node = else_node,
            });
            return self.emit(alloc, .if_else, .{ .a = condition, .b = asPayload(branch_ref) });
        },
        .param => {
            const name_tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[name_tok];
            const @"type" = try self.lower(alloc, data.a);
            return self.emit(alloc, .param, .{
                .a = Payload.from(name),
                .b = @"type",
            });
        },
        .block => {
            const statements = self.ast.refSlice(data.a, data.b);

            var stmt_refs: std.ArrayList(Payload) = .empty;
            defer stmt_refs.deinit(alloc);
            for (statements) |stmt| {
                const node_ref = stmt.to(AST.Node.Ref);
                const ref = try self.lower(alloc, node_ref);
                try stmt_refs.append(alloc, ref);
            }

            const refs = try self.appendRefList(alloc, stmt_refs.items);
            return self.emit(alloc, .block, .{ .a = refs.start, .b = refs.end });
        },
        .expr_statement => return self.lower(alloc, data.a),
        .return_val => {
            const expr = if (data.a != .none) try self.lower(alloc, data.a) else .none;
            return self.emit(alloc, .ret, .{ .a = expr });
        },
        .unary_op => {
            const expr = try self.lower(alloc, data.a);
            const op_tok = self.ast.nodeMainToken(node);
            const tag: Inst.Tag = switch (self.ast.tokens.items(.tag)[op_tok]) {
                .bang => .not,
                .minus => .negate,
                else => unreachable,
            };
            return self.emit(alloc, tag, .{ .a = expr });
        },
        .binary_op => {
            const left = try self.lower(alloc, data.a);
            const right = try self.lower(alloc, data.b);
            const op_tok = self.ast.nodeMainToken(node);
            const tag: Inst.Tag = switch (self.ast.tokens.items(.tag)[op_tok]) {
                .plus => .add,
                .minus => .sub,
                .star => .mul,
                .slash => .div,
                else => unreachable,
            };
            return self.emit(alloc, tag, .{ .a = left, .b = right });
        },
        .identifier => {
            const tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[tok];
            return try self.emit(alloc, .ref, .{
                .a = Payload.from(name),
            });
        },
        .int_literal => {
            const tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[tok];
            return try self.emit(alloc, .int_literal, .{
                .a = Payload.from(name),
            });
        },
        .float_literal => {
            const tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[tok];
            return try self.emit(alloc, .float_literal, .{
                .a = Payload.from(name),
            });
        },
        .@"error" => return self.emit(alloc, .trap, .{ .a = data.a }),
        else => {
            const diag_ref = try self.addDiagnostic(.lowering_unsupported_node);
            return self.emit(alloc, .trap, .{ .a = Payload.from(diag_ref) });
        },
    }
}

fn addDiagnostic(self: *Self, tag: Diagnostic.Tag) !Diagnostic.Ref {
    return self.diagnostics.add(self.diagnostic_alloc, .{
        .stage = .lowering,
        .severity = .err,
        .tag = tag,
        .span = null,
    });
}

pub fn emit(self: *Self, alloc: mem.Allocator, tag: Inst.Tag, data: Inst.Data) !Inst.Ref {
    try self.insts.append(alloc, .{ .tag = tag, .data = data });
    return Payload.fromIndex(self.insts.len - 1);
}

pub fn emitDeclInfo(self: *Self, alloc: mem.Allocator, info: DeclInfo) !DeclRef {
    const ref: DeclRef = Payload.fromIndex(self.decls.len).to(DeclRef);
    try self.decls.append(alloc, info);
    return ref;
}

pub fn declInfo(self: *const Self, ref: DeclRef) DeclInfo {
    return self.decls.get(@intFromEnum(ref));
}

pub fn emitFuncInfo(self: *Self, alloc: mem.Allocator, info: FuncInfo) !FuncRef {
    const ref: FuncRef = Payload.fromIndex(self.funcs.len).to(FuncRef);
    try self.funcs.append(alloc, info);
    return ref;
}

pub fn funcInfo(self: *const Self, ref: FuncRef) FuncInfo {
    return self.funcs.get(@intFromEnum(ref));
}

pub fn emitBranchInfo(self: *Self, alloc: mem.Allocator, info: IfElseInfo) !BranchRef {
    const ref: BranchRef = Payload.fromIndex(self.branches.len).to(BranchRef);
    try self.branches.append(alloc, info);
    return ref;
}

pub fn branchInfo(self: *const Self, ref: BranchRef) IfElseInfo {
    return self.branches.get(@intFromEnum(ref));
}

pub fn appendRefList(self: *Self, alloc: mem.Allocator, refs: []const Payload) !struct { start: Payload, end: Payload } {
    const start = Payload.fromIndex(self.ref_lists.items.len);
    try self.ref_lists.appendSlice(alloc, refs);
    const end = Payload.fromIndex(self.ref_lists.items.len);
    return .{ .start = start, .end = end };
}

pub fn refSlice(self: *const Self, start: Payload, end: Payload) []const Payload {
    return self.ref_lists.items[@intFromEnum(start)..@intFromEnum(end)];
}

pub fn asPayload(ref: anytype) Payload {
    const Ref = @TypeOf(ref);
    if (Ref != DeclRef and Ref != FuncRef and Ref != BranchRef)
        @compileError("expected a typed hir payload ref");
    return Payload.from(ref);
}

pub fn asDeclRef(ref: Payload) DeclRef {
    return ref.to(DeclRef);
}

pub fn asFuncRef(ref: Payload) FuncRef {
    return ref.to(FuncRef);
}

pub fn asBranchRef(ref: Payload) BranchRef {
    return ref.to(BranchRef);
}

pub fn rootDecls(self: *const Self) []const Payload {
    return self.refSlice(self.root_start, self.root_end);
}

pub fn render(self: *const Self, alloc: mem.Allocator) ![]const u8 {
    var aw: std.Io.Writer.Allocating = .init(alloc);
    const w = &aw.writer;

    for (0..self.insts.len) |idx| {
        const inst = self.insts.get(idx);
        switch (inst.tag) {
            .int_literal => {
                const val = self.str_pool.get(inst.data.a.to(StringPool.ID));
                try w.print("%{d: <3} int({s})\n", .{ idx, val });
            },
            .float_literal => {
                const val = self.str_pool.get(inst.data.a.to(StringPool.ID));
                try w.print("%{d: <3} float({s})\n", .{ idx, val });
            },
            .str_literal => {
                const val = self.str_pool.get(inst.data.a.to(StringPool.ID));
                try w.print("%{d: <3} str(\"{s}\")\n", .{ idx, val });
            },
            .trap => try w.print("%{d: <3} trap(D{d})\n", .{ idx, @intFromEnum(inst.data.a) }),
            .not, .negate => {
                const tag_name = @tagName(inst.tag);
                try w.print("%{d: <3} {s}(%{d})\n", .{ idx, tag_name, @intFromEnum(inst.data.a) });
            },
            .add, .sub, .mul, .div => {
                const tag_name = @tagName(inst.tag);
                try w.print(
                    "%{d: <3} {s}(%{d}, %{d})\n",
                    .{ idx, tag_name, @intFromEnum(inst.data.a), @intFromEnum(inst.data.b) },
                );
            },
            .ref => {
                const name = self.str_pool.get(inst.data.a.to(StringPool.ID));
                try w.print("%{d: <3} ref(\"{s}\")\n", .{ idx, name });
            },
            .decl_const, .decl_var => {
                const tag_name = @tagName(inst.tag);
                const name = self.str_pool.get(inst.data.a.to(StringPool.ID));
                const info = self.declInfo(asDeclRef(inst.data.b));

                try w.print("%{d: <3} {s}(\"{s}\"", .{ idx, tag_name, name });

                if (info.type != .none)
                    try w.print(", type=%{d}", .{@intFromEnum(info.type)})
                else
                    try w.print(", type=<pending>", .{});

                try w.print(", value=%{d})\n", .{@intFromEnum(info.value)});
            },
            .decl_namespace => {
                const tag_name = @tagName(inst.tag);
                const name = self.str_pool.get(inst.data.a.to(StringPool.ID));
                try w.print("%{d: <3} {s}(\"{s}\", body=%{d})\n", .{ idx, tag_name, name, @intFromEnum(inst.data.b) });
            },
            .param => {
                const name = self.str_pool.get(inst.data.a.to(StringPool.ID));
                const hir_type = @intFromEnum(inst.data.b);
                try w.print(
                    "%{d: <3} param(\"{s}\", type=%{d})\n",
                    .{ idx, name, hir_type },
                );
            },
            .ret => {
                if (inst.data.a != .none)
                    try w.print("%{d: <3} ret(%{d})\n", .{ idx, @intFromEnum(inst.data.a) })
                else
                    try w.print("%{d: <3} ret()\n", .{idx});
            },
            .block => {
                try w.print("%{d: <3} block(", .{idx});
                const refs = self.refSlice(inst.data.a, inst.data.b);
                for (refs, 0..) |r, j| {
                    if (j > 0) try w.writeAll(", ");
                    try w.print("%{d}", .{@intFromEnum(r)});
                }
                try w.writeAll(")\n");
            },
            .decl_func => {
                const tag_name = @tagName(inst.tag);
                const name = self.str_pool.get(inst.data.a.to(StringPool.ID));
                const info = self.funcInfo(asFuncRef(inst.data.b));
                const cc: AST.FuncDecl.CallingConvention =
                    @enumFromInt((info.flags & AST.FuncDecl.Flag.cc_mask) >> AST.FuncDecl.Flag.cc_shift);

                try w.print("%{d: <3} {s}(\"{s}\", cc={s}, params=%{d}..%{d}", .{
                    idx,
                    tag_name,
                    name,
                    @tagName(cc),
                    @intFromEnum(info.params_start),
                    @intFromEnum(info.params_end),
                });

                if (info.ret_type != .none)
                    try w.print(", ret=%{d}", .{@intFromEnum(info.ret_type)});

                if (info.body != .none)
                    try w.print(", body=%{d})\n", .{@intFromEnum(info.body)})
                else
                    try w.print(", body=<extern>)\n", .{});
            },
            .if_simple => try w.print(
                "%{d: <3} if_simple(cond=%{d}, body=%{d})\n",
                .{ idx, @intFromEnum(inst.data.a), @intFromEnum(inst.data.b) },
            ),
            .if_else => {
                const info = self.branchInfo(asBranchRef(inst.data.b));
                try w.print(
                    "%{d: <3} if_else(cond=%{d}, body=%{d}, else=%{d})\n",
                    .{ idx, @intFromEnum(inst.data.a), @intFromEnum(info.body), @intFromEnum(info.else_node) },
                );
            },
        }
    }

    return aw.toOwnedSlice();
}
