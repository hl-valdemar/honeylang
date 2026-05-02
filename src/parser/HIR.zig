const std = @import("std");
const mem = std.mem;

ast: *const AST,
str_pool: *const StringPool,
diagnostics: *Diagnostic,
diagnostic_alloc: mem.Allocator,
/// indexed by Inst.Ref.
insts: std.MultiArrayList(Inst),
extra_data: std.ArrayList(Inst.Ref),
root_start: BaseRef,
root_end: BaseRef,

const Self = @This();

const BaseRef = @import("../root.zig").BaseRef;
const StringPool = @import("../util/StringPool.zig");
const Diagnostic = @import("../diagnostic/Store.zig");
const AST = @import("../parser/AST.zig");

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Ref = BaseRef;

    pub const Data = struct {
        a: BaseRef = @enumFromInt(0),
        b: BaseRef = @enumFromInt(0),
    };

    pub const Tag = enum {
        // literals

        /// extra-data:
        /// * a: string pool id (value)
        int_literal,

        /// extra-data:
        /// * a: string pool id (value)
        float_literal,

        /// extra-data:
        /// * a: string pool id (value)
        str_literal,

        /// extra-data:
        /// * a: diagnostic ref
        trap,

        // binary ops

        /// extra-data:
        /// * a: left Ref
        /// * b: right Ref
        add,

        /// extra-data:
        /// * a: left Ref
        /// * b: right Ref
        sub,

        /// extra-data:
        /// * a: left Ref
        /// * b: right Ref
        mul,

        /// extra-data:
        /// * a: left Ref
        /// * b: right Ref
        div,

        // unary ops

        /// extra-data:
        /// * a: Ref
        not,

        /// extra-data:
        /// * a: Ref
        negate,

        /// extra-data:
        /// * a: Ref to the decl/param instruction
        ref,

        // decls

        /// extra-data:
        /// * a: string id (name)
        /// * b: extra_data index → DeclInfo
        decl_const,

        /// extra-data:
        /// * a: string id (name)
        /// * b: extra_data index → DeclInfo
        decl_var,

        /// extra-data:
        /// * a: string id (name)
        /// * b: Ref to block
        decl_namespace,

        // functions

        /// extra-data:
        /// * a: string id (name)
        /// * b: Ref to type
        param,

        /// extra-data:
        /// * a: Ref to value (or .none)
        ret,

        /// extra-data:
        /// * a, b: extra_data range to Ref indices
        block,

        /// extra-data:
        /// * a: string id (name)
        /// * b: extra_data index → FuncInfo
        decl_func,

        // if-statements

        /// extra-data:
        /// * a: condition (Ref)
        /// * b: body (Ref)
        if_simple,

        /// extra-data:
        /// * a: condition (Ref)
        /// * b: extra_data index → IfElseInfo
        if_else,
    };
};

pub const DeclInfo = struct {
    type: Inst.Ref, // .none when unresolved
    value: Inst.Ref,
};

pub const FuncInfo = struct {
    params_start: BaseRef, // range into extra_data
    params_end: BaseRef,
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
        .extra_data = .empty,
        .root_start = @enumFromInt(0),
        .root_end = @enumFromInt(0),
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.insts.deinit(alloc);
    self.extra_data.deinit(alloc);
}

pub fn lower(self: *Self, alloc: mem.Allocator, node: AST.Node.Ref) !Inst.Ref {
    if (node == .none) return .none;

    const data = self.ast.nodeData(node);
    switch (self.ast.nodeTag(node)) {
        .root => {
            var decl_refs: std.ArrayList(BaseRef) = .empty;
            defer decl_refs.deinit(alloc);

            for (self.ast.extraSlice(data.a, data.b)) |decl_ref| {
                const ref = try self.lower(alloc, decl_ref);
                try decl_refs.append(alloc, ref);
            }

            self.root_start = @enumFromInt(self.extra_data.items.len);
            try self.extra_data.appendSlice(alloc, decl_refs.items);
            self.root_end = @enumFromInt(self.extra_data.items.len);
            return .none;
        },
        .const_decl => {
            const name_tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[name_tok];

            const @"type" = if (data.a != .none) try self.lower(alloc, data.a) else .none;
            const value = try self.lower(alloc, data.b);

            const extra_idx = try self.packExtraData(alloc, DeclInfo, .{
                .type = @"type",
                .value = value,
            });

            return self.emit(alloc, .decl_const, .{
                .a = @enumFromInt(@intFromEnum(name)),
                .b = extra_idx,
            });
        },
        .var_decl => {
            const name_tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[name_tok];

            const @"type" = if (data.a != .none) try self.lower(alloc, data.a) else .none;
            const value = try self.lower(alloc, data.b);

            const extra_idx = try self.packExtraData(alloc, DeclInfo, .{
                .type = @"type",
                .value = value,
            });

            return self.emit(alloc, .decl_var, .{
                .a = @enumFromInt(@intFromEnum(name)),
                .b = extra_idx,
            });
        },
        .func_decl => {
            const name_tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[name_tok];

            const func_decl = self.ast.unpackExtraData(AST.FuncDecl, data.a);

            const params = self.ast.extraSlice(func_decl.params_start, func_decl.params_end);
            var param_refs: std.ArrayList(BaseRef) = .empty;
            defer param_refs.deinit(alloc);
            for (params) |param| {
                const ref = try self.lower(alloc, param);
                try param_refs.append(alloc, ref);
            }

            const params_start: BaseRef = @enumFromInt(self.extra_data.items.len);
            try self.extra_data.appendSlice(alloc, param_refs.items);
            const params_end: BaseRef = @enumFromInt(self.extra_data.items.len);

            const ret_type = try self.lower(alloc, func_decl.ret_type);
            const body = if (func_decl.body != .none) try self.lower(alloc, func_decl.body) else .none;

            const extra_idx = try self.packExtraData(alloc, FuncInfo, .{
                .params_start = params_start,
                .params_end = params_end,
                .ret_type = ret_type,
                .body = body,
                .flags = func_decl.flags,
            });

            return self.emit(alloc, .decl_func, .{
                .a = @enumFromInt(@intFromEnum(name)),
                .b = extra_idx,
            });
        },
        .namespace_decl => {
            const name_tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[name_tok];

            const block_ref: AST.Node.Ref = @enumFromInt(@intFromEnum(data.a));
            const block = try self.lower(alloc, block_ref);

            return self.emit(alloc, .decl_namespace, .{
                .a = @enumFromInt(@intFromEnum(name)),
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
            const ast_info = self.ast.unpackExtraData(AST.ElseIfInfo, data.b);
            const body = try self.lower(alloc, ast_info.body);
            const else_node = try self.lower(alloc, ast_info.else_node);
            const extra_idx = try self.packExtraData(alloc, IfElseInfo, .{
                .body = body,
                .else_node = else_node,
            });
            return self.emit(alloc, .if_else, .{ .a = condition, .b = extra_idx });
        },
        .param => {
            const name_tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[name_tok];
            const @"type" = try self.lower(alloc, data.a);
            return self.emit(alloc, .param, .{
                .a = @enumFromInt(@intFromEnum(name)),
                .b = @"type",
            });
        },
        .block => {
            const statements = self.ast.extraSlice(data.a, data.b);

            var stmt_refs: std.ArrayList(BaseRef) = .empty;
            defer stmt_refs.deinit(alloc);
            for (statements) |stmt| {
                const node_ref: AST.Node.Ref = @enumFromInt(@intFromEnum(stmt));
                const ref = try self.lower(alloc, node_ref);
                try stmt_refs.append(alloc, ref);
            }

            // copy to extra_data contiguously
            const extra_start: BaseRef = @enumFromInt(self.extra_data.items.len);
            try self.extra_data.appendSlice(alloc, stmt_refs.items);
            const extra_end: BaseRef = @enumFromInt(self.extra_data.items.len);
            return self.emit(alloc, .block, .{ .a = extra_start, .b = extra_end });
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
                .a = @enumFromInt(@intFromEnum(name)),
            });
        },
        .int_literal => {
            const tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[tok];
            return try self.emit(alloc, .int_literal, .{
                .a = @enumFromInt(@intFromEnum(name)),
            });
        },
        .float_literal => {
            const tok = self.ast.nodeMainToken(node);
            const name = self.ast.tokens.items(.str_id)[tok];
            return try self.emit(alloc, .float_literal, .{
                .a = @enumFromInt(@intFromEnum(name)),
            });
        },
        .@"error" => return self.emit(alloc, .trap, .{ .a = data.a }),
        else => {
            const diag_ref = try self.addDiagnostic(.lowering_unsupported_node);
            return self.emit(alloc, .trap, .{ .a = @enumFromInt(@intFromEnum(diag_ref)) });
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

fn emit(self: *Self, alloc: mem.Allocator, tag: Inst.Tag, data: Inst.Data) !Inst.Ref {
    try self.insts.append(alloc, .{ .tag = tag, .data = data });
    return @enumFromInt(self.insts.len - 1);
}

fn packExtraData(self: *Self, alloc: mem.Allocator, comptime T: type, data: T) !BaseRef {
    const start: BaseRef = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
    const fields = @typeInfo(T).@"struct".fields;
    inline for (fields) |field| {
        const val = @field(data, field.name);
        try self.extra_data.append(alloc, switch (field.type) {
            BaseRef => val,
            u32 => @enumFromInt(val),
            StringPool.ID => @enumFromInt(@intFromEnum(val)),
            else => @compileError("unsupported extra_data field type"),
        });
    }
    return start;
}

/// read a packed struct from extra_data starting from idx.
pub fn unpackExtraData(self: *const Self, comptime T: type, idx: BaseRef) T {
    const fields = @typeInfo(T).@"struct".fields;
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        const val = self.extra_data.items[@intFromEnum(idx) + i];
        @field(result, field.name) = switch (field.type) {
            BaseRef => val,
            u32 => @intFromEnum(val),
            StringPool.ID => @enumFromInt(@intFromEnum(val)),
            else => @compileError("unsupported extra_data field type"),
        };
    }
    return result;
}

pub fn extraSlice(self: *const Self, start: BaseRef, end: BaseRef) []const BaseRef {
    return self.extra_data.items[@intFromEnum(start)..@intFromEnum(end)];
}

pub fn rootDecls(self: *const Self) []const BaseRef {
    return self.extraSlice(self.root_start, self.root_end);
}

pub fn render(self: *const Self, alloc: mem.Allocator) ![]const u8 {
    var aw: std.Io.Writer.Allocating = .init(alloc);
    const w = &aw.writer;

    for (0..self.insts.len) |idx| {
        const inst = self.insts.get(idx);
        switch (inst.tag) {
            .int_literal => {
                const val = self.str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                try w.print("%{d: <3} int({s})\n", .{ idx, val });
            },
            .float_literal => {
                const val = self.str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                try w.print("%{d: <3} float({s})\n", .{ idx, val });
            },
            .str_literal => {
                const val = self.str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
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
                const name = self.str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                try w.print("%{d: <3} ref(\"{s}\")\n", .{ idx, name });
            },
            .decl_const, .decl_var => {
                const tag_name = @tagName(inst.tag);
                const name = self.str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                const info = self.unpackExtraData(DeclInfo, inst.data.b);

                try w.print("%{d: <3} {s}(\"{s}\"", .{ idx, tag_name, name });

                if (info.type != .none)
                    try w.print(", type=%{d}", .{@intFromEnum(info.type)})
                else
                    try w.print(", type=<pending>", .{});

                try w.print(", value=%{d})\n", .{@intFromEnum(info.value)});
            },
            .decl_namespace => {
                const tag_name = @tagName(inst.tag);
                const name = self.str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                try w.print("%{d: <3} {s}(\"{s}\", body=%{d})\n", .{ idx, tag_name, name, @intFromEnum(inst.data.b) });
            },
            .param => {
                const name = self.str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
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
                const refs = self.extraSlice(inst.data.a, inst.data.b);
                for (refs, 0..) |r, j| {
                    if (j > 0) try w.writeAll(", ");
                    try w.print("%{d}", .{@intFromEnum(r)});
                }
                try w.writeAll(")\n");
            },
            .decl_func => {
                const tag_name = @tagName(inst.tag);
                const name = self.str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                const info = self.unpackExtraData(FuncInfo, inst.data.b);
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
                const info = self.unpackExtraData(IfElseInfo, inst.data.b);
                try w.print(
                    "%{d: <3} if_else(cond=%{d}, body=%{d}, else=%{d})\n",
                    .{ idx, @intFromEnum(inst.data.a), @intFromEnum(info.body), @intFromEnum(info.else_node) },
                );
            },
        }
    }

    return aw.toOwnedSlice();
}
