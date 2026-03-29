const std = @import("std");
const mem = std.mem;

insts: std.MultiArrayList(Inst),
extra_data: std.ArrayListUnmanaged(Inst.Ref),

const Self = @This();

const BaseRef = @import("../root.zig").BaseRef;
const StringPool = @import("../util/StringPool.zig");
const Ast = @import("../parser/Ast.zig");

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Ref = BaseRef;

    pub const Data = struct {
        a: BaseRef = @enumFromInt(0),
        b: BaseRef = @enumFromInt(0),
    };

    pub const Tag = enum(u32) {
        // ilterals
        int_literal, // a: string pool id
        str_literal, // a: string pool id

        // math
        // a: left Ref
        // b: right Ref
        add,
        sub,
        mul,
        div,

        // references
        ref, // a: Ref to the decl/param instruction
        type_ref, // a: Ref to a type (could be builtin or computed)

        // decls
        // a: string id (name), b: extra_data index → DeclInfo
        decl_const,
        decl_var,

        // functions
        param, // a: string id (name), b: Ref to type
        ret, // a: Ref to value (or .none)
        block, // a, b: extra_data range to Ref indices
        func, // a: string id (name), b: extra_data index → FuncInfo
    };
};

const DeclInfo = struct {
    type: Inst.Ref,
    value: Inst.Ref,
};

const FuncInfo = struct {
    params_start: BaseRef, // range into extra_data
    params_end: BaseRef,
    ret_type: Inst.Ref, // .none when void
    body: Inst.Ref, // ref to block instruction
    flags: u32,
};

pub fn init() Self {
    return .{
        .insts = .{},
        .extra_data = .{},
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.insts.deinit(alloc);
    self.extra_data.deinit(alloc);
}

pub fn lower(
    self: *Self,
    alloc: mem.Allocator,
    node: Ast.Node.Ref,
    ast: *const Ast,
    str_pool: *const StringPool,
) !Inst.Ref {
    const data = ast.nodeData(node);
    switch (ast.nodeTag(node)) {
        .root => {
            for (ast.extraSlice(data.a, data.b)) |decl_ref|
                _ = try self.lower(alloc, decl_ref, ast, str_pool);
            return .none;
        },
        .const_decl => {
            const name_tok = ast.nodeMainToken(node);
            const name = ast.tokens.items(.str_id)[name_tok];

            const @"type" = if (data.a != .none) try self.lower(alloc, data.a, ast, str_pool) else .none;
            const value = try self.lower(alloc, data.b, ast, str_pool);

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
            const name_tok = ast.nodeMainToken(node);
            const name = ast.tokens.items(.str_id)[name_tok];

            const @"type" = if (data.a != .none) try self.lower(alloc, data.a, ast, str_pool) else .none;
            const value = try self.lower(alloc, data.b, ast, str_pool);

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
            const name_tok = ast.nodeMainToken(node);
            const name = ast.tokens.items(.str_id)[name_tok];

            const func_decl = ast.unpackExtraData(Ast.FuncDecl, data.a);

            const params = ast.extraSlice(func_decl.params_start, func_decl.params_end);
            const params_start: BaseRef = @enumFromInt(self.insts.len);
            for (params) |param| _ = try self.lower(alloc, param, ast, str_pool);
            const params_end: BaseRef = @enumFromInt(self.insts.len);

            const ret_type = try self.lower(alloc, func_decl.ret_type, ast, str_pool);
            const body = try self.lower(alloc, func_decl.body, ast, str_pool);

            const extra_idx = try self.packExtraData(alloc, FuncInfo, .{
                .params_start = params_start,
                .params_end = params_end,
                .ret_type = ret_type,
                .body = body,
                .flags = func_decl.flags,
            });

            return self.emit(alloc, .func, .{
                .a = @enumFromInt(@intFromEnum(name)),
                .b = extra_idx,
            });
        },
        .param => {
            const name_tok = ast.nodeMainToken(node);
            const name = ast.tokens.items(.str_id)[name_tok];
            const @"type" = try self.lower(alloc, data.a, ast, str_pool);
            return self.emit(alloc, .param, .{
                .a = @enumFromInt(@intFromEnum(name)),
                .b = @"type",
            });
        },
        .block => {
            const statements = ast.extraSlice(data.a, data.b);
            const start: BaseRef = @enumFromInt(self.insts.len);
            for (statements) |stmt| {
                const node_ref: Ast.Node.Ref = @enumFromInt(@intFromEnum(stmt));
                _ = try self.lower(alloc, node_ref, ast, str_pool);
            }
            const end: BaseRef = @enumFromInt(self.insts.len);
            return self.emit(alloc, .block, .{ .a = start, .b = end });
        },
        .expr_statement => return self.lower(alloc, data.a, ast, str_pool),
        .return_val => return self.lower(alloc, data.a, ast, str_pool),
        .binary_op => {
            const left = try self.lower(alloc, data.a, ast, str_pool);
            const right = try self.lower(alloc, data.b, ast, str_pool);
            const op_tok = ast.nodeMainToken(node);
            const tag = switch (ast.tokens.items(.tag)[op_tok]) {
                .plus => .add,
                else => unreachable,
            };
            return self.emit(alloc, tag, .{ .a = left, .b = right });
        },
        .identifier => {
            const tok = ast.nodeMainToken(node);
            const name = ast.tokens.items(.str_id)[tok];
            return try self.emit(alloc, .type_ref, .{
                .a = @enumFromInt(@intFromEnum(name)),
            });
        },
        .number_literal => {
            const tok = ast.nodeMainToken(node);
            const name = ast.tokens.items(.str_id)[tok];
            return try self.emit(alloc, .int_literal, .{
                .a = @enumFromInt(@intFromEnum(name)),
            });
        },
        else => {
            std.debug.print("[TODO]: unsupported node in lowering: {any}\n", .{ast.nodeTag(node)});
            @panic("");
        },
    }
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
    return self.extra_data[@intFromEnum(start)..@intFromEnum(end)];
}

pub fn render(self: *const Self, alloc: mem.Allocator, str_pool: *const StringPool) ![]const u8 {
    var buf = std.ArrayListUnmanaged(u8){};
    const w = buf.writer(alloc);

    for (0..self.insts.len) |idx| {
        const inst = self.insts.get(idx);
        switch (inst.tag) {
            .int_literal => {
                const name = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                try w.print("%{d} = int({s})\n", .{ idx, name });
            },
            .str_literal => {
                const name = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                try w.print("%{d} = str(\"{s}\")\n", .{ idx, name });
            },
            .add, .sub, .mul, .div => {
                const tag_name = @tagName(inst.tag);
                try w.print("%{d} = {s}(%{d}, %{d})\n", .{ idx, tag_name, @intFromEnum(inst.data.a), @intFromEnum(inst.data.b) });
            },
            .ref => {
                try w.print("%{d} = ref(%{d})\n", .{ idx, @intFromEnum(inst.data.a) });
            },
            .type_ref => {
                const name = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                try w.print("%{d} = type_ref({s})\n", .{ idx, name });
            },
            .decl_const, .decl_var => {
                const tag_name = @tagName(inst.tag);
                const name = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                const info = self.unpackExtraData(DeclInfo, inst.data.b);
                try w.print("%{d} = {s}(\"{s}\"", .{ idx, tag_name, name });
                if (info.type != .none) {
                    try w.print(", type=%{d}", .{@intFromEnum(info.type)});
                } else {
                    try w.print(", type=<pending>", .{});
                }
                try w.print(", value=%{d})\n", .{@intFromEnum(info.value)});
            },
            .param => {
                const name = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                try w.print("%{d} = param(\"{s}\", type=%{d})\n", .{ idx, name, @intFromEnum(inst.data.b) });
            },
            .ret => {
                if (inst.data.a != .none) {
                    try w.print("%{d} = ret(%{d})\n", .{ idx, @intFromEnum(inst.data.a) });
                } else {
                    try w.print("%{d} = ret()\n", .{idx});
                }
            },
            .block => {
                try w.print("%{d} = block(%{d}..%{d})\n", .{ idx, @intFromEnum(inst.data.a), @intFromEnum(inst.data.b) });
            },
            .func => {
                const name = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                const info = self.unpackExtraData(FuncInfo, inst.data.b);
                const cc: Ast.FuncDecl.CallingConvention = @enumFromInt((info.flags & Ast.FuncDecl.Flag.cc_mask) >> Ast.FuncDecl.Flag.cc_shift);
                try w.print("%{d} = func(\"{s}\", cc={s}, params=%{d}..%{d}", .{ idx, name, @tagName(cc), @intFromEnum(info.params_start), @intFromEnum(info.params_end) });
                if (info.ret_type != .none) {
                    try w.print(", ret=%{d}", .{@intFromEnum(info.ret_type)});
                }
                try w.print(", body=%{d})\n", .{@intFromEnum(info.body)});
            },
        }
    }

    return buf.toOwnedSlice(alloc);
}
