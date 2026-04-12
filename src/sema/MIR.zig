const std = @import("std");
const mem = std.mem;

/// instructions. Indexed by Inst.Ref.
insts: std.MultiArrayList(Inst) = .{},
extra_data: std.ArrayList(Inst.Ref) = .empty,

const Self = @This();

const BaseRef = @import("../root.zig").BaseRef;
const StringPool = @import("../util/StringPool.zig");
const AST = @import("../parser/AST.zig");
const HIR = @import("../parser/HIR.zig");

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
        /// * a: immediate value (0 = false, 1 = true)
        bool_literal,

        /// extra-data:
        /// * a: string pool id (value)
        str_literal,

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

        // functions

        /// extra-data:
        /// * a: string id (name)
        /// * b: Type (encoded)
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
    };

    pub const Type = enum {
        i32,
        f32,
        bool,
        void,
    };
};

pub const DeclInfo = struct {
    value: BaseRef, // Inst.Ref to the value
    type: Inst.Type,
};

pub const FuncInfo = struct {
    params_start: BaseRef, // range into extra_data
    params_end: BaseRef,
    body: Inst.Ref, // ref to block instruction
    ret_type: Inst.Type,
    flags: u32,
};

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.insts.deinit(alloc);
    self.extra_data.deinit(alloc);
}

pub fn emit(self: *Self, alloc: mem.Allocator, tag: Inst.Tag, data: Inst.Data) !Inst.Ref {
    try self.insts.append(alloc, .{ .tag = tag, .data = data });
    return @enumFromInt(self.insts.len - 1);
}

pub fn packExtraData(self: *Self, alloc: mem.Allocator, comptime T: type, data: T) !BaseRef {
    const start: BaseRef = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
    const fields = @typeInfo(T).@"struct".fields;
    inline for (fields) |field| {
        const val = @field(data, field.name);
        try self.extra_data.append(alloc, switch (field.type) {
            BaseRef => val,
            u32 => @enumFromInt(val),
            Inst.Type => @enumFromInt(@intFromEnum(val)),
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
            Inst.Type => @enumFromInt(@intFromEnum(val)),
            StringPool.ID => @enumFromInt(@intFromEnum(val)),
            else => @compileError("unsupported extra_data field type"),
        };
    }
    return result;
}

pub fn extraSlice(self: *const Self, start: BaseRef, end: BaseRef) []const BaseRef {
    return self.extra_data.items[@intFromEnum(start)..@intFromEnum(end)];
}

pub fn render(self: *const Self, alloc: mem.Allocator, str_pool: *const StringPool) ![]const u8 {
    var aw: std.Io.Writer.Allocating = .init(alloc);
    const w = &aw.writer;

    for (0..self.insts.len) |idx| {
        const inst = self.insts.get(idx);
        switch (inst.tag) {
            .bool_literal => try w.print("%{d: <3} bool({d})\n", .{ idx, @intFromEnum(inst.data.a) }),
            .int_literal => {
                const val = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                try w.print("%{d: <3} int({s})\n", .{ idx, val });
            },
            .float_literal => {
                const val = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                try w.print("%{d: <3} float({s})\n", .{ idx, val });
            },
            .str_literal => {
                const val = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                try w.print("%{d: <3} str(\"{s}\")\n", .{ idx, val });
            },
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
                const name = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                try w.print("%{d: <3} ref(\"{s}\")\n", .{ idx, name });
            },
            .decl_const, .decl_var => {
                const tag_name = @tagName(inst.tag);
                const name = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                const info = self.unpackExtraData(DeclInfo, inst.data.b);

                try w.print("%{d: <3} {s}(\"{s}\"", .{ idx, tag_name, name });
                try w.print(", type={s}", .{@tagName(info.type)});
                try w.print(", value=%{d})\n", .{@intFromEnum(info.value)});
            },
            .param => {
                const name = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                const param_type: Inst.Type = @enumFromInt(@intFromEnum(inst.data.b));
                try w.print(
                    "%{d: <3} param(\"{s}\", type={s})\n",
                    .{ idx, name, @tagName(param_type) },
                );
            },
            .ret => {
                if (inst.data.a != .none)
                    try w.print("%{d: <3} ret(%{d})\n", .{ idx, @intFromEnum(inst.data.a) })
                else
                    try w.print("%{d: <3} ret()\n", .{idx});
            },
            .block => {
                const refs = self.extraSlice(inst.data.a, inst.data.b);
                try w.print("%{d: <3} block(", .{idx});
                for (refs, 0..) |ref, i| {
                    if (i > 0) try w.writeAll(", ");
                    try w.print("%{d}", .{@intFromEnum(ref)});
                }
                try w.writeAll(")\n");
            },
            .decl_func => {
                const tag_name = @tagName(inst.tag);
                const name = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                const info = self.unpackExtraData(FuncInfo, inst.data.b);
                const cc: AST.FuncDecl.CallingConvention =
                    @enumFromInt((info.flags & AST.FuncDecl.Flag.cc_mask) >> AST.FuncDecl.Flag.cc_shift);

                const param_refs = self.extraSlice(info.params_start, info.params_end);

                try w.print("%{d: <3} {s}(\"{s}\", cc={s}, params=(", .{
                    idx,
                    tag_name,
                    name,
                    @tagName(cc),
                });
                for (param_refs, 0..) |ref, i| {
                    if (i > 0) try w.writeAll(", ");
                    try w.print("%{d}", .{@intFromEnum(ref)});
                }
                try w.print("), ret={s}", .{@tagName(info.ret_type)});
                try w.print(", body=%{d})\n", .{@intFromEnum(info.body)});
            },
            .if_simple => try w.print(
                "%{d: <3} if_simple(cond=%{d}, body=%{d})\n",
                .{ idx, @intFromEnum(inst.data.a), @intFromEnum(inst.data.b) },
            ),
        }
    }

    return aw.toOwnedSlice();
}
