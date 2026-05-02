const std = @import("std");
const mem = std.mem;

/// instructions. indexed by inst ref.
insts: std.MultiArrayList(Inst) = .{},
decls: std.MultiArrayList(DeclInfo) = .{},
funcs: std.MultiArrayList(FuncInfo) = .{},
branches: std.MultiArrayList(IfElseInfo) = .{},
ref_lists: std.ArrayList(Inst.Ref) = .empty,

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

        /// data:
        /// * a: string pool id (value)
        int_literal,

        /// data:
        /// * a: string pool id (value)
        float_literal,

        /// data:
        /// * a: immediate value (0 = false, 1 = true)
        bool_literal,

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
        /// * a: operand ref
        not,

        /// data:
        /// * a: operand ref
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

        // functions

        /// data:
        /// * a: string id (name)
        /// * b: type (encoded)
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
        /// * a: condition ref
        /// * b: body ref
        if_simple,

        /// data:
        /// * a: condition ref
        /// * b: branch ref
        if_else,
    };

    pub const Type = enum {
        i32,
        f32,
        bool,
        void,
        err,
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
    value: BaseRef,
    type: Inst.Type,
};

pub const FuncInfo = struct {
    params_start: BaseRef,
    params_end: BaseRef,
    body: Inst.Ref,
    ret_type: Inst.Type,
    flags: u32,
};

pub const IfElseInfo = struct {
    body: Inst.Ref,
    else_node: Inst.Ref,
};

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.insts.deinit(alloc);
    self.decls.deinit(alloc);
    self.funcs.deinit(alloc);
    self.branches.deinit(alloc);
    self.ref_lists.deinit(alloc);
}

pub fn emit(self: *Self, alloc: mem.Allocator, tag: Inst.Tag, data: Inst.Data) !Inst.Ref {
    try self.insts.append(alloc, .{ .tag = tag, .data = data });
    return @enumFromInt(self.insts.len - 1);
}

pub fn emitDeclInfo(self: *Self, alloc: mem.Allocator, info: DeclInfo) !DeclRef {
    const ref: DeclRef = @enumFromInt(@as(u32, @intCast(self.decls.len)));
    try self.decls.append(alloc, info);
    return ref;
}

pub fn declInfo(self: *const Self, ref: DeclRef) DeclInfo {
    return self.decls.get(@intFromEnum(ref));
}

pub fn emitFuncInfo(self: *Self, alloc: mem.Allocator, info: FuncInfo) !FuncRef {
    const ref: FuncRef = @enumFromInt(@as(u32, @intCast(self.funcs.len)));
    try self.funcs.append(alloc, info);
    return ref;
}

pub fn funcInfo(self: *const Self, ref: FuncRef) FuncInfo {
    return self.funcs.get(@intFromEnum(ref));
}

pub fn emitBranchInfo(self: *Self, alloc: mem.Allocator, info: IfElseInfo) !BranchRef {
    const ref: BranchRef = @enumFromInt(@as(u32, @intCast(self.branches.len)));
    try self.branches.append(alloc, info);
    return ref;
}

pub fn branchInfo(self: *const Self, ref: BranchRef) IfElseInfo {
    return self.branches.get(@intFromEnum(ref));
}

pub fn appendRefList(self: *Self, alloc: mem.Allocator, refs: []const BaseRef) !struct { start: BaseRef, end: BaseRef } {
    const start: BaseRef = @enumFromInt(@as(u32, @intCast(self.ref_lists.items.len)));
    try self.ref_lists.appendSlice(alloc, refs);
    const end: BaseRef = @enumFromInt(@as(u32, @intCast(self.ref_lists.items.len)));
    return .{ .start = start, .end = end };
}

pub fn refSlice(self: *const Self, start: BaseRef, end: BaseRef) []const BaseRef {
    return self.ref_lists.items[@intFromEnum(start)..@intFromEnum(end)];
}

pub fn asBaseRef(ref: anytype) BaseRef {
    const Ref = @TypeOf(ref);
    if (Ref != DeclRef and Ref != FuncRef and Ref != BranchRef)
        @compileError("expected a typed mir payload ref");
    return @enumFromInt(@intFromEnum(ref));
}

pub fn asDeclRef(ref: BaseRef) DeclRef {
    return @enumFromInt(@intFromEnum(ref));
}

pub fn asFuncRef(ref: BaseRef) FuncRef {
    return @enumFromInt(@intFromEnum(ref));
}

pub fn asBranchRef(ref: BaseRef) BranchRef {
    return @enumFromInt(@intFromEnum(ref));
}

pub fn refListStart(self: *const Self) BaseRef {
    return @enumFromInt(@as(u32, @intCast(self.ref_lists.items.len)));
}

pub fn appendRef(self: *Self, alloc: mem.Allocator, ref: BaseRef) !void {
    try self.ref_lists.append(alloc, ref);
}

pub fn refListEnd(self: *const Self) BaseRef {
    return @enumFromInt(@as(u32, @intCast(self.ref_lists.items.len)));
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
                const name = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                try w.print("%{d: <3} ref(\"{s}\")\n", .{ idx, name });
            },
            .decl_const, .decl_var => {
                const tag_name = @tagName(inst.tag);
                const name = str_pool.get(@enumFromInt(@intFromEnum(inst.data.a)));
                const info = self.declInfo(asDeclRef(inst.data.b));

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
                const refs = self.refSlice(inst.data.a, inst.data.b);
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
                const info = self.funcInfo(asFuncRef(inst.data.b));
                const cc: AST.FuncDecl.CallingConvention =
                    @enumFromInt((info.flags & AST.FuncDecl.Flag.cc_mask) >> AST.FuncDecl.Flag.cc_shift);

                const param_refs = self.refSlice(info.params_start, info.params_end);

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
