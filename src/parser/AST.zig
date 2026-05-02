const std = @import("std");
const mem = std.mem;

nodes: Nodes.Slice,
funcs: Funcs.Slice,
branches: Branches.Slice,
ref_lists: []const Payload,
errors: Errors.Slice,
tokens: Lexer.Tokens.Slice,

const Self = @This();

pub const Payload = @import("../root.zig").Payload;

const StringPool = @import("../root.zig").StringPool;
const Source = @import("../source/Source.zig");
const Token = @import("../lexer/Token.zig");
const Lexer = @import("../lexer/Lexer.zig");

pub const Nodes = std.MultiArrayList(Node);
pub const Funcs = std.MultiArrayList(FuncDecl);
pub const Branches = std.MultiArrayList(ElseIfInfo);
pub const Refs = std.ArrayList(Payload);
pub const Errors = std.MultiArrayList(Error);

pub const Node = struct {
    tag: Tag,
    main_tok: Token.Index, // anchor in source
    data: Data,

    pub const Ref = Payload;

    pub const Data = struct {
        a: Payload = @enumFromInt(0),
        b: Payload = @enumFromInt(0),
    };

    pub const Tag = enum {
        /// main_token: unused
        /// ref_lists[a..b] contains top-level node indices
        root,

        /// <name> [type] :: <value>
        /// main_token: identifier
        /// a: type expression (ref)
        /// b: value expression (ref)
        const_decl,

        /// <name> [type] = <value>
        /// main_token: identifier
        /// a: type expression (ref)
        /// b: value expression (ref)
        var_decl,

        /// `<name> :: [cc] func([params]) <type> { [body] }`
        /// main_token: identifier
        /// b: func ref
        func_decl,

        /// `<name> { [body] }`
        /// main_token: identifier
        /// a: block ref
        namespace_decl,

        /// `import "<path>"` or `<name> :: import "<path>"`
        /// main_token: import token for implicit import, identifier token for explicit import
        /// a: string literal path expression (ref)
        /// b: 1 for explicit import, 0 for implicit import
        import_decl,

        /// <name> <type> (e.g. a int)
        /// main_token: identifier (param name)
        /// a: type expression (Ref)
        /// b: unused
        param,

        /// `-a`, `!a`
        /// main_token: operator token
        /// a: operand (ref)
        /// b: unused
        unary_op,

        /// `a + b`, `a * b`, etc.
        /// main_token: operator token
        /// a: left operand (ref)
        /// b: right operand (ref)
        binary_op,

        /// a bare identifier.
        /// main_token: the identifier token
        /// a, b: unused
        identifier,

        /// an integer literal.
        /// main_token: the integer token
        /// a, b: unused
        int_literal,

        /// an float literal.
        /// main_token: the float token
        /// a, b: unused
        float_literal,

        /// a string literal.
        /// main_token: the string token
        /// a, b: unused
        string_literal,

        /// `a.b`
        /// main_token: `.` token
        /// a: left expression (ref)
        /// b: right identifier (ref)
        qualified_ref,

        /// `(<expr>)`
        /// main_token: `(` token
        /// a: inner expression (ref)
        /// b: unused
        grouped_expr,

        /// a dangling expression
        /// main_token: first token of expression
        /// a: expr
        /// b: unused
        expr_statement,

        /// `{ [body] }`
        /// main_token: `{` token
        /// ref_lists[a..b] contains statement node indices
        block,

        /// `<return> [expr]`
        /// main_token: `return` token
        /// a: value expression (ref), .none for bare return
        /// b: unused
        return_val,

        /// `if <expr> { [body] }`
        /// main_token: if
        /// a: condition (ref)
        /// b: body (ref)
        if_simple,

        /// `if <expr> { [body] } else { [body] }`
        /// main_token: if
        /// a: condition (ref)
        /// b: branch ref
        if_else,

        /// node representing a parse error. downstream passes insert traps.
        /// main_token: token where error was detected
        /// a, b: unused
        @"error",
    };
};

pub const FuncRef = enum(u32) {
    none = std.math.maxInt(u32),
    _,
};

pub const BranchRef = enum(u32) {
    none = std.math.maxInt(u32),
    _,
};

pub const FuncDecl = struct {
    params_start: Payload,
    params_end: Payload,
    ret_type: Node.Ref, // .none for void
    body: Node.Ref,
    flags: u32,

    pub const Flag = struct {
        pub const is_variadic: u32 = 1;
        pub const cc_shift: u5 = 1;
        pub const cc_mask: u32 = 0b110;
    };

    pub const CallingConvention = enum(u2) {
        honey = 0,
        c = 1,
    };
};

pub const ElseIfInfo = struct {
    body: Node.Ref,
    else_node: Node.Ref,
};

pub const Error = struct {
    tag: Tag,
    token: Token.Index,
    expected: Token.Tag = .eof,

    pub const Tag = enum {
        expected_expression,
        expected_declaration,
        expected_token,
    };
};

pub fn nodeData(self: *const Self, node: Node.Ref) Node.Data {
    return self.nodes.items(.data)[@intFromEnum(node)];
}

pub fn nodeTag(self: *const Self, node: Node.Ref) Node.Tag {
    return self.nodes.items(.tag)[@intFromEnum(node)];
}

pub fn nodeMainToken(self: *const Self, node: Node.Ref) Token.Index {
    return self.nodes.items(.main_tok)[@intFromEnum(node)];
}

pub fn funcInfo(self: *const Self, ref: FuncRef) FuncDecl {
    return self.funcs.get(@intFromEnum(ref));
}

pub fn branchInfo(self: *const Self, ref: BranchRef) ElseIfInfo {
    return self.branches.get(@intFromEnum(ref));
}

pub fn refSlice(self: *const Self, start: Payload, end: Payload) []const Payload {
    return self.ref_lists[@intFromEnum(start)..@intFromEnum(end)];
}

pub fn asPayload(ref: anytype) Payload {
    const Ref = @TypeOf(ref);
    if (Ref != FuncRef and Ref != BranchRef)
        @compileError("expected a typed ast payload ref");
    return Payload.from(ref);
}

pub fn asFuncRef(ref: Payload) FuncRef {
    return ref.to(FuncRef);
}

pub fn asBranchRef(ref: Payload) BranchRef {
    return ref.to(BranchRef);
}

pub fn tokenSlice(self: *const Self, tok: Token.Index, src: []const u8) []const u8 {
    var start = self.tokens.items(.start)[tok];
    const scanned = Lexer.nextToken(src, &start);
    return src[scanned.start..scanned.end];
}

const Writer = std.Io.Writer;

fn writeByteNTimes(w: *Writer, byte: u8, n: u32) Writer.Error!void {
    for (0..n) |_| try w.writeByte(byte);
}

/// render ast as raw honey code.
pub fn render(self: *const Self, alloc: mem.Allocator, src: []const u8, str_pool: *const StringPool) ![]const u8 {
    var aw: std.Io.Writer.Allocating = .init(alloc);
    const root: Node.Ref = @enumFromInt(0);
    try self.renderNode(&aw.writer, root, src, str_pool, 0);
    return aw.toOwnedSlice();
}

fn renderNode(self: *const Self, w: *Writer, node: Node.Ref, src: []const u8, str_pool: *const StringPool, indent: u32) Writer.Error!void {
    const indent_size = 4;
    const data = self.nodeData(node);
    switch (self.nodeTag(node)) {
        .root => {
            try self.renderDeclList(w, self.refSlice(data.a, data.b), src, str_pool, indent);
        },
        .const_decl => {
            const tok = self.nodeMainToken(node);
            try writeByteNTimes(w, ' ', indent);
            try w.writeAll(str_pool.get(self.tokens.items(.str_id)[tok]));
            if (data.a != .none) {
                try w.writeByte(' ');
                try self.renderNode(w, data.a, src, str_pool, 0);
            }
            try w.writeAll(" :: ");
            try self.renderNode(w, data.b, src, str_pool, 0);
            try w.writeByte('\n');
        },
        .var_decl => {
            const tok = self.nodeMainToken(node);
            try writeByteNTimes(w, ' ', indent);
            try w.writeAll(str_pool.get(self.tokens.items(.str_id)[tok]));
            if (data.a != .none) {
                try w.writeByte(' ');
                try self.renderNode(w, data.a, src, str_pool, 0);
            }
            try w.writeAll(" = ");
            try self.renderNode(w, data.b, src, str_pool, 0);
            try w.writeByte('\n');
        },
        .func_decl => {
            const tok = self.nodeMainToken(node);
            const func = self.funcInfo(asFuncRef(data.b));
            const params = self.refSlice(func.params_start, func.params_end);
            try writeByteNTimes(w, ' ', indent);
            try w.writeAll(str_pool.get(self.tokens.items(.str_id)[tok]));
            try w.writeAll(" :: ");
            switch ((func.flags & FuncDecl.Flag.cc_mask) >> FuncDecl.Flag.cc_shift) {
                @intFromEnum(FuncDecl.CallingConvention.c) => try w.writeAll("c "),
                @intFromEnum(FuncDecl.CallingConvention.honey) => {},
                else => unreachable,
            }
            try w.writeAll("func(");
            for (params, 0..) |param, i| {
                if (i > 0) try w.writeAll(", ");
                try self.renderNode(w, param, src, str_pool, 0);
            }
            try w.writeAll(") ");
            try self.renderNode(w, func.ret_type, src, str_pool, 0);
            if (func.body != .none) {
                try w.writeByte(' ');
                try self.renderNode(w, func.body, src, str_pool, indent);
            }
            try w.writeByte('\n');
        },
        .namespace_decl => {
            const tok = self.nodeMainToken(node);
            try writeByteNTimes(w, ' ', indent);
            try w.writeAll(str_pool.get(self.tokens.items(.str_id)[tok]));
            try w.writeByte(' ');
            try self.renderNode(w, data.a, src, str_pool, indent);
            try w.writeByte('\n');
        },
        .import_decl => {
            try writeByteNTimes(w, ' ', indent);
            if (@intFromEnum(data.b) != 0) {
                const tok = self.nodeMainToken(node);
                try w.writeAll(str_pool.get(self.tokens.items(.str_id)[tok]));
                try w.writeAll(" :: ");
            }
            try w.writeAll("import ");
            try self.renderNode(w, data.a, src, str_pool, 0);
            try w.writeByte('\n');
        },
        .param => {
            const tok = self.nodeMainToken(node);
            try w.writeAll(str_pool.get(self.tokens.items(.str_id)[tok]));
            try w.writeByte(' ');
            try self.renderNode(w, data.a, src, str_pool, 0);
        },
        .block => {
            try w.writeAll("{\n");
            try self.renderDeclList(w, self.refSlice(data.a, data.b), src, str_pool, indent + indent_size);
            try writeByteNTimes(w, ' ', indent);
            try w.writeByte('}');
        },
        .return_val => {
            try writeByteNTimes(w, ' ', indent);
            try w.writeAll("return");
            if (data.a != .none) {
                try w.writeByte(' ');
                try self.renderNode(w, data.a, src, str_pool, 0);
            }
            try w.writeByte('\n');
        },
        .if_simple => {
            const tok = self.nodeMainToken(node);
            try writeByteNTimes(w, ' ', indent);
            try w.writeAll(str_pool.get(self.tokens.items(.str_id)[tok]));
            try w.writeByte(' ');
            try self.renderNode(w, data.a, src, str_pool, 0);
            try w.writeByte(' ');
            try self.renderNode(w, data.b, src, str_pool, indent);
            try w.writeByte('\n');
        },
        .if_else => {
            const tok = self.nodeMainToken(node);
            const info = self.branchInfo(asBranchRef(data.b));
            try writeByteNTimes(w, ' ', indent);
            try w.writeAll(str_pool.get(self.tokens.items(.str_id)[tok]));
            try w.writeByte(' ');
            try self.renderNode(w, data.a, src, str_pool, 0);
            try w.writeByte(' ');
            try self.renderNode(w, info.body, src, str_pool, indent);
            if (info.else_node != .none) {
                try w.writeAll(" else ");
                try self.renderNode(w, info.else_node, src, str_pool, indent);
            }
            try w.writeByte('\n');
        },
        .binary_op => {
            const op_str = self.tokenSlice(self.nodeMainToken(node), src);
            try writeByteNTimes(w, ' ', indent);
            try self.renderNode(w, data.a, src, str_pool, 0);
            try w.print(" {s} ", .{op_str});
            try self.renderNode(w, data.b, src, str_pool, 0);
        },
        .grouped_expr => {
            try w.writeByte('(');
            try self.renderNode(w, data.a, src, str_pool, 0);
            try w.writeByte(')');
        },
        .expr_statement => {
            try writeByteNTimes(w, ' ', indent);
            try self.renderNode(w, data.a, src, str_pool, 0);
            try w.writeByte('\n');
        },
        .identifier, .int_literal, .float_literal => {
            const tok = self.nodeMainToken(node);
            try w.writeAll(str_pool.get(self.tokens.items(.str_id)[tok]));
        },
        .string_literal => {
            const tok = self.nodeMainToken(node);
            try w.writeByte('"');
            try w.writeAll(str_pool.get(self.tokens.items(.str_id)[tok]));
            try w.writeByte('"');
        },
        .qualified_ref => {
            try self.renderNode(w, data.a, src, str_pool, 0);
            try w.writeByte('.');
            try self.renderNode(w, data.b, src, str_pool, 0);
        },
        .@"error" => try w.writeAll("<error>"),
        else => try w.writeAll("<?>"),
    }
}

/// render a list of declarations and insert blank lines before func_decl nodes.
fn renderDeclList(self: *const Self, w: *Writer, decls: []const Payload, src: []const u8, str_pool: *const StringPool, indent: u32) Writer.Error!void {
    for (decls, 0..) |decl, i| {
        if (i > 0 and (self.nodeTag(decl) == .func_decl or self.nodeTag(decl) == .namespace_decl))
            try w.writeByte('\n');
        try self.renderNode(w, decl, src, str_pool, indent);
    }
}
