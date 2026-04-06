const std = @import("std");
const mem = std.mem;

nodes: Nodes.Slice,
extra_data: []const BaseRef,
errors: Errors.Slice,
tokens: Lexer.Tokens.Slice,

const Self = @This();

pub const BaseRef = @import("../root.zig").BaseRef;

const StringPool = @import("../root.zig").StringPool;
const Source = @import("../source/Source.zig");
const Token = @import("../lexer/Token.zig");
const Lexer = @import("../lexer/Lexer.zig");

pub const Nodes = std.MultiArrayList(Node);
pub const Refs = std.ArrayListUnmanaged(BaseRef);
pub const Errors = std.MultiArrayList(Error);

pub const Node = struct {
    tag: Tag,
    main_tok: Token.Ref, // anchor in source
    data: Data,

    pub const Ref = BaseRef;

    pub const Data = struct {
        a: BaseRef = @enumFromInt(0),
        b: BaseRef = @enumFromInt(0),
    };

    pub const Tag = enum {
        /// main_token: unused
        /// extra_data[a..b] contains top-level node indices
        root,

        /// <name> [type] :: <value>
        /// main_token: identifier
        /// a: type expression (Ref)
        /// b: value expression (Ref)
        const_decl,

        /// <name> [type] = <value>
        /// main_token: identifier
        /// a: type expression (Ref)
        /// b: value expression (Ref)
        var_decl,

        /// `if <expr> { [body] }`
        /// main_token: if
        /// a: condition (Ref)
        /// b: body (Ref)
        if_simple,

        /// `if <expr> { [body] } else { [body] }`
        /// main_token: if
        /// a: condition (Ref)
        /// b: extra-data index (BaseRef) → { body, else_node }
        if_else,

        /// `<name> :: [cc] func([params]) <type> { [body] }`
        /// main_token: identifier
        /// a: extra-data index (BaseRef) → FuncDecl
        /// b: unused
        func_decl,

        /// <name> <type> (e.g. a int)
        /// main_token: identifier (param name)
        /// a: type expression (Ref)
        /// b: unused
        param,

        /// `-a`, `!a`
        /// main_token: operator token
        /// a: operand (Ref)
        /// b: unused
        unary_op,

        /// `a + b`, `a * b`, etc.
        /// main_token: operator token
        /// a: left operand (Ref)
        /// b: right operand (Ref)
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

        /// `(<expr>)`
        /// main_token: `(` token
        /// a: inner expression (Ref)
        /// b: unused
        grouped_expr,

        /// a dangling expression
        /// main_token: first token of expression
        /// a: expr
        /// b: unused
        expr_statement,

        /// `{ [body] }`
        /// main_token: `{` token
        /// extra_data[a..b] contains statement node indices
        block,

        /// `<return> [expr]`
        /// main_token: `return` token
        /// a: value expression (Ref), .none for bare return
        /// b: unused
        return_val,

        /// node representing a parse error. downstream passes insert traps.
        /// main_token: token where error was detected
        /// a, b: unused
        @"error",
    };
};

/// extra data for func_decl. packed into extra-data as consecutive integers.
pub const FuncDecl = struct {
    /// index into extra_data
    params_start: BaseRef,

    /// index into extra_data
    params_end: BaseRef,

    /// .none for void
    ret_type: Node.Ref, // .none for void

    /// .none for extern declarations
    body: Node.Ref,

    /// packed: bits[0] = is_variadic, bits[1..3] for calling convention
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

/// extra data for if_else. packed into extra-data as consecutive integers. (condition in parent node.)
pub const ElseIfInfo = struct {
    body: Node.Ref,
    else_node: Node.Ref,
};

pub const Error = struct {
    tag: Tag,
    token: Token.Ref,
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

pub fn nodeMainToken(self: *const Self, node: Node.Ref) Token.Ref {
    return self.nodes.items(.main_tok)[@intFromEnum(node)];
}

/// read a packed struct from extra_data starting from idx.
pub fn unpackExtraData(self: *const Self, comptime T: type, ref: BaseRef) T {
    const fields = @typeInfo(T).@"struct".fields;
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        const val = self.extra_data[@intFromEnum(ref) + i];
        @field(result, field.name) = switch (field.type) {
            BaseRef => val,
            u32 => @intFromEnum(val),
            else => @compileError("unsupported extra_data field type"),
        };
    }
    return result;
}

pub fn extraSlice(self: *const Self, start: BaseRef, end: BaseRef) []const BaseRef {
    return self.extra_data[@intFromEnum(start)..@intFromEnum(end)];
}

pub fn tokenSlice(self: *const Self, tok: Token.Ref, src: []const u8) []const u8 {
    var start = self.tokens.items(.start)[tok];
    const scanned = Lexer.nextToken(src, &start);
    return src[scanned.start..scanned.end];
}

const Writer = std.ArrayListUnmanaged(u8).Writer;

/// render ast as raw honey code.
pub fn render(self: *const Self, alloc: mem.Allocator, src: []const u8, str_pool: *const StringPool) ![]const u8 {
    var buf = std.ArrayListUnmanaged(u8){};
    const w = buf.writer(alloc);
    const root: Node.Ref = @enumFromInt(0);
    try self.renderNode(w, root, src, str_pool, 0);
    return buf.toOwnedSlice(alloc);
}

fn renderNode(self: *const Self, w: Writer, node: Node.Ref, src: []const u8, str_pool: *const StringPool, indent: u32) Writer.Error!void {
    const data = self.nodeData(node);
    switch (self.nodeTag(node)) {
        .root => {
            try self.renderDeclList(w, self.extraSlice(data.a, data.b), src, str_pool, indent);
        },
        .const_decl => {
            const tok = self.nodeMainToken(node);
            try w.writeByteNTimes(' ', indent);
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
            try w.writeByteNTimes(' ', indent);
            try w.writeAll(str_pool.get(self.tokens.items(.str_id)[tok]));
            if (data.a != .none) {
                try w.writeByte(' ');
                try self.renderNode(w, data.a, src, str_pool, 0);
            }
            try w.writeAll(" = ");
            try self.renderNode(w, data.b, src, str_pool, 0);
            try w.writeByte('\n');
        },
        .if_simple => {
            const tok = self.nodeMainToken(node);
            try w.writeByteNTimes(' ', indent);
            try w.writeAll(str_pool.get(self.tokens.items(.str_id)[tok]));
            try w.writeByte(' ');
            try self.renderNode(w, data.a, src, str_pool, 0);
            try w.writeByte(' ');
            try self.renderNode(w, data.b, src, str_pool, indent);
            try w.writeByte('\n');
        },
        .if_else => {
            const tok = self.nodeMainToken(node);
            const info = self.unpackExtraData(ElseIfInfo, data.b);
            try w.writeByteNTimes(' ', indent);
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
        .func_decl => {
            const tok = self.nodeMainToken(node);
            const func = self.unpackExtraData(FuncDecl, data.a);
            const params = self.extraSlice(func.params_start, func.params_end);
            try w.writeByteNTimes(' ', indent);
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
            try w.writeByte(' ');
            try self.renderNode(w, func.body, src, str_pool, indent);
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
            try self.renderDeclList(w, self.extraSlice(data.a, data.b), src, str_pool, indent + 4);
            try w.writeByteNTimes(' ', indent);
            try w.writeByte('}');
        },
        .return_val => {
            try w.writeByteNTimes(' ', indent);
            try w.writeAll("return");
            if (data.a != .none) {
                try w.writeByte(' ');
                try self.renderNode(w, data.a, src, str_pool, 0);
            }
            try w.writeByte('\n');
        },
        .binary_op => {
            const op_str = self.tokenSlice(self.nodeMainToken(node), src);
            try w.writeByteNTimes(' ', indent);
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
            try w.writeByteNTimes(' ', indent);
            try self.renderNode(w, data.a, src, str_pool, 0);
            try w.writeByte('\n');
        },
        .identifier, .int_literal, .float_literal => {
            const tok = self.nodeMainToken(node);
            try w.writeAll(str_pool.get(self.tokens.items(.str_id)[tok]));
        },
        .@"error" => try w.writeAll("<error>"),
        else => try w.writeAll("<?>"),
    }
}

/// render a list of declarations and insert blank lines before func_decl nodes.
fn renderDeclList(self: *const Self, w: Writer, decls: []const BaseRef, src: []const u8, str_pool: *const StringPool, indent: u32) Writer.Error!void {
    for (decls, 0..) |decl, i| {
        if (i > 0 and self.nodeTag(decl) == .func_decl)
            try w.writeByte('\n');
        try self.renderNode(w, decl, src, str_pool, indent);
    }
}
