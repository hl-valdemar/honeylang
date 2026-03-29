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
    main_token: Token.Ref, // anchor in source
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

        /// name :: value
        /// main_token: identifier
        /// a: type expression (Ref)
        /// b: value expression (Ref)
        const_decl,

        /// name = value
        /// main_token: identifier
        /// a: type expression (Ref)
        /// b: value expression (Ref)
        var_decl,

        /// name :: func(params) return_type { body }
        /// main_token: identifier
        /// a: BaseRef → FuncDecl
        /// b: unused
        func_decl,

        /// name type (e.g. a int)
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

        /// a numeric literal.
        /// main_token: the number token
        /// a, b: unused
        number_literal,

        /// a string literal.
        /// main_token: the string token
        /// a, b: unused
        string_literal,

        /// a grouped expression: `(expr)`
        /// main_token: `(` token
        /// a: inner expression (Ref)
        /// b: unused
        grouped_expr,

        /// a block of statements: `{ ... }`
        /// main_token: `{` token
        /// extra_data[a..b] contains statement node indices
        block,

        /// return expr
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
    params_start: BaseRef,
    params_end: BaseRef,
    return_type: Node.Ref, // .none for void
    body: Node.Ref, // .none for extern declarations
    flags: u32, // packed: bits[0] = is_variadic, bits[1..3] for calling convention

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

pub fn nodeData(self: *const Self, idx: Node.Ref) Node.Data {
    return self.nodes.items(.data)[@intFromEnum(idx)];
}

pub fn nodeTag(self: *const Self, idx: Node.Ref) Node.Tag {
    return self.nodes.items(.tag)[@intFromEnum(idx)];
}

pub fn nodeMainToken(self: *const Self, idx: Node.Ref) Token.Ref {
    return self.nodes.items(.main_token)[@intFromEnum(idx)];
}

/// read a packed struct from extra_data starting from idx.
pub fn extraData(self: *const Self, comptime T: type, idx: BaseRef) T {
    const fields = @typeInfo(T).@"struct".fields;
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        const val = self.extra_data[@intFromEnum(idx) + i];
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

pub fn tokenSlice(self: *const Self, idx: Token.Ref, src: []const u8) []const u8 {
    var start = self.tokens.items(.start)[idx];
    const token = Lexer.nextToken(src, &start);
    return src[token.start..token.end];
}

/// render ast as raw honey code.
pub fn render(self: *const Self, alloc: mem.Allocator, src: []const u8, str_pool: *const StringPool) ![]const u8 {
    var buf = std.ArrayListUnmanaged(u8){};
    const root_idx: Node.Ref = @enumFromInt(0);
    try self.renderNode(alloc, &buf, root_idx, src, str_pool, 0);
    return buf.toOwnedSlice(alloc);
}

fn renderNode(
    self: *const Self,
    alloc: mem.Allocator,
    buf: *std.ArrayListUnmanaged(u8),
    idx: Node.Ref,
    src: []const u8,
    str_pool: *const StringPool,
    indent: u32,
) mem.Allocator.Error!void {
    const data = self.nodeData(idx);
    switch (self.nodeTag(idx)) {
        .root => {
            try self.renderDeclList(alloc, buf, self.extraSlice(data.a, data.b), src, str_pool, indent);
        },
        .const_decl => {
            const ident_idx = self.nodeMainToken(idx);

            // write `name [type] :: value\n`
            try buf.appendNTimes(alloc, ' ', indent);
            try buf.appendSlice(alloc, str_pool.get(self.tokens.items(.str_id)[ident_idx]));
            if (data.a != .none) {
                try buf.append(alloc, ' ');
                try self.renderNode(alloc, buf, data.a, src, str_pool, 0);
            }
            try buf.appendSlice(alloc, " :: ");
            try self.renderNode(alloc, buf, data.b, src, str_pool, 0);
            try buf.append(alloc, '\n');
        },
        .var_decl => {
            const ident_idx = self.nodeMainToken(idx);

            // write `name [type] = value\n`
            try buf.appendNTimes(alloc, ' ', indent);
            try buf.appendSlice(alloc, str_pool.get(self.tokens.items(.str_id)[ident_idx]));
            if (data.a != .none) {
                try buf.append(alloc, ' ');
                try self.renderNode(alloc, buf, data.a, src, str_pool, 0);
            }
            try buf.appendSlice(alloc, " = ");
            try self.renderNode(alloc, buf, data.b, src, str_pool, 0);
            try buf.append(alloc, '\n');
        },
        .func_decl => {
            const ident_idx = self.nodeMainToken(idx);
            const func = self.extraData(FuncDecl, data.a);
            const params = self.extraSlice(func.params_start, func.params_end);

            // write `name :: [cc] func(params) type { body }\n`
            try buf.appendNTimes(alloc, ' ', indent);
            try buf.appendSlice(alloc, str_pool.get(self.tokens.items(.str_id)[ident_idx]));
            try buf.appendSlice(alloc, " :: ");

            switch ((func.flags & FuncDecl.Flag.cc_mask) >> FuncDecl.Flag.cc_shift) {
                @intFromEnum(FuncDecl.CallingConvention.c) => try buf.appendSlice(alloc, "c "),
                @intFromEnum(FuncDecl.CallingConvention.honey) => {}, // default, write nothing
                else => unreachable,
            }

            try buf.appendSlice(alloc, "func(");

            for (params, 0..) |param_ref, i| {
                if (i > 0) try buf.appendSlice(alloc, ", ");
                try self.renderNode(alloc, buf, param_ref, src, str_pool, 0);
            }

            try buf.appendSlice(alloc, ") ");
            try self.renderNode(alloc, buf, func.return_type, src, str_pool, 0);
            try buf.append(alloc, ' ');
            try self.renderNode(alloc, buf, func.body, src, str_pool, indent);
            try buf.append(alloc, '\n');
        },
        .param => {
            const param_tok = self.nodeMainToken(idx);
            const param_val = str_pool.get(self.tokens.items(.str_id)[param_tok]);

            // write `name type_expr`
            try buf.appendSlice(alloc, param_val);
            try buf.append(alloc, ' ');
            try self.renderNode(alloc, buf, data.a, src, str_pool, 0);
        },
        .block => {
            // write `{ body }`
            try buf.appendSlice(alloc, "{\n");
            try self.renderDeclList(alloc, buf, self.extraSlice(data.a, data.b), src, str_pool, indent + 4);
            try buf.appendNTimes(alloc, ' ', indent);
            try buf.append(alloc, '}');
        },
        .return_val => {
            // write `return [expr]\n`
            try buf.appendNTimes(alloc, ' ', indent);
            try buf.appendSlice(alloc, "return");
            if (data.a != .none) {
                try buf.append(alloc, ' ');
                try self.renderNode(alloc, buf, data.a, src, str_pool, 0);
            }
            try buf.append(alloc, '\n');
        },
        .binary_op => {
            const op_idx = self.nodeMainToken(idx);
            const op_val = self.tokenSlice(op_idx, src);

            // write `[expr] [op] [expr]`
            try buf.appendNTimes(alloc, ' ', indent);
            try self.renderNode(alloc, buf, data.a, src, str_pool, 0);
            try buf.append(alloc, ' ');
            try buf.appendSlice(alloc, op_val);
            try buf.append(alloc, ' ');
            try self.renderNode(alloc, buf, data.b, src, str_pool, 0);
        },
        .identifier => {
            const ident_idx = self.nodeMainToken(idx);
            try buf.appendSlice(alloc, str_pool.get(self.tokens.items(.str_id)[ident_idx]));
        },
        .number_literal => {
            const num_idx = self.nodeMainToken(idx);
            try buf.appendSlice(alloc, str_pool.get(self.tokens.items(.str_id)[num_idx]));
        },
        .@"error" => {
            try buf.appendSlice(alloc, "<error>");
        },
        else => {
            try buf.appendSlice(alloc, "<?>");
        },
    }
}

/// render a list of declarations and insert blank lines before func_decl nodes.
fn renderDeclList(
    self: *const Self,
    alloc: mem.Allocator,
    buf: *std.ArrayListUnmanaged(u8),
    decls: []const BaseRef,
    src: []const u8,
    str_pool: *const StringPool,
    indent: u32,
) mem.Allocator.Error!void {
    for (decls, 0..) |decl_ref, i| {
        if (i > 0 and self.nodeTag(decl_ref) == .func_decl)
            try buf.append(alloc, '\n');
        try self.renderNode(alloc, buf, decl_ref, src, str_pool, indent);
    }
}
