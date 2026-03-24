const std = @import("std");
const mem = std.mem;

const BaseIndex = @import("../root.zig").BaseIndex;
const Source = @import("../source/Source.zig");
const Token = @import("../lexer/Token.zig");
const Lexer = @import("../lexer/Lexer.zig");

nodes: Nodes.Slice,
errors: Errors.Slice,
extra_data: []const Slot,
token_tags: []const Token.Tag,
token_starts: []const Token.Index,

const Self = @This();

pub const Nodes = std.MultiArrayList(Node);
pub const Slots = std.ArrayListUnmanaged(Slot);
pub const Errors = std.MultiArrayList(Error);

pub const Slot = BaseIndex;

/// index into node list.
pub const NodeIndex = enum(Slot) {
    none = std.math.maxInt(Slot), // no-node sentinel
    _,

    pub fn unwrap(self: NodeIndex) ?Slot {
        if (self == .none) return null;
        return @intFromEnum(self);
    }

    pub fn asExtra(self: NodeIndex) Slot {
        return @intFromEnum(self);
    }
};

/// index into flat extra-data array.
pub const ExtraIndex = Slot;

pub const Node = struct {
    tag: Tag,
    main_token: Token.Index, // anchor in source
    data: Data,

    /// struct of registers for arbitrary data.
    pub const Data = struct {
        a: Slot = 0,
        b: Slot = 0,
    };

    pub const Tag = enum {
        /// main_token: unused
        /// extra_data[a..b] contains top-level node indices
        root,

        /// name :: value
        /// main_token: identifier
        /// a: value expression (NodeIndex)
        /// b: unused
        const_decl,

        /// name = value
        /// main_token: identifier
        /// a: value expression (NodeIndex)
        /// b: unused
        var_decl,

        /// name :: func(params) return_type { body }
        /// main_token: func keyword
        /// a: ExtraIndex → FuncDecl
        /// b: unused
        func_decl,

        /// name type (e.g. a int)
        /// main_token: identifier (param name)
        /// a: type expression (NodeIndex)
        /// b: unused
        param,

        /// `-a`, `!a`
        /// main_token: operator token
        /// a: operand (NodeIndex)
        /// b: unused
        unary_op,

        /// `a + b`, `a * b`, etc.
        /// main_token: operator token
        /// a: left operand (NodeIndex)
        /// b: right operand (NodeIndex)
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
        /// a: inner expression (NodeIndex)
        /// b: unused
        grouped_expr,

        /// a block of statements: `{ ... }`
        /// main_token: `{` token
        /// extra_data[a..b] contains statement node indices
        block,

        /// return expr
        /// main_token: `return` token
        /// a: value expression (NodeIndex), .none for bare return
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
    params_start: ExtraIndex,
    params_end: ExtraIndex,
    return_type: NodeIndex, // .none for void
    body: NodeIndex, // .none for extern declarations
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
    token: Token.Index,
    expected: Token.Tag = .eof,

    pub const Tag = enum {
        expected_expression,
        expected_declaration,
        expected_token,
    };
};

pub fn nodeData(self: *const Self, idx: NodeIndex) Node.Data {
    return self.nodes.items(.data)[@intFromEnum(idx)];
}

pub fn nodeTag(self: *const Self, idx: NodeIndex) Node.Tag {
    return self.nodes.items(.tag)[@intFromEnum(idx)];
}

pub fn nodeMainToken(self: *const Self, idx: NodeIndex) Token.Index {
    return self.nodes.items(.main_token)[@intFromEnum(idx)];
}

/// read a packed struct from extra_data starting from idx.
pub fn extraData(self: *const Self, comptime T: type, idx: ExtraIndex) T {
    const fields = @typeInfo(T).@"struct".fields;
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        const val = self.extra_data[idx + i];
        @field(result, field.name) = switch (field.type) {
            Slot => val,
            NodeIndex => @enumFromInt(val),
            else => @compileError("unsupported extra_data field type"),
        };
    }
    return result;
}

pub fn extraSlice(self: *const Self, start: ExtraIndex, end: ExtraIndex) []const Slot {
    return self.extra_data[start..end];
}

pub fn tokenSlice(self: *const Self, src: []const u8, tok: Token.Index) []const u8 {
    var pos = self.token_starts[tok];
    const scanned = Lexer.nextToken(src, &pos);
    return src[scanned.start..scanned.end];
}

/// render ast as raw honey code.
pub fn render(self: *const Self, alloc: mem.Allocator, src: []const u8) ![]const u8 {
    var buf = std.ArrayListUnmanaged(u8){};
    const root_data = self.nodeData(@enumFromInt(0));
    const decls = self.extra_data[root_data.a..root_data.b];
    for (decls) |decl_idx| {
        try self.renderNode(alloc, &buf, @enumFromInt(decl_idx), src, 0);
    }
    return buf.toOwnedSlice(alloc);
}

fn renderNode(
    self: *const Self,
    alloc: mem.Allocator,
    buf: *std.ArrayListUnmanaged(u8),
    idx: NodeIndex,
    src: []const u8,
    indent: u32,
) !void {
    const data = self.nodeData(idx);
    switch (self.nodeTag(idx)) {
        .root => unreachable,
        .const_decl => {
            const ident_idx = self.nodeMainToken(idx);
            const ident_val = self.tokenSlice(src, ident_idx);
            const val_idx: NodeIndex = @enumFromInt(data.a);

            // write `name :: value\n`
            try buf.appendNTimes(alloc, ' ', indent);
            try buf.appendSlice(alloc, ident_val);
            try buf.appendSlice(alloc, " :: ");
            try self.renderNode(alloc, buf, val_idx, src, 0);
            try buf.append(alloc, '\n');
        },
        .var_decl => {
            const ident_idx = self.nodeMainToken(idx);
            const ident_val = self.tokenSlice(src, ident_idx);
            const val_idx: NodeIndex = @enumFromInt(data.a);

            // write `name = value\n`
            try buf.appendNTimes(alloc, ' ', indent);
            try buf.appendSlice(alloc, ident_val);
            try buf.appendSlice(alloc, " = ");
            try self.renderNode(alloc, buf, val_idx, src, 0);
            try buf.append(alloc, '\n');
        },
        .func_decl => {
            const func = self.extraData(FuncDecl, data.a);
            const params = self.extra_data[func.params_start..func.params_end];

            // write `name :: <cc> func(params) ret_type { body }\n`
            try buf.appendNTimes(alloc, ' ', indent);
            try buf.appendSlice(alloc, self.tokenSlice(src, self.nodeMainToken(idx)));
            try buf.appendSlice(alloc, " :: ");

            switch ((func.flags & FuncDecl.Flag.cc_mask) >> FuncDecl.Flag.cc_shift) {
                @intFromEnum(FuncDecl.CallingConvention.c) => try buf.appendSlice(alloc, "c "),
                @intFromEnum(FuncDecl.CallingConvention.honey) => {}, // default, write nothing
                else => unreachable,
            }

            try buf.appendSlice(alloc, "func(");

            for (params, 0..) |param_idx, i| {
                if (i > 0) try buf.appendSlice(alloc, ", ");
                try self.renderNode(alloc, buf, @enumFromInt(param_idx), src, 0);
            }

            try buf.appendSlice(alloc, ") ");
            try self.renderNode(alloc, buf, func.return_type, src, 0);
            try buf.append(alloc, ' ');
            try self.renderNode(alloc, buf, func.body, src, indent);
            try buf.append(alloc, '\n');
        },
        .param => {
            const param_idx = self.nodeMainToken(idx);
            const param_val = self.tokenSlice(src, param_idx);
            const type_idx: NodeIndex = @enumFromInt(data.a);

            // write `name type_expr`
            try buf.appendSlice(alloc, param_val);
            try buf.append(alloc, ' ');
            try self.renderNode(alloc, buf, type_idx, src, 0);
        },
        .block => {
            // write `{ body }`
            const stmts = self.extra_data[data.a..data.b];
            try buf.appendSlice(alloc, "{\n");
            for (stmts) |stmt_idx| {
                const stmt_node_idx: NodeIndex = @enumFromInt(stmt_idx);
                try self.renderNode(alloc, buf, stmt_node_idx, src, indent + 4);
            }
            try buf.appendNTimes(alloc, ' ', indent);
            try buf.append(alloc, '}');
        },
        .return_val => {
            const val_idx: NodeIndex = @enumFromInt(data.a);

            // write `return <expr>\n`
            try buf.appendNTimes(alloc, ' ', indent);
            try buf.appendSlice(alloc, "return");
            if (val_idx != .none) {
                try buf.append(alloc, ' ');
                try self.renderNode(alloc, buf, val_idx, src, 0);
            }
            try buf.append(alloc, '\n');
        },
        .identifier => {
            const ident_idx = self.nodeMainToken(idx);
            const ident_val = self.tokenSlice(src, ident_idx);

            // write token string
            try buf.appendSlice(alloc, ident_val);
        },
        .number_literal => {
            const num_idx = self.nodeMainToken(idx);
            const num_val = self.tokenSlice(src, num_idx);

            // write token string
            try buf.appendSlice(alloc, num_val);
        },
        .@"error" => {
            try buf.appendSlice(alloc, "<error>");
        },
        else => {
            try buf.appendSlice(alloc, "<?>");
        },
    }
}
