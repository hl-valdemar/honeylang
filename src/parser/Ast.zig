const std = @import("std");

const Token = @import("../lexer/Token.zig");
const BaseIndex = @import("../root.zig").BaseIndex;

nodes: Nodes.Slice,
errors: Errors.Slice,
extra_data: []const Slot,
token_tags: []const Token.Tag,
token_starts: []const Token.Index,

const Self = @This();

pub const Nodes = std.MultiArrayList(Node);
pub const Slots = std.ArrayListUnmanaged(Slot);
pub const Errors = std.MultiArrayList(Error);

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
            ExtraIndex => val,
            else => @compileError("unsupported extra_data field type"),
        };
    }
    return result;
}

pub fn extraSlice(self: *const Self, start: ExtraIndex, end: ExtraIndex) []const Slot {
    return self.extra_data[start..end];
}

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
        pub const is_variadic: u32 = 1 << 0;
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

    pub const Tag = enum {
        expected_expression,
        expected_identifier,
        expected_token,
        expected_newline,
        expected_declaration,
    };
};
