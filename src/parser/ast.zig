const std = @import("std");

const Token = @import("../lexer/Token.zig");
const BaseIndex = @import("../root.zig").BaseIndex;

nodes: NodeList.Slice,
extra_data: []const Slot,
errors: []const Error,
token_starts: []const Token.Index,
token_tags: []const Token.Tag,

const Self = @This();

pub const NodeList = std.MultiArrayList(Node);

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

const Slot = BaseIndex;

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

    pub const Data = struct {
        lhs: Slot = 0,
        rhs: Slot = 0,
    };

    pub const Tag = enum {
        /// root node. extra_data[lhs..rhs] contains top-level node indices.
        root,

        /// name :: value
        /// main_token: identifier
        /// lhs: value expression (NodeIndex)
        /// rhs: unused
        const_decl,

        /// name = value
        /// main_token: identifier
        /// lhs: value expression (NodeIndex)
        /// rhs: unused
        var_decl,

        /// name :: func(params) return_type { body }
        /// main_token: func keyword
        /// lhs: ExtraIndex → FuncDecl
        /// rhs: unused
        func_decl,

        /// name type (e.g. a int)
        /// main_token: identifier (param name)
        /// lhs: type expression (NodeIndex)
        /// rhs: unused
        param,

        /// node representing a parse error. downstream passes insert traps.
        /// main_token: token where error was detected
        /// lhs, rhs: unused
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
