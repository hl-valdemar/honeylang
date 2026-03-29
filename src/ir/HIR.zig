const std = @import("std");

insts: std.MultiArrayList(Inst),
extra_data: std.ArrayListUnmanaged(Inst.Ref),

const BaseRef = @import("../root.zig").BaseRef;

const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Ref = BaseRef;

    pub const Data = struct {
        a: BaseRef = @enumFromInt(0),
        b: BaseRef = @enumFromInt(0),
    };

    const Tag = enum(u32) {
        // ilterals
        int_literal, // a: value
        str_literal, // a: string pool id

        // math
        // a: left Ref
        // b: right Ref
        add,
        sub,
        mul,
        div,

        // decls
        // a: extra_data index → DeclInfo
        decl_const,
        decl_var,
    };
};
