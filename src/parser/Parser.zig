const std = @import("std");
const mem = std.mem;

src: *const Source,
tokens: Lexer.ScanResult,
pos: Token.Idx,
nodes: Ast.Nodes,
extra_data: Ast.Slots,
scratch: Ast.Slots,
errors: Ast.Errors,

const Self = @This();

const Source = @import("../source/Source.zig");
const Lexer = @import("../lexer/Lexer.zig");
const Token = @import("../lexer/Token.zig");
const Ast = @import("Ast.zig");

const BindingPower = struct {
    left: Type,
    right: Type,

    const Type = u32;

    fn from(t: Token.Tag) ?BindingPower {
        return switch (t) {
            .mul, .div => .{ .left = 12, .right = 13 },
            .add, .sub => .{ .left = 10, .right = 11 },
            else => null, // terminate expression
        };
    }
};

pub const Context = struct {
    src: *const Source,
    tokens: Lexer.ScanResult,
};

pub fn init(ctx: Context) Self {
    return .{
        .src = ctx.src,
        .tokens = ctx.tokens,
        .pos = 0,
        .nodes = .{},
        .extra_data = .{},
        .scratch = .{},
        .errors = .{},
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.nodes.deinit(alloc);
    self.extra_data.deinit(alloc);
    self.scratch.deinit(alloc);
    self.errors.deinit(alloc);
}

pub fn parse(self: *Self, alloc: mem.Allocator) !Ast {
    // reserve the root node
    try self.nodes.append(alloc, .{
        .tag = .root,
        .main_token = 0,
        .data = .{},
    });

    // parse top-level decls
    const scratch_top = self.scratch.items.len;
    while (self.tag(self.pos) != .eof) {
        self.skipNewlines();
        if (self.tag(self.pos) == .eof) break;

        const decl = try self.parseTopLevelDecl(alloc);
        try self.scratch.append(alloc, @intFromEnum(decl));
    }

    // copy to extra-data
    const decls = self.scratch.items[scratch_top..];
    const extra_start: Ast.Slot = @intCast(self.extra_data.items.len);
    try self.extra_data.appendSlice(alloc, decls);
    const extra_end: Ast.Slot = @intCast(self.extra_data.items.len);
    self.scratch.items.len = scratch_top; // reset scratch pointer

    // patch root data
    self.nodes.items(.data)[0] = .{ .a = extra_start, .b = extra_end };

    return Ast{
        .nodes = self.nodes.slice(),
        .extra_data = self.extra_data.items,
        .errors = self.errors.slice(),
        .token_tags = self.tokens.tags,
        .token_starts = self.tokens.starts,
        .token_str_ids = self.tokens.str_ids,
    };
}

fn parseTopLevelDecl(self: *Self, alloc: mem.Allocator) !Ast.NodeIdx {
    if (self.tag(self.pos) != .identifier) return self.addError(alloc, .expected_declaration);

    const name_tok = self.pos;
    self.advance();

    switch (self.tag(self.pos)) {
        .double_colon => return try self.parseConstOrFunc(alloc, name_tok, .none),
        .equal => return try self.parseVarDecl(alloc, name_tok, .none),
        else => {
            // type annotation → parse `name type :: value` or `name type = value`
            const type_expr = try self.parseExpr(alloc, 0);
            return switch (self.tag(self.pos)) {
                .double_colon => try self.parseConstOrFunc(alloc, name_tok, type_expr),
                .equal => try self.parseVarDecl(alloc, name_tok, type_expr),
                else => self.addError(alloc, .expected_declaration),
            };
        },
    }
}

fn parseConstOrFunc(self: *Self, alloc: mem.Allocator, name_tok: Token.Idx, type_expr: Ast.NodeIdx) !Ast.NodeIdx {
    self.advance(); // skip `::`
    if (self.tag(self.pos) == .func or self.tag(self.pos) == .cc_c)
        return try self.parseFuncDecl(alloc, name_tok);
    return try self.parseConstDecl(alloc, name_tok, type_expr);
}

fn parseConstDecl(self: *Self, alloc: mem.Allocator, name_tok: Token.Idx, type_expr: Ast.NodeIdx) !Ast.NodeIdx {
    const value = try self.parseExpr(alloc, 0);
    try self.expect(alloc, .newline);

    return self.addNode(alloc, .{
        .tag = .const_decl,
        .main_token = name_tok,
        .data = .{ .a = @intFromEnum(type_expr), .b = @intFromEnum(value) },
    });
}

fn parseVarDecl(self: *Self, alloc: mem.Allocator, name_tok: Token.Idx, type_expr: Ast.NodeIdx) !Ast.NodeIdx {
    self.advance(); // skip `=`
    const value = try self.parseExpr(alloc, 0);
    try self.expect(alloc, .newline);

    return self.addNode(alloc, .{
        .tag = .var_decl,
        .main_token = name_tok,
        .data = .{ .a = @intFromEnum(type_expr), .b = @intFromEnum(value) },
    });
}

fn parseFuncDecl(self: *Self, alloc: mem.Allocator, name_tok: Token.Idx) !Ast.NodeIdx {
    // optional calling convention
    var flags: u32 = 0;
    if (self.tag(self.pos) == .cc_c) {
        flags |= @as(u32, @intFromEnum(Ast.FuncDecl.CallingConvention.c)) << Ast.FuncDecl.Flag.cc_shift;
        self.advance();
    }

    try self.expect(alloc, .func);
    try self.expect(alloc, .left_paren);

    // parse params
    const scratch_top = self.scratch.items.len;
    while (self.tag(self.pos) != .right_paren and self.tag(self.pos) != .eof) {
        const param = try self.parseParam(alloc);
        try self.scratch.append(alloc, @intFromEnum(param));
        if (self.tag(self.pos) == .comma) self.advance();
    }

    try self.expect(alloc, .right_paren);

    // copy params to extra_data
    const params = self.scratch.items[scratch_top..];
    const params_start: Ast.ExtraIdx = @intCast(self.extra_data.items.len);
    try self.extra_data.appendSlice(alloc, params);
    const params_end: Ast.ExtraIdx = @intCast(self.extra_data.items.len);
    self.scratch.items.len = scratch_top;

    const return_type = try self.parseExpr(alloc, 0);
    const body = try self.parseBlock(alloc);

    // pack func decl into extra_data
    const extra_idx = try self.addExtraData(alloc, Ast.FuncDecl, .{
        .params_start = params_start,
        .params_end = params_end,
        .return_type = return_type,
        .body = body,
        .flags = flags,
    });

    return self.addNode(alloc, .{
        .tag = .func_decl,
        .main_token = name_tok,
        .data = .{ .a = extra_idx },
    });
}

fn parseParam(self: *Self, alloc: mem.Allocator) !Ast.NodeIdx {
    const name_tok = self.pos;
    try self.expect(alloc, .identifier);

    const type_expr = try self.parseExpr(alloc, 0);

    return self.addNode(alloc, .{
        .tag = .param,
        .main_token = name_tok,
        .data = .{ .a = @intFromEnum(type_expr) },
    });
}

fn parseBlock(self: *Self, alloc: mem.Allocator) !Ast.NodeIdx {
    const lbrace = self.pos;
    try self.expect(alloc, .left_curly);

    const scratch_top = self.scratch.items.len;
    while (self.tag(self.pos) != .right_curly and self.tag(self.pos) != .eof) {
        self.skipNewlines();
        if (self.tag(self.pos) == .right_curly or self.tag(self.pos) == .eof) break;

        const stmt = try self.parseStatement(alloc);
        try self.scratch.append(alloc, @intFromEnum(stmt));
    }

    try self.expect(alloc, .right_curly);

    const stmts = self.scratch.items[scratch_top..];
    const start: Ast.Slot = @intCast(self.extra_data.items.len);
    try self.extra_data.appendSlice(alloc, stmts);
    const end: Ast.Slot = @intCast(self.extra_data.items.len);
    self.scratch.items.len = scratch_top;

    return self.addNode(alloc, .{
        .tag = .block,
        .main_token = lbrace,
        .data = .{ .a = start, .b = end },
    });
}

fn parseStatement(self: *Self, alloc: mem.Allocator) !Ast.NodeIdx {
    switch (self.tag(self.pos)) {
        .@"return" => return self.parseReturnStatement(alloc),
        else => {
            const expr = try self.parseExpr(alloc, 0);
            try self.expect(alloc, .newline);
            return expr;
        },
    }
}

fn parseReturnStatement(self: *Self, alloc: mem.Allocator) !Ast.NodeIdx {
    const tok = self.pos;
    self.advance(); // skip `return`

    // parse value unless immediately terminated
    const value: Ast.NodeIdx = if (self.tag(self.pos) != .newline and
        self.tag(self.pos) != .eof and
        self.tag(self.pos) != .right_curly)
        try self.parseExpr(alloc, 0)
    else
        .none;

    try self.expect(alloc, .newline);

    return self.addNode(alloc, .{
        .tag = .return_val,
        .main_token = tok,
        .data = .{ .a = @intFromEnum(value) },
    });
}

/// parse an expression (pratt parsing).
fn parseExpr(self: *Self, alloc: mem.Allocator, min_bp: BindingPower.Type) !Ast.NodeIdx {
    var left = try self.parsePrimary(alloc);

    while (true) {
        const op_pos = self.pos;
        const op = self.tag(self.pos);

        const bp = BindingPower.from(op) orelse break;
        if (bp.left < min_bp) break;

        self.advance();
        const right = try self.parseExpr(alloc, bp.right);
        left = try self.addNode(alloc, .{
            .tag = .binary_op,
            .main_token = op_pos,
            .data = .{
                .a = @intFromEnum(left),
                .b = @intFromEnum(right),
            },
        });
    }

    return left;
}

fn parsePrimary(self: *Self, alloc: mem.Allocator) !Ast.NodeIdx {
    switch (self.tag(self.pos)) {
        .number => {
            const tok = self.pos;
            self.advance();
            return self.addNode(alloc, .{
                .tag = .number_literal,
                .main_token = tok,
                .data = .{},
            });
        },
        .identifier => {
            const tok = self.pos;
            self.advance();
            return self.addNode(alloc, .{
                .tag = Ast.Node.Tag.identifier,
                .main_token = tok,
                .data = .{},
            });
        },
        else => return self.addError(alloc, .expected_expression),
    }
}

fn skipNewlines(self: *Self) void {
    while (self.tag(self.pos) == .newline) self.advance();
}

fn expect(self: *Self, alloc: mem.Allocator, t: Token.Tag) !void {
    if (self.tag(self.pos) == t) {
        self.advance();
    } else if (self.tag(self.pos) != .eof) {
        try self.errors.append(alloc, .{
            .tag = .expected_token,
            .token = self.pos,
            .expected = t,
        });
    }
}

fn advance(self: *Self) void {
    self.pos += 1;
}

fn advanceN(self: *Self, offset: Token.Idx) void {
    self.pos += offset;
}

/// get the tag for a token at the given position.
/// pre: position is in bounds of the token arrays.
fn tag(self: *const Self, pos: Token.Idx) Token.Tag {
    // note: we should never reach a state where the index is out of bounds.
    // in case we do, just crash the program.
    return self.tokens.tags[pos];
}

fn addExtraData(self: *Self, alloc: mem.Allocator, comptime T: type, data: T) !Ast.ExtraIdx {
    const start: Ast.ExtraIdx = @intCast(self.extra_data.items.len);
    const fields = @typeInfo(T).@"struct".fields;
    inline for (fields) |field| {
        const val = @field(data, field.name);
        try self.extra_data.append(alloc, switch (field.type) {
            Ast.Slot => val,
            Ast.NodeIdx => @intFromEnum(val),
            else => @compileError("unsupported extra_data field type"),
        });
    }
    return start;
}

fn addNode(self: *Self, alloc: mem.Allocator, node: Ast.Node) !Ast.NodeIdx {
    const idx: u32 = @intCast(self.nodes.len);
    try self.nodes.append(alloc, node);
    return @enumFromInt(idx);
}

fn addError(self: *Self, alloc: mem.Allocator, t: Ast.Error.Tag) !Ast.NodeIdx {
    try self.errors.append(alloc, .{ .tag = t, .token = self.pos });
    self.advance(); // skip offending token

    return self.addNode(alloc, .{
        .tag = .@"error",
        .main_token = self.pos -| 1,
        .data = .{},
    });
}
