const std = @import("std");
const mem = std.mem;

src: *const Source,
tokens: Lexer.Tokens.Slice,
pos: Token.Ref,
nodes: AST.Nodes,
extra_data: AST.Refs,
scratch: AST.Refs,
errors: AST.Errors,

const Self = @This();

const BaseRef = @import("../root.zig").BaseRef;
const StringPool = @import("../util/StringPool.zig");
const Source = @import("../source/Source.zig");
const Lexer = @import("../lexer/Lexer.zig");
const Token = @import("../lexer/Token.zig");
const AST = @import("AST.zig");
const HIR = @import("HIR.zig");

const BindingPower = struct {
    left: Type,
    right: Type,

    const Type = u32;

    /// used for unary operators.
    /// should bind tighter than any other operators.
    const prefix_bp = 14;

    fn from(tag: Token.Tag) ?BindingPower {
        return switch (tag) {
            .star, .slash => .{ .left = 12, .right = 13 },
            .plus, .minus => .{ .left = 10, .right = 11 },
            else => null, // terminate expression
        };
    }
};

pub const Context = struct {
    src: *const Source,
    tokens: Lexer.Tokens.Slice,
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

pub fn parse(self: *Self, alloc: mem.Allocator) !AST {
    // reserve the root node
    try self.nodes.append(alloc, .{
        .tag = .root,
        .main_tok = 0,
        .data = .{},
    });

    // parse top-level decls
    const scratch_top = self.scratch.items.len;
    while (self.tokenTag(self.pos) != .eof) {
        self.skipNewlines();
        if (self.tokenTag(self.pos) == .eof) break;

        const decl = try self.parseTopLevelDecl(alloc);
        try self.scratch.append(alloc, decl);
    }

    // copy to extra-data
    const decls = self.scratch.items[scratch_top..];
    const extra_start: BaseRef = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
    try self.extra_data.appendSlice(alloc, decls);
    const extra_end: BaseRef = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
    self.scratch.items.len = scratch_top; // reset scratch pointer

    // patch root data
    self.nodes.items(.data)[0] = .{ .a = extra_start, .b = extra_end };

    return AST{
        .nodes = self.nodes.slice(),
        .extra_data = self.extra_data.items,
        .errors = self.errors.slice(),
        .tokens = self.tokens,
    };
}

fn parseTopLevelDecl(self: *Self, alloc: mem.Allocator) !AST.Node.Ref {
    if (self.tokenTag(self.pos) != .identifier) return self.addError(alloc, .expected_declaration);

    const name_tok = self.pos;
    self.advance();

    switch (self.tokenTag(self.pos)) {
        .double_colon => return try self.parseConstOrFunc(alloc, name_tok, .none),
        .equal => return try self.parseVarDecl(alloc, name_tok, .none),
        else => {
            // type annotation → parse `name type :: value` or `name type = value`
            const type_expr = try self.parseExpr(alloc, 0);
            return switch (self.tokenTag(self.pos)) {
                .double_colon => try self.parseConstOrFunc(alloc, name_tok, type_expr),
                .equal => try self.parseVarDecl(alloc, name_tok, type_expr),
                else => self.addError(alloc, .expected_declaration),
            };
        },
    }
}

fn parseConstOrFunc(self: *Self, alloc: mem.Allocator, name_tok: Token.Ref, type_expr: AST.Node.Ref) !AST.Node.Ref {
    if (self.tokenTag(self.pos + 1) == .func or self.tokenTag(self.pos + 1) == .cc_c)
        return try self.parseFuncDecl(alloc, name_tok);
    return try self.parseConstDecl(alloc, name_tok, type_expr);
}

fn parseConstDecl(self: *Self, alloc: mem.Allocator, name_tok: Token.Ref, type_expr: AST.Node.Ref) !AST.Node.Ref {
    self.advance(); // skip `::`
    const value = try self.parseExpr(alloc, 0);
    try self.expect(alloc, .newline);

    return self.addNode(alloc, .{
        .tag = .const_decl,
        .main_tok = name_tok,
        .data = .{ .a = type_expr, .b = value },
    });
}

fn parseVarDecl(self: *Self, alloc: mem.Allocator, name_tok: Token.Ref, type_expr: AST.Node.Ref) !AST.Node.Ref {
    self.advance(); // skip `=`
    const value = try self.parseExpr(alloc, 0);
    try self.expect(alloc, .newline);

    return self.addNode(alloc, .{
        .tag = .var_decl,
        .main_tok = name_tok,
        .data = .{ .a = type_expr, .b = value },
    });
}

fn parseFuncDecl(self: *Self, alloc: mem.Allocator, name_tok: Token.Ref) !AST.Node.Ref {
    self.advance(); // skip `::`

    // optional calling convention
    var flags: u32 = 0;
    if (self.tokenTag(self.pos) == .cc_c) {
        flags |= @as(u32, @intFromEnum(AST.FuncDecl.CallingConvention.c)) << AST.FuncDecl.Flag.cc_shift;
        self.advance();
    }

    try self.expect(alloc, .func);
    try self.expect(alloc, .left_paren);

    // parse params
    const scratch_top = self.scratch.items.len;
    while (self.tokenTag(self.pos) != .right_paren and self.tokenTag(self.pos) != .eof) {
        const param = try self.parseParam(alloc);
        try self.scratch.append(alloc, param);
        if (self.tokenTag(self.pos) == .comma) self.advance();
    }

    try self.expect(alloc, .right_paren);

    // copy params to extra_data
    const params = self.scratch.items[scratch_top..];
    const params_start: BaseRef = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
    try self.extra_data.appendSlice(alloc, params);
    const params_end: BaseRef = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
    self.scratch.items.len = scratch_top;

    const ret_type = try self.parseExpr(alloc, 0);
    const body = try self.parseBlock(alloc);

    // pack func decl into extra_data
    const extra_idx = try self.packExtraData(alloc, AST.FuncDecl, .{
        .params_start = params_start,
        .params_end = params_end,
        .ret_type = ret_type,
        .body = body,
        .flags = flags,
    });

    return self.addNode(alloc, .{
        .tag = .func_decl,
        .main_tok = name_tok,
        .data = .{ .a = extra_idx },
    });
}

fn parseParam(self: *Self, alloc: mem.Allocator) !AST.Node.Ref {
    const name_tok = self.pos;
    try self.expect(alloc, .identifier);

    const type_expr = try self.parseExpr(alloc, 0);

    return self.addNode(alloc, .{
        .tag = .param,
        .main_tok = name_tok,
        .data = .{ .a = type_expr },
    });
}

fn parseBlock(self: *Self, alloc: mem.Allocator) mem.Allocator.Error!AST.Node.Ref {
    const lbrace = self.pos;
    try self.expect(alloc, .left_curly);

    const scratch_top = self.scratch.items.len;
    while (self.tokenTag(self.pos) != .right_curly and self.tokenTag(self.pos) != .eof) {
        self.skipNewlines();
        if (self.tokenTag(self.pos) == .right_curly or self.tokenTag(self.pos) == .eof) break;

        const stmt = try self.parseStatement(alloc);
        try self.scratch.append(alloc, stmt);
    }

    try self.expect(alloc, .right_curly);

    const stmts = self.scratch.items[scratch_top..];
    const start: BaseRef = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
    try self.extra_data.appendSlice(alloc, stmts);
    const end: BaseRef = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
    self.scratch.items.len = scratch_top;

    return self.addNode(alloc, .{
        .tag = .block,
        .main_tok = lbrace,
        .data = .{ .a = start, .b = end },
    });
}

fn parseStatement(self: *Self, alloc: mem.Allocator) !AST.Node.Ref {
    switch (self.tokenTag(self.pos)) {
        .identifier => {
            const next = self.tokenTag(self.pos + 1);

            if (next == .double_colon or next == .equal) {
                // untyped decl: `name :: value` or `name = value`
                const name_tok = self.pos;
                self.advance();
                return switch (self.tokenTag(self.pos)) {
                    .double_colon => try self.parseConstDecl(alloc, name_tok, .none),
                    .equal => try self.parseVarDecl(alloc, name_tok, .none),
                    else => unreachable,
                };
            }

            if (next == .identifier) {
                // typed decl: `name type :: value` or `name type = value`
                const name_tok = self.pos;
                self.advance();
                const type_expr = try self.parseExpr(alloc, 0);
                return switch (self.tokenTag(self.pos)) {
                    .double_colon => try self.parseConstOrFunc(alloc, name_tok, type_expr),
                    .equal => try self.parseVarDecl(alloc, name_tok, type_expr),
                    else => self.addError(alloc, .expected_declaration),
                };
            }

            return self.parseExprStatement(alloc);
        },
        .@"if" => return self.parseIfStatement(alloc),
        .@"return" => return self.parseReturnStatement(alloc),
        else => return self.parseExprStatement(alloc),
    }
}

fn parseExprStatement(self: *Self, alloc: mem.Allocator) !AST.Node.Ref {
    const tok = self.pos;
    const expr = try self.parseExpr(alloc, 0);
    try self.expect(alloc, .newline);
    return self.addNode(alloc, .{
        .tag = .expr_statement,
        .main_tok = tok,
        .data = .{ .a = expr },
    });
}

fn parseIfStatement(self: *Self, alloc: mem.Allocator) !AST.Node.Ref {
    const tok = self.pos;
    self.advance(); // skip `if`

    const condition = try self.parseExpr(alloc, 0); // note: optional parentheses
    const body = try self.parseBlock(alloc);

    if (self.tokenTag(self.pos) == .@"else") {
        self.advance(); // skip `else`

        // parse `else if` recursively
        const next_if = if (self.tokenTag(self.pos) == .@"if")
            try self.parseIfStatement(alloc)
        else
            try self.parseBlock(alloc);

        const extra_data_idx = try self.packExtraData(alloc, AST.ElseIfInfo, .{
            .body = body,
            .else_node = next_if,
        });

        return self.addNode(alloc, .{
            .tag = .if_else,
            .main_tok = tok,
            .data = .{ .a = condition, .b = extra_data_idx },
        });
    }

    return self.addNode(alloc, .{
        .tag = .if_simple,
        .main_tok = tok,
        .data = .{ .a = condition, .b = body },
    });
}

fn parseReturnStatement(self: *Self, alloc: mem.Allocator) !AST.Node.Ref {
    const tok = self.pos;
    self.advance(); // skip `return`

    // parse value unless immediately terminated
    const value: AST.Node.Ref = if (self.tokenTag(self.pos) != .newline and
        self.tokenTag(self.pos) != .eof and
        self.tokenTag(self.pos) != .right_curly)
        try self.parseExpr(alloc, 0)
    else
        .none;

    try self.expect(alloc, .newline);

    return self.addNode(alloc, .{
        .tag = .return_val,
        .main_tok = tok,
        .data = .{ .a = value },
    });
}

/// parse an expression (pratt parsing).
fn parseExpr(self: *Self, alloc: mem.Allocator, min_bp: BindingPower.Type) mem.Allocator.Error!AST.Node.Ref {
    var left = try self.parsePrimary(alloc);

    while (true) {
        const op_pos = self.pos;
        const op = self.tokenTag(self.pos);

        const bp = BindingPower.from(op) orelse break;
        if (bp.left < min_bp) break;

        self.advance();
        const right = try self.parseExpr(alloc, bp.right);
        left = try self.addNode(alloc, .{
            .tag = .binary_op,
            .main_tok = op_pos,
            .data = .{
                .a = left,
                .b = right,
            },
        });
    }

    return left;
}

fn parsePrimary(self: *Self, alloc: mem.Allocator) !AST.Node.Ref {
    switch (self.tokenTag(self.pos)) {
        .int => {
            const tok = self.pos;
            self.advance(); // skip int
            return self.addNode(alloc, .{
                .tag = .int_literal,
                .main_tok = tok,
                .data = .{},
            });
        },
        .float => {
            const tok = self.pos;
            self.advance(); // skip float
            return self.addNode(alloc, .{
                .tag = .float_literal,
                .main_tok = tok,
                .data = .{},
            });
        },
        .identifier => {
            const tok = self.pos;
            self.advance(); // skip identifier
            return self.addNode(alloc, .{
                .tag = .identifier,
                .main_tok = tok,
                .data = .{},
            });
        },
        .minus, .bang => {
            const tok = self.pos;
            self.advance(); // skip '-'
            const expr = try self.parseExpr(alloc, BindingPower.prefix_bp);
            return self.addNode(alloc, .{
                .tag = .unary_op,
                .main_tok = tok,
                .data = .{ .a = expr },
            });
        },
        .left_paren => {
            const tok = self.pos;
            self.advance(); // skip `(`
            const expr = try self.parseExpr(alloc, 0);
            try self.expect(alloc, .right_paren);
            return self.addNode(alloc, .{
                .tag = .grouped_expr,
                .main_tok = tok,
                .data = .{ .a = expr },
            });
        },
        else => return self.addError(alloc, .expected_expression),
    }
}

fn skipNewlines(self: *Self) void {
    while (self.tokenTag(self.pos) == .newline) self.advance();
}

fn expect(self: *Self, alloc: mem.Allocator, tag: Token.Tag) !void {
    if (self.tokenTag(self.pos) == tag) {
        self.advance();
    } else if (self.tokenTag(self.pos) != .eof) {
        try self.errors.append(alloc, .{
            .tag = .expected_token,
            .token = self.pos,
            .expected = tag,
        });
    }
}

fn advance(self: *Self) void {
    self.pos += 1;
}

fn advanceN(self: *Self, offset: Token.Ref) void {
    self.pos += offset;
}

/// get the tag for a token at the given position.
/// pre: position is in bounds of the token arrays.
fn tokenTag(self: *const Self, pos: Token.Ref) Token.Tag {
    // note: we should never reach a state where the index is out of bounds.
    // in case we do, just crash the program.
    return self.tokens.items(.tag)[pos];
}

fn packExtraData(self: *Self, alloc: mem.Allocator, comptime T: type, data: T) !BaseRef {
    const start: BaseRef = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
    const fields = @typeInfo(T).@"struct".fields;
    inline for (fields) |field| {
        const val = @field(data, field.name);
        try self.extra_data.append(alloc, switch (field.type) {
            BaseRef => val,
            u32 => @enumFromInt(val),
            else => @compileError("unsupported extra_data field type"),
        });
    }
    return start;
}

fn addNode(self: *Self, alloc: mem.Allocator, node: AST.Node) !AST.Node.Ref {
    const idx: u32 = @intCast(self.nodes.len);
    try self.nodes.append(alloc, node);
    return @enumFromInt(idx);
}

fn addError(self: *Self, alloc: mem.Allocator, tag: AST.Error.Tag) !AST.Node.Ref {
    try self.errors.append(alloc, .{ .tag = tag, .token = self.pos });
    self.advance(); // skip offending token

    return self.addNode(alloc, .{
        .tag = .@"error",
        .main_tok = self.pos -| 1,
        .data = .{},
    });
}

pub fn lower(alloc: mem.Allocator, ast: *const AST, str_pool: *const StringPool) !HIR {
    var hir = HIR.init(.{ .ast = ast, .str_pool = str_pool });
    const root: HIR.Inst.Ref = @enumFromInt(0);
    _ = try hir.lower(alloc, root);
    return hir;
}
