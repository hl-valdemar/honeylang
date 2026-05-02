const std = @import("std");
const mem = std.mem;

src: *const Source,
tokens: Lexer.Tokens.Slice,
diagnostics: *Diagnostic,
diagnostic_alloc: mem.Allocator,
pos: Token.Index,
nodes: AST.Nodes,
funcs: AST.Funcs,
branches: AST.Branches,
ref_lists: AST.Refs,
scratch: AST.Refs,
errors: AST.Errors,

const Self = @This();

const Payload = @import("../root.zig").Payload;
const StringPool = @import("../util/StringPool.zig");
const Source = @import("../source/Source.zig");
const Diagnostic = @import("../diagnostic/Store.zig");
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
    diagnostic_alloc: mem.Allocator,
    diagnostics: *Diagnostic,
};

pub fn init(ctx: Context) Self {
    return .{
        .src = ctx.src,
        .tokens = ctx.tokens,
        .diagnostics = ctx.diagnostics,
        .diagnostic_alloc = ctx.diagnostic_alloc,
        .pos = 0,
        .nodes = .{},
        .funcs = .{},
        .branches = .{},
        .ref_lists = .empty,
        .scratch = .empty,
        .errors = .{},
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.nodes.deinit(alloc);
    self.funcs.deinit(alloc);
    self.branches.deinit(alloc);
    self.ref_lists.deinit(alloc);
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

    const decls = self.scratch.items[scratch_top..];
    const refs = try self.appendRefList(alloc, decls);
    self.scratch.items.len = scratch_top;

    // patch root data
    self.nodes.items(.data)[0] = .{ .a = refs.start, .b = refs.end };

    return AST{
        .nodes = self.nodes.slice(),
        .funcs = self.funcs.slice(),
        .branches = self.branches.slice(),
        .ref_lists = self.ref_lists.items,
        .errors = self.errors.slice(),
        .tokens = self.tokens,
    };
}

fn parseTopLevelDecl(self: *Self, alloc: mem.Allocator) !AST.Node.Ref {
    if (self.tokenTag(self.pos) == .import) return self.parseImportDecl(alloc, null);
    if (self.tokenTag(self.pos) != .identifier) return self.addError(alloc, .expected_declaration);

    const name_tok = self.pos;
    self.advance();

    switch (self.tokenTag(self.pos)) {
        .double_colon => {
            if (self.tokenTag(self.pos + 1) == .import) return try self.parseImportDecl(alloc, name_tok);
            return try self.parseConstOrFunc(alloc, name_tok, .none);
        },
        .equal => return try self.parseVarDecl(alloc, name_tok, .none),
        .left_curly => return try self.parseNamespace(alloc, name_tok),
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

fn parseImportDecl(self: *Self, alloc: mem.Allocator, name_tok: ?Token.Index) !AST.Node.Ref {
    const explicit = name_tok != null;
    const main_tok: Token.Index = if (name_tok) |tok| tok else self.pos;

    if (explicit) try self.expect(alloc, .double_colon);
    try self.expect(alloc, .import);
    const path = try self.parsePrimary(alloc);
    try self.expect(alloc, .newline);

    return self.addNode(alloc, .{
        .tag = .import_decl,
        .main_tok = main_tok,
        .data = .{
            .a = path,
            .b = Payload.fromIndex(if (explicit) 1 else 0),
        },
    });
}

fn parseNamespace(self: *Self, alloc: mem.Allocator, name_tok: Token.Index) mem.Allocator.Error!AST.Node.Ref {
    const block = try self.parseDeclBlock(alloc);
    return self.addNode(alloc, .{
        .tag = .namespace_decl,
        .main_tok = name_tok,
        .data = .{ .a = block },
    });
}

fn parseConstOrFunc(self: *Self, alloc: mem.Allocator, name_tok: Token.Index, type_expr: AST.Node.Ref) !AST.Node.Ref {
    if (self.tokenTag(self.pos + 1) == .func or self.tokenTag(self.pos + 1) == .cc_c)
        return try self.parseFuncDecl(alloc, name_tok);
    return try self.parseConstDecl(alloc, name_tok, type_expr);
}

fn parseConstDecl(self: *Self, alloc: mem.Allocator, name_tok: Token.Index, type_expr: AST.Node.Ref) !AST.Node.Ref {
    try self.expect(alloc, .double_colon);
    const value = try self.parseExpr(alloc, 0);
    try self.expect(alloc, .newline);

    return self.addNode(alloc, .{
        .tag = .const_decl,
        .main_tok = name_tok,
        .data = .{ .a = type_expr, .b = value },
    });
}

fn parseVarDecl(self: *Self, alloc: mem.Allocator, name_tok: Token.Index, type_expr: AST.Node.Ref) !AST.Node.Ref {
    try self.expect(alloc, .equal);
    const value = try self.parseExpr(alloc, 0);
    try self.expect(alloc, .newline);

    return self.addNode(alloc, .{
        .tag = .var_decl,
        .main_tok = name_tok,
        .data = .{ .a = type_expr, .b = value },
    });
}

fn parseFuncDecl(self: *Self, alloc: mem.Allocator, name_tok: Token.Index) !AST.Node.Ref {
    try self.expect(alloc, .double_colon);

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

    const params = self.scratch.items[scratch_top..];
    const param_refs = try self.appendRefList(alloc, params);
    self.scratch.items.len = scratch_top;

    const ret_type = try self.parseExpr(alloc, 0);
    const body: AST.Node.Ref = if (self.tokenTag(self.pos) == .left_curly)
        try self.parseBlock(alloc)
    else body: {
        try self.expect(alloc, .newline);
        break :body .none;
    };

    const func_ref = try self.emitFuncInfo(alloc, .{
        .params_start = param_refs.start,
        .params_end = param_refs.end,
        .ret_type = ret_type,
        .body = body,
        .flags = flags,
    });

    return self.addNode(alloc, .{
        .tag = .func_decl,
        .main_tok = name_tok,
        .data = .{ .b = AST.asPayload(func_ref) },
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
    const refs = try self.appendRefList(alloc, stmts);
    self.scratch.items.len = scratch_top;

    return self.addNode(alloc, .{
        .tag = .block,
        .main_tok = lbrace,
        .data = .{ .a = refs.start, .b = refs.end },
    });
}

fn parseDeclBlock(self: *Self, alloc: mem.Allocator) !AST.Node.Ref {
    const lbrace = self.pos;
    try self.expect(alloc, .left_curly);

    const scratch_top = self.scratch.items.len;
    while (self.tokenTag(self.pos) != .right_curly and self.tokenTag(self.pos) != .eof) {
        self.skipNewlines();
        if (self.tokenTag(self.pos) == .right_curly or self.tokenTag(self.pos) == .eof) break;

        const decl = try self.parseTopLevelDecl(alloc);
        try self.scratch.append(alloc, decl);
    }

    try self.expect(alloc, .right_curly);

    const stmts = self.scratch.items[scratch_top..];
    const refs = try self.appendRefList(alloc, stmts);
    self.scratch.items.len = scratch_top;

    return self.addNode(alloc, .{
        .tag = .block,
        .main_tok = lbrace,
        .data = .{ .a = refs.start, .b = refs.end },
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
    try self.expect(alloc, .@"if");

    const condition = try self.parseExpr(alloc, 0); // note: optional parentheses
    const body = try self.parseBlock(alloc);

    if (self.tokenTag(self.pos) == .@"else") {
        self.advance(); // skip `else`

        // parse `else if` recursively
        const next_if = if (self.tokenTag(self.pos) == .@"if")
            try self.parseIfStatement(alloc)
        else
            try self.parseBlock(alloc);

        const branch_ref = try self.emitBranchInfo(alloc, .{
            .body = body,
            .else_node = next_if,
        });

        return self.addNode(alloc, .{
            .tag = .if_else,
            .main_tok = tok,
            .data = .{ .a = condition, .b = AST.asPayload(branch_ref) },
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
    try self.expect(alloc, .@"return");

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
    var left: AST.Node.Ref = blk: switch (self.tokenTag(self.pos)) {
        .int => {
            const tok = self.pos;
            self.advance(); // skip int
            break :blk try self.addNode(alloc, .{
                .tag = .int_literal,
                .main_tok = tok,
                .data = .{},
            });
        },
        .float => {
            const tok = self.pos;
            self.advance(); // skip float
            break :blk try self.addNode(alloc, .{
                .tag = .float_literal,
                .main_tok = tok,
                .data = .{},
            });
        },
        .string => {
            const tok = self.pos;
            self.advance(); // skip string
            break :blk try self.addNode(alloc, .{
                .tag = .string_literal,
                .main_tok = tok,
                .data = .{},
            });
        },
        .identifier => {
            const tok = self.pos;
            self.advance(); // skip identifier
            break :blk try self.addNode(alloc, .{
                .tag = .identifier,
                .main_tok = tok,
                .data = .{},
            });
        },
        .minus, .bang => {
            const tok = self.pos;
            self.advance(); // skip '-'
            const expr = try self.parseExpr(alloc, BindingPower.prefix_bp);
            break :blk try self.addNode(alloc, .{
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
            break :blk try self.addNode(alloc, .{
                .tag = .grouped_expr,
                .main_tok = tok,
                .data = .{ .a = expr },
            });
        },
        else => break :blk try self.addError(alloc, .expected_expression),
    };

    while (self.tokenTag(self.pos) == .dot) {
        const dot_tok = self.pos;
        self.advance();

        const right_tok = self.pos;
        try self.expect(alloc, .identifier);
        const right = try self.addNode(alloc, .{
            .tag = .identifier,
            .main_tok = right_tok,
            .data = .{},
        });
        left = try self.addNode(alloc, .{
            .tag = .qualified_ref,
            .main_tok = dot_tok,
            .data = .{ .a = left, .b = right },
        });
    }

    return left;
}

fn skipNewlines(self: *Self) void {
    while (self.tokenTag(self.pos) == .newline) self.advance();
}

/// advance through matching tag or fail.
fn expect(self: *Self, alloc: mem.Allocator, tag: Token.Tag) !void {
    if (self.tokenTag(self.pos) == tag) {
        self.advance();
    } else {
        try self.errors.append(alloc, .{
            .tag = .expected_token,
            .token = self.pos,
            .expected = tag,
        });
        _ = try self.addDiagnostic(.parser_expected_token, self.pos);
    }
}

fn advance(self: *Self) void {
    if (self.tokenTag(self.pos) != .eof) self.pos += 1;
}

fn advanceN(self: *Self, offset: Token.Index) void {
    self.pos += offset;
}

/// get the tag for a token, treating out-of-bounds recovery lookahead as eof.
fn tokenTag(self: *const Self, pos: Token.Index) Token.Tag {
    if (pos >= self.tokens.len) return .eof;
    return self.tokens.items(.tag)[pos];
}

fn addNode(self: *Self, alloc: mem.Allocator, node: AST.Node) !AST.Node.Ref {
    try self.nodes.append(alloc, node);
    return Payload.fromIndex(self.nodes.len - 1);
}

fn emitFuncInfo(self: *Self, alloc: mem.Allocator, info: AST.FuncDecl) !AST.FuncRef {
    const ref: AST.FuncRef = Payload.fromIndex(self.funcs.len).to(AST.FuncRef);
    try self.funcs.append(alloc, info);
    return ref;
}

fn emitBranchInfo(self: *Self, alloc: mem.Allocator, info: AST.ElseIfInfo) !AST.BranchRef {
    const ref: AST.BranchRef = Payload.fromIndex(self.branches.len).to(AST.BranchRef);
    try self.branches.append(alloc, info);
    return ref;
}

fn appendRefList(self: *Self, alloc: mem.Allocator, refs: []const Payload) !struct { start: Payload, end: Payload } {
    const start = Payload.fromIndex(self.ref_lists.items.len);
    try self.ref_lists.appendSlice(alloc, refs);
    const end = Payload.fromIndex(self.ref_lists.items.len);
    return .{ .start = start, .end = end };
}

fn addError(self: *Self, alloc: mem.Allocator, tag: AST.Error.Tag) !AST.Node.Ref {
    try self.errors.append(alloc, .{ .tag = tag, .token = self.pos });
    const err_tok = self.pos;
    const diag_ref = try self.addDiagnostic(switch (tag) {
        .expected_expression => .parser_expected_expression,
        .expected_declaration => .parser_expected_declaration,
        .expected_token => .parser_expected_token,
    }, err_tok);
    self.advance(); // skip offending token unless already at eof

    return self.addNode(alloc, .{
        .tag = .@"error",
        .main_tok = err_tok,
        .data = .{ .a = Payload.from(diag_ref) },
    });
}

fn addDiagnostic(self: *Self, tag: Diagnostic.Tag, token: Token.Index) !Diagnostic.Ref {
    return self.diagnostics.add(self.diagnostic_alloc, .{
        .stage = .parser,
        .severity = .err,
        .tag = tag,
        .span = self.tokenSpan(token),
    });
}

fn tokenSpan(self: *const Self, token: Token.Index) ?Diagnostic.Span {
    if (token >= self.tokens.len) return null;
    const start = self.tokens.items(.start)[token];
    var end = start;
    if (token + 1 < self.tokens.len) {
        end = self.tokens.items(.start)[token + 1];
    } else {
        end = @intCast(self.src.contents.len);
    }
    return .{ .start = start, .end = end };
}

pub fn lower(alloc: mem.Allocator, ast: *const AST, str_pool: *const StringPool, diagnostics: *Diagnostic, diagnostic_alloc: mem.Allocator) !HIR {
    var hir = HIR.init(.{ .ast = ast, .str_pool = str_pool, .diagnostics = diagnostics, .diagnostic_alloc = diagnostic_alloc });
    const root = Payload.fromIndex(0);
    _ = try hir.lower(alloc, root);
    return hir;
}
