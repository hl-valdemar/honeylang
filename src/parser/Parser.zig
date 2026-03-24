const std = @import("std");
const mem = std.mem;

const Source = @import("../source/Source.zig");
const Lexer = @import("../lexer/Lexer.zig");
const Token = @import("../lexer/Token.zig");
const Ast = @import("Ast.zig");

tokens: Lexer.Tokens,
src: *const Source,
pos: Token.Index,
nodes: Ast.Nodes,
extra_data: Ast.Slots,
scratch: Ast.Slots,
errors: Ast.Errors,

const Self = @This();

pub fn init(tokens: Lexer.Tokens, src: *const Source) Self {
    return .{
        .tokens = tokens,
        .src = src,
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
    while (self.peekTag(0) != .eof) {
        self.skipNewlines();
        if (self.peekTag(0) == .eof) break;

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
    };
}

fn parseTopLevelDecl(self: *Self, alloc: mem.Allocator) !Ast.NodeIndex {
    if (self.tokens.tags[self.pos] == .identifier) {
        // accept `name ::` or `name =`
        const next = self.peekTag(1);
        if (next == .double_colon) {
            // check if `name :: func` or `name :: c func`
            if (self.peekTag(2) == .func or self.peekTag(3) == .func)
                return try self.parseFuncDecl(alloc);

            return try self.parseConstDecl(alloc);
        }
        if (next == .equal) return try self.parseVarDecl(alloc);
    }

    // error → skip token, produce error node
    return self.addError(alloc, .expected_declaration);
}

fn parseConstDecl(self: *Self, alloc: mem.Allocator) !Ast.NodeIndex {
    const name_token = self.pos;
    self.advanceN(2); // consume identifier and `::`

    const value = try self.parseExpr(alloc);
    try self.expect(alloc, .newline);

    return self.addNode(alloc, .{
        .tag = .const_decl,
        .main_token = name_token,
        .data = .{ .a = @intFromEnum(value) },
    });
}

fn parseVarDecl(self: *Self, alloc: mem.Allocator) !Ast.NodeIndex {
    const name_token = self.pos;
    self.advanceN(2); // consume identifier and `=`

    const value = try self.parseExpr(alloc);
    try self.expect(alloc, .newline);

    return self.addNode(alloc, .{
        .tag = .var_decl,
        .main_token = name_token,
        .data = .{ .a = @intFromEnum(value) },
    });
}

fn parseFuncDecl(self: *Self, alloc: mem.Allocator) !Ast.NodeIndex {
    const name_token = self.pos;
    self.advanceN(2); // consume identifier and `::`

    // optional calling convention
    var flags: u32 = 0;
    if (self.peekTag(0) == .cc_c) {
        flags |= @as(u32, @intFromEnum(Ast.FuncDecl.CallingConvention.c)) << Ast.FuncDecl.Flag.cc_shift;
        self.advance();
    }

    try self.expect(alloc, .func);
    try self.expect(alloc, .left_paren);

    // parse params
    const scratch_top = self.scratch.items.len;
    while (self.peekTag(0) != .right_paren and self.peekTag(0) != .eof) {
        const param = try self.parseParam(alloc);
        try self.scratch.append(alloc, @intFromEnum(param));
        if (self.peekTag(0) == .comma) self.advance();
    }

    try self.expect(alloc, .right_paren);

    // copy params to extra_data
    const params = self.scratch.items[scratch_top..];
    const params_start: Ast.ExtraIndex = @intCast(self.extra_data.items.len);
    try self.extra_data.appendSlice(alloc, params);
    const params_end: Ast.ExtraIndex = @intCast(self.extra_data.items.len);
    self.scratch.items.len = scratch_top;

    const return_type = try self.parseExpr(alloc);
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
        .main_token = name_token,
        .data = .{ .a = extra_idx },
    });
}

fn parseParam(self: *Self, alloc: mem.Allocator) !Ast.NodeIndex {
    const name_tok = self.pos;
    try self.expect(alloc, .identifier);

    const type_expr = try self.parseExpr(alloc);

    return self.addNode(alloc, .{
        .tag = .param,
        .main_token = name_tok,
        .data = .{ .a = @intFromEnum(type_expr) },
    });
}

fn parseBlock(self: *Self, alloc: mem.Allocator) !Ast.NodeIndex {
    const lbrace = self.pos;
    try self.expect(alloc, .left_curly);

    const scratch_top = self.scratch.items.len;
    while (self.peekTag(0) != .right_curly and self.peekTag(0) != .eof) {
        self.skipNewlines();
        if (self.peekTag(0) == .right_curly or self.peekTag(0) == .eof) break;

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

fn parseStatement(self: *Self, alloc: mem.Allocator) !Ast.NodeIndex {
    switch (self.peekTag(0)) {
        .@"return" => return self.parseReturnStatement(alloc),
        else => {
            const expr = try self.parseExpr(alloc);
            try self.expect(alloc, .newline);
            return expr;
        },
    }
}

fn parseReturnStatement(self: *Self, alloc: mem.Allocator) !Ast.NodeIndex {
    const tok = self.pos;
    self.advance(); // consume `return`

    // parse value unless immediately terminated
    const value: Ast.NodeIndex = if (self.peekTag(0) != .newline and
        self.peekTag(0) != .eof and
        self.peekTag(0) != .right_curly)
        try self.parseExpr(alloc)
    else
        .none;

    try self.expect(alloc, .newline);

    return self.addNode(alloc, .{
        .tag = .return_val,
        .main_token = tok,
        .data = .{ .a = @intFromEnum(value) },
    });
}

// todo: pratt parsing for expressions
fn parseExpr(self: *Self, alloc: mem.Allocator) !Ast.NodeIndex {
    return self.parsePrimary(alloc);
}

fn parsePrimary(self: *Self, alloc: mem.Allocator) !Ast.NodeIndex {
    switch (self.peekTag(0)) {
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
    while (self.peekTag(0) == .newline) self.advance();
}

fn expect(self: *Self, alloc: mem.Allocator, tag: Token.Tag) !void {
    if (self.peekTag(0) == tag) {
        self.advance();
    } else if (self.peekTag(0) != .eof) {
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

fn advanceN(self: *Self, offset: Token.Index) void {
    self.pos += offset;
}

fn peekTag(self: *const Self, offset: Token.Index) Token.Tag {
    // note: we should never reach a state where the index is out of bounds.
    // in case we do, just crash the program.
    return self.tokens.tags[self.pos + offset];
}

fn addExtraData(self: *Self, alloc: mem.Allocator, comptime T: type, data: T) !Ast.ExtraIndex {
    const start: Ast.ExtraIndex = @intCast(self.extra_data.items.len);
    const fields = @typeInfo(T).@"struct".fields;
    inline for (fields) |field| {
        const val = @field(data, field.name);
        try self.extra_data.append(alloc, switch (field.type) {
            Ast.Slot => val,
            Ast.NodeIndex => @intFromEnum(val),
            else => @compileError("unsupported extra_data field type"),
        });
    }
    return start;
}

fn addNode(self: *Self, alloc: mem.Allocator, node: Ast.Node) !Ast.NodeIndex {
    const idx: u32 = @intCast(self.nodes.len);
    try self.nodes.append(alloc, node);
    return @enumFromInt(idx);
}

fn addError(self: *Self, alloc: mem.Allocator, tag: Ast.Error.Tag) !Ast.NodeIndex {
    try self.errors.append(alloc, .{ .tag = tag, .token = self.pos });
    self.advance(); // skip offending token

    return self.addNode(alloc, .{
        .tag = .@"error",
        .main_token = self.pos -| 1,
        .data = .{},
    });
}

test "parse const decl" {
    const alloc = std.testing.allocator;

    const src_str =
        \\pi :: 3.14
        \\
    ;

    var src = try Source.init.fromStr(alloc, src_str);
    defer src.deinit(alloc);

    var lexer = Lexer.init(&src);
    defer lexer.deinit(alloc);

    const tokens = try lexer.scan(alloc);

    var parser = Self.init(tokens, &src);
    defer parser.deinit(alloc);

    const ast = try parser.parse(alloc);
    const rendered_str = try ast.render(alloc, src.contents);
    defer alloc.free(rendered_str);

    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "parse func decl" {
    const alloc = std.testing.allocator;

    const src_str =
        \\main :: func() int {
        \\    return 0xff
        \\}
        \\
    ;

    var src = try Source.init.fromStr(alloc, src_str);
    defer src.deinit(alloc);

    var lexer = Lexer.init(&src);
    defer lexer.deinit(alloc);

    const tokens = try lexer.scan(alloc);

    var parser = Self.init(tokens, &src);
    defer parser.deinit(alloc);

    const ast = try parser.parse(alloc);
    const rendered_str = try ast.render(alloc, src.contents);
    defer alloc.free(rendered_str);

    try std.testing.expectEqualStrings(src_str, rendered_str);
}
