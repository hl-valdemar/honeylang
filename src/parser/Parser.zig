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
        if (next == .double_colon) return try self.parseConstDecl(alloc);
        if (next == .equal) return try self.parseVarDecl(alloc);
    }

    // error → skip token, produce error node
    return self.addError(alloc, .expected_declaration);
}

fn parseConstDecl(self: *Self, alloc: mem.Allocator) !Ast.NodeIndex {
    const name_token = self.pos;
    self.advanceN(2); // consume identifier and `::`

    const value = try self.parseExpr(alloc);
    try self.expectNewline(alloc);

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
    try self.expectNewline(alloc);

    return self.addNode(alloc, .{
        .tag = .var_decl,
        .main_token = name_token,
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

fn expectNewline(self: *Self, alloc: mem.Allocator) !void {
    switch (self.peekTag(0)) {
        .newline => self.advance(),
        .eof => {},
        else => {
            try self.errors.append(alloc, .{
                .tag = .expected_newline,
                .token = self.pos,
            });
        },
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
