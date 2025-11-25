const std = @import("std");
const mem = std.mem;

const Token = @import("../lexer/token.zig").Token;
const TokenKind = @import("../lexer/token.zig").Kind;
const TokenList = @import("../lexer/token.zig").TokenList;

const ast = @import("ast.zig");
const Ast = @import("ast.zig").Ast;
const NodeIndex = @import("ast.zig").NodeIndex;
const SourceIndex = @import("../source/source.zig").Index;

pub fn parse(allocator: mem.Allocator, tokens: TokenList) !Ast {
    var parser = try Parser.init(allocator, tokens);
    return parser.parse();
}

pub const ParseError = error{
    UnexpectedEof,
    UnexpectedToken,
    OutOfMemory,
};

pub const Parser = struct {
    tokens: []const Token,
    pos: u32,
    ast: Ast,
    allocator: mem.Allocator,

    pub fn init(allocator: mem.Allocator, tokens: TokenList) !Parser {
        return .{
            .tokens = tokens.items,
            .pos = 0,
            .ast = try Ast.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.ast.deinit();
    }

    pub fn parse(self: *Parser) ParseError!Ast {
        const decls = try self.parseProgram();

        // create root program node
        const root = try self.ast.addProgram(decls, 0, 0); // always at index 0
        self.ast.root = root;

        return self.ast;
    }

    fn parseProgram(self: *Parser) ParseError!ast.Range {
        var decls = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 10);
        defer decls.deinit(self.allocator);

        while (!self.check(.eof)) {
            const decl = try self.parseDeclaration();
            try decls.append(self.allocator, decl);
        }

        return try self.ast.addExtra(decls.items);
    }

    fn parseDeclaration(self: *Parser) !NodeIndex {
        // look ahead to determine declaration type
        const token = self.peek() orelse return error.UnexpectedEof;

        if (token.kind == .identifier) {
            const next = self.peekOffset(1) orelse return error.UnexpectedEof;

            if (next.kind == .double_colon) {
                // check if it's a function or const
                const after_colon = self.peekOffset(2) orelse return error.UnexpectedEof;

                if (after_colon.kind == .func) {
                    return try self.parseFuncDecl();
                } else {
                    return try self.parseConstDecl();
                }
            }
        }

        return error.UnexpectedToken;
    }

    fn parseConstDecl(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentPos();

        // parse name
        const name = try self.parseIdentifier();

        // parse optional type annotation
        const type_node = if (self.match(.colon))
            try self.parseIdentifier() // type is also an identifier
        else
            null;

        // expect ::
        try self.expect(.double_colon);

        // parse value expression
        const value = try self.parseExpression();

        const end_pos = self.currentPos();

        return try self.ast.addConstDecl(name, type_node, value, start_pos, end_pos);
    }

    fn parseFuncDecl(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentPos();

        // parse name
        const name = try self.parseIdentifier();

        // expect ::
        try self.expect(.double_colon);

        // expect 'func'
        try self.expect(.func);

        // expect (
        try self.expect(.left_paren);

        // parse parameters
        const params = try self.parseParameters();

        // expect )
        try self.expect(.right_paren);

        // parse return type
        const return_type = try self.parseIdentifier();

        // parse body
        const body = try self.parseBlock();

        const end_pos = self.currentPos();

        return try self.ast.addFuncDecl(
            name,
            params,
            return_type,
            body,
            start_pos,
            end_pos,
        );
    }

    fn parseParameters(self: *Parser) ParseError!ast.Range {
        var params = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 1);
        defer params.deinit(self.allocator);

        while (!self.check(.right_paren)) {
            // parse parameter name (identifier)
            const param_name = try self.parseIdentifier();
            try params.append(self.allocator, param_name);

            // expect :
            try self.expect(.colon);

            // parse parameter type (identifier)
            const param_type = try self.parseIdentifier();
            try params.append(self.allocator, param_type);

            // handle comma
            if (self.check(.comma)) {
                self.advance();
            } else {
                break;
            }
        }

        return try self.ast.addExtra(params.items);
    }

    fn parseBlock(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentPos();

        try self.expect(.left_curly);

        var statements = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 2);
        defer statements.deinit(self.allocator);

        var deferred = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 1);
        defer deferred.deinit(self.allocator);

        while (!self.check(.right_curly) and !self.check(.eof)) {
            const stmt = try self.parseStatement();

            // check if it's a defer statement
            const kind = self.ast.getKind(stmt);
            if (kind == .defer_stmt) {
                try deferred.append(self.allocator, stmt);
            } else {
                try statements.append(self.allocator, stmt);
            }
        }

        try self.expect(.right_curly);

        const stmt_range = try self.ast.addExtra(statements.items);
        const defer_range = try self.ast.addExtra(deferred.items);

        const end_pos = self.currentPos();

        return try self.ast.addBlock(stmt_range, defer_range, start_pos, end_pos);
    }

    fn parseStatement(self: *Parser) ParseError!NodeIndex {
        const token = self.peek() orelse return error.UnexpectedEof;

        return switch (token.kind) {
            .return_ => try self.parseReturn(),
            .defer_ => try self.parseDefer(),
            .mut => try self.parseVarDecl(),
            .identifier => blk: {
                // could be var decl or assignment
                const next = self.peekOffset(1) orelse return error.UnexpectedEof;
                if (next.kind == .colon) {
                    break :blk try self.parseVarDecl();
                } else if (next.kind == .equal or
                    next.kind == .plus_equal or
                    next.kind == .minus_equal or
                    next.kind == .star_equal or
                    next.kind == .slash_equal)
                {
                    break :blk try self.parseAssignment();
                } else {
                    // expression statement
                    break :blk try self.parseExpression();
                }
            },
            else => try self.parseExpression(),
        };
    }

    fn parseReturn(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentPos();

        try self.expect(.return_);

        const expr = try self.parseExpression();

        const end_pos = self.currentPos();

        return try self.ast.addReturn(expr, start_pos, end_pos);
    }

    fn parseDefer(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentPos();

        try self.expect(.defer_);

        const stmt = try self.parseStatement();

        const end_pos = self.currentPos();

        return try self.ast.addDefer(stmt, start_pos, end_pos);
    }

    fn parseVarDecl(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentPos();

        // optional 'mut'
        const is_mutable = self.match(.mut);

        // parse name
        const name = try self.parseIdentifier();

        // expect :
        try self.expect(.colon);

        // optional type
        const type_node = if (self.check(.identifier))
            try self.parseIdentifier()
        else
            null;

        // expect =
        try self.expect(.equal);

        // parse value
        const value = try self.parseExpression();

        const end_pos = self.currentPos();

        return try self.ast.addVarDecl(
            name,
            type_node,
            value,
            is_mutable,
            start_pos,
            end_pos,
        );
    }

    fn parseAssignment(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentPos();

        const target = try self.parseIdentifier();

        // check what kind of assignment
        const token = self.peek() orelse return error.UnexpectedEof;

        // handle compound assignment by desugaring to binary op + assignment
        if (token.kind == .plus_equal or
            token.kind == .minus_equal or
            token.kind == .star_equal or
            token.kind == .slash_equal)
        {
            const op: ast.BinaryOp.Op = switch (token.kind) {
                .plus_equal => .add,
                .minus_equal => .sub,
                .star_equal => .mul,
                .slash_equal => .div,
                else => unreachable,
            };

            self.advance(); // consume the compound operator

            const rhs = try self.parseExpression();

            // desugar: x += y  becomes  x = x + y
            // create a reference to target for the binary op
            const target_ref = try self.ast.addIdentifier(
                self.ast.getIdentifier(target).token_idx,
                self.ast.getLocation(target).start,
                self.ast.getLocation(target).end,
            );

            const binary_op = try self.ast.addBinaryOp(
                op,
                target_ref,
                rhs,
                start_pos,
                self.currentPos(),
            );

            const end_pos = self.currentPos();

            return try self.ast.addAssignment(target, binary_op, start_pos, end_pos);
        }

        // regular assignment
        try self.expect(.equal);

        const value = try self.parseExpression();

        const end_pos = self.currentPos();

        return try self.ast.addAssignment(target, value, start_pos, end_pos);
    }

    fn parseExpression(self: *Parser) ParseError!NodeIndex {
        return try self.parseLogicalOr();
    }

    fn parseLogicalOr(self: *Parser) ParseError!NodeIndex {
        var left = try self.parseLogicalAnd();

        while (self.match(.or_)) {
            const start_pos = self.ast.getLocation(left).start;
            const right = try self.parseLogicalAnd();
            const end_pos = self.currentPos();

            left = try self.ast.addBinaryOp(
                .or_,
                left,
                right,
                start_pos,
                end_pos,
            );
        }

        return left;
    }

    fn parseLogicalAnd(self: *Parser) ParseError!NodeIndex {
        var left = try self.parseComparative();

        while (self.match(.and_)) {
            const start_pos = self.ast.getLocation(left).start;
            const right = try self.parseComparative();
            const end_pos = self.currentPos();

            left = try self.ast.addBinaryOp(
                .and_,
                left,
                right,
                start_pos,
                end_pos,
            );
        }

        return left;
    }

    fn parseComparative(self: *Parser) ParseError!NodeIndex {
        var left = try self.parseAdditive();

        if (self.peek()) |token| {
            const op: ?ast.BinaryOp.Op = switch (token.kind) {
                .double_equal => .equal,
                .not_equal => .not_equal,
                .less => .less,
                .greater => .greater,
                .less_equal => .less_equal,
                .greater_equal => .greater_equal,
                else => null,
            };

            if (op) |binary_op| {
                const start_pos = self.ast.getLocation(left).start;
                self.advance();
                const right = try self.parseAdditive();
                const end_pos = self.currentPos();

                left = try self.ast.addBinaryOp(
                    binary_op,
                    left,
                    right,
                    start_pos,
                    end_pos,
                );
            }
        }

        return left;
    }

    fn parseAdditive(self: *Parser) ParseError!NodeIndex {
        var left = try self.parseMultiplicative();

        while (true) {
            const token = self.peek() orelse break;
            const op: ?ast.BinaryOp.Op = switch (token.kind) {
                .plus => .add,
                .minus => .sub,
                else => null,
            };

            if (op) |binary_op| {
                const start_pos = self.ast.getLocation(left).start;
                self.advance();
                const right = try self.parseMultiplicative();
                const end_pos = self.currentPos();

                left = try self.ast.addBinaryOp(
                    binary_op,
                    left,
                    right,
                    start_pos,
                    end_pos,
                );
            } else {
                break;
            }
        }

        return left;
    }

    fn parseMultiplicative(self: *Parser) ParseError!NodeIndex {
        var left = try self.parseUnary();

        while (true) {
            const token = self.peek() orelse break;
            const op: ?ast.BinaryOp.Op = switch (token.kind) {
                .star => .mul,
                .slash => .div,
                else => null,
            };

            if (op) |binary_op| {
                const start_pos = self.ast.getLocation(left).start;
                self.advance();
                const right = try self.parseUnary();
                const end_pos = self.currentPos();

                left = try self.ast.addBinaryOp(
                    binary_op,
                    left,
                    right,
                    start_pos,
                    end_pos,
                );
            } else {
                break;
            }
        }

        return left;
    }

    fn parseUnary(self: *Parser) ParseError!NodeIndex {
        const token = self.peek() orelse return error.UnexpectedEof;

        const op: ?ast.UnaryOp.Op = switch (token.kind) {
            .minus => .negate,
            .not => .not,
            else => null,
        };

        if (op) |unary_op| {
            const start_pos = self.currentPos();
            self.advance();
            const operand = try self.parseUnary(); // right associative
            const end_pos = self.currentPos();

            return try self.ast.addUnaryOp(unary_op, operand, start_pos, end_pos);
        }

        return try self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ParseError!NodeIndex {
        const token = self.peek() orelse return error.UnexpectedEof;

        return switch (token.kind) {
            .identifier => blk: {
                // check if it's a call expression
                if (self.peekOffset(1)) |next| {
                    if (next.kind == .left_paren) {
                        break :blk try self.parseCallExpr();
                    }
                }
                break :blk try self.parseIdentifier();
            },
            .number, .boolean => try self.parseLiteral(),
            .left_paren => blk: {
                self.advance();
                const expr = try self.parseExpression();
                try self.expect(.right_paren);
                break :blk expr;
            },
            else => error.UnexpectedToken,
        };
    }

    fn parseIdentifier(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentPos();

        try self.expect(.identifier);

        const end_pos = self.currentPos();
        const token_idx: u32 = @intCast(self.pos - 1);

        return try self.ast.addIdentifier(token_idx, start_pos, end_pos);
    }

    fn parseLiteral(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentPos();
        const token_idx: u32 = @intCast(self.pos);

        self.advance();

        const end_pos = self.currentPos();

        return try self.ast.addLiteral(token_idx, start_pos, end_pos);
    }

    fn parseCallExpr(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentPos();

        const func = try self.parseIdentifier();

        try self.expect(.left_paren);

        var args = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 1);
        defer args.deinit(self.allocator);

        while (!self.check(.right_paren)) {
            const arg = try self.parseExpression();
            try args.append(self.allocator, arg);

            if (!self.match(.comma)) break;
        }

        try self.expect(.right_paren);

        const args_range = try self.ast.addExtra(args.items);
        const end_pos = self.currentPos();

        return try self.ast.addCallExpr(func, args_range, start_pos, end_pos);
    }

    fn peek(self: *const Parser) ?Token {
        if (self.pos < self.tokens.len) {
            return self.tokens[self.pos];
        }
        return null;
    }

    fn peekOffset(self: *const Parser, offset: u32) ?Token {
        const idx = self.pos + offset;
        if (idx < self.tokens.len) {
            return self.tokens[idx];
        }
        return null;
    }

    fn check(self: *const Parser, kind: TokenKind) bool {
        const token = self.peek() orelse return false;
        return token.kind == kind;
    }

    fn match(self: *Parser, kind: TokenKind) bool {
        if (self.check(kind)) {
            self.advance();
            return true;
        }
        return false;
    }

    fn expect(self: *Parser, kind: TokenKind) ParseError!void {
        if (!self.check(kind)) {
            return error.UnexpectedToken;
        }
        self.advance();
    }

    fn advance(self: *Parser) void {
        self.pos += 1;
    }

    fn currentPos(self: *const Parser) SourceIndex {
        if (self.pos > 0 and self.pos <= self.tokens.len) {
            const token = self.tokens[self.pos - 1];
            return token.start;
        }
        return 0;
    }
};
