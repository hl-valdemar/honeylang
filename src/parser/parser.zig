const std = @import("std");
const mem = std.mem;

const Token = @import("../lexer/token.zig").Token;
const TokenKind = @import("../lexer/token.zig").Kind;
const TokenList = @import("../lexer/token.zig").TokenList;
const TokenIndex = @import("../lexer/token.zig").TokenIndex;

const ast = @import("ast.zig");
const Ast = @import("ast.zig").Ast;
const NodeIndex = @import("ast.zig").NodeIndex;
const SourceIndex = @import("../source/source.zig").SourceIndex;

const parse_error = @import("error.zig");
const ParseErrorKind = parse_error.ParseErrorKind;
const ErrorList = parse_error.ErrorList;

pub const error_printer = @import("error_printer.zig");

pub fn parse(allocator: mem.Allocator, tokens: TokenList) !ParseResult {
    var parser = try Parser.init(allocator, tokens);
    return parser.parse();
}

pub const ParseResult = struct {
    ast: Ast,
    errors: ErrorList,
};

pub const ParseError = error{
    UnexpectedEof,
    UnexpectedToken,
    OutOfMemory,
};

pub const Parser = struct {
    allocator: mem.Allocator,
    errors: ErrorList,
    tokens: []const Token,
    pos: TokenIndex,
    ast: Ast,

    pub fn init(allocator: mem.Allocator, tokens: TokenList) !Parser {
        return .{
            .allocator = allocator,
            .errors = try ErrorList.init(allocator),
            .tokens = tokens.items,
            .pos = 0,
            .ast = try Ast.init(allocator),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.ast.deinit();
        self.errors.deinit();
    }

    pub fn parse(self: *Parser) ParseError!ParseResult {
        const start_pos = self.currentStart();
        const decls = try self.parseDecls();
        const end_pos = self.previousEnd();

        const root = try self.ast.addProgram(decls, start_pos, end_pos);
        self.ast.root = root;

        return .{
            .ast = self.ast,
            .errors = self.errors,
        };
    }

    fn parseDecls(self: *Parser) ParseError!ast.Range {
        var decls = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 10);
        defer decls.deinit(self.allocator);

        while (!self.check(.eof)) {
            const decl = self.parseDeclaration() catch {
                // parsing functions already added detailed errors.
                // just create an error node and synchronize.
                const start = self.currentStart();
                var end = start;
                if (self.peek()) |tok| {
                    end = tok.start + tok.len;
                }

                // get the actual error kind from the last recorded error
                const error_kind = if (self.errors.errors.items.len > 0)
                    self.errors.errors.items[self.errors.errors.items.len - 1].kind
                else
                    ParseErrorKind.unexpected_token;

                // emit an error node (no additional error message)
                const err_node = try self.ast.addError(
                    @tagName(error_kind),
                    start,
                    end,
                );
                try decls.append(self.allocator, err_node);

                // skip to next declaration
                self.synchronize();
                continue;
            };
            try decls.append(self.allocator, decl);
        }

        return try self.ast.addExtra(decls.items);
    }

    fn parseDeclaration(self: *Parser) !NodeIndex {
        const token = self.peek() orelse {
            try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
            return error.UnexpectedEof;
        };

        // mutable runtime var
        if (token.kind == .mut) {
            return try self.parseVarDecl();
        }

        // otherwise, disambiguate
        if (token.kind == .identifier) {
            const next = self.peekOffset(1) orelse {
                try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
                return error.UnexpectedEof;
            };

            if (next.kind == .double_colon) {
                // `identifier :: ...` => const or func decl
                const after = self.peekOffset(2) orelse {
                    try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
                    return error.UnexpectedEof;
                };

                if (after.kind == .func) {
                    return try self.parseFuncDecl();
                } else if (after.kind == .abi_c) {
                    // check if followed by func
                    const after_abi = self.peekOffset(3) orelse {
                        try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
                        return error.UnexpectedEof;
                    };
                    if (after_abi.kind == .func) {
                        return try self.parseFuncDecl();
                    } else {
                        return try self.parseConstDecl();
                    }
                } else {
                    return try self.parseConstDecl();
                }
            } else if (next.kind == .colon) {
                // `identifier : ...` => need to disambiguate
                const after = self.peekOffset(2) orelse {
                    try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
                    return error.UnexpectedEof;
                };

                if (after.kind == .equal) {
                    // `identifier := ...` => runtime var (inferred type)
                    return try self.parseVarDecl();
                } else if (after.kind == .identifier) {
                    // `identifier : type ...` => check what follows type
                    const type_ = self.peekOffset(3) orelse {
                        try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
                        return error.UnexpectedEof;
                    };

                    if (type_.kind == .double_colon) {
                        // `identifier : type :: ...` => typed const decl
                        return try self.parseConstDecl();
                    } else if (type_.kind == .equal) {
                        // `identifier : type = ...` => typed runtime var
                        return try self.parseVarDecl();
                    }
                }
            }
        }

        try self.addErrorWithFound(.expected_declaration, token.kind, token.start, token.start + token.len);
        return error.UnexpectedToken;
    }

    fn parseConstDecl(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // parse name
        const name = try self.parseIdentifier();

        // parse optional type annotation
        const type_node = if (self.match(.colon))
            try self.parseIdentifier() // type is also an identifier
        else
            null;

        // expect ::
        try self.expectToken(.double_colon, .expected_double_colon);

        // parse value expression
        const value = try self.parseExpression();

        const end_pos = self.previousEnd();

        return try self.ast.addConstDecl(name, type_node, value, start_pos, end_pos);
    }

    fn parseFuncDecl(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // parse name
        const name = try self.parseIdentifier();

        // expect ::
        try self.expectToken(.double_colon, .expected_double_colon);

        // parse optional calling convention
        const calling_conv: ast.CallingConvention = if (self.match(.abi_c))
            .c
        else if (self.match(.abi_cobol))
            .cobol
        else if (self.match(.abi_fortran))
            .fortran
        else
            .honey;

        // expect 'func'
        try self.expectToken(.func, .unexpected_token);

        // expect (
        try self.expectToken(.left_paren, .expected_left_paren);

        // parse parameters
        const params = try self.parseParameters();

        // expect )
        try self.expectToken(.right_paren, .expected_right_paren);

        // parse return type
        const return_type = try self.parseIdentifier();

        // parse body (optional for external functions)
        const body: ?NodeIndex = if (self.check(.left_curly))
            try self.parseBlock()
        else
            null;

        const end_pos = self.previousEnd();

        return try self.ast.addFuncDecl(name, params, return_type, body, calling_conv, start_pos, end_pos);
    }

    fn parseParameters(self: *Parser) ParseError!ast.Range {
        var params = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 1);
        defer params.deinit(self.allocator);

        while (!self.check(.right_paren)) {
            // parse parameter name (identifier)
            const param_name = try self.parseIdentifier();
            try params.append(self.allocator, param_name);

            // expect :
            try self.expectToken(.colon, .expected_colon);

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
        const start_pos = self.currentStart();

        try self.expectToken(.left_curly, .expected_left_curly);

        var statements = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 2);
        defer statements.deinit(self.allocator);

        var deferred = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 1);
        defer deferred.deinit(self.allocator);

        while (!self.check(.right_curly) and !self.check(.eof)) {
            const stmt = self.parseStatement() catch {
                // parsing functions already added detailed errors.
                // just create an error node and synchronize.
                const err_start = self.currentStart();
                var err_end = err_start;
                if (self.peek()) |tok| {
                    err_end = tok.start + tok.len;
                }

                // get the actual error kind from the last recorded error
                const error_kind = if (self.errors.errors.items.len > 0)
                    self.errors.errors.items[self.errors.errors.items.len - 1].kind
                else
                    ParseErrorKind.unexpected_token;

                // emit an error node (no additional error message)
                const err_node = try self.ast.addError(@tagName(error_kind), err_start, err_end);
                try statements.append(self.allocator, err_node);

                // skip to next statement
                self.synchronizeStatement();
                continue;
            };

            // check if it's a defer statement
            const kind = self.ast.getKind(stmt);
            if (kind == .defer_stmt) {
                try deferred.append(self.allocator, stmt);
            } else {
                try statements.append(self.allocator, stmt);
            }
        }

        if (self.check(.eof) and !self.check(.right_curly)) {
            try self.addError(.unclosed_brace, start_pos, self.currentStart());
        }

        try self.expectToken(.right_curly, .expected_right_curly);

        const stmt_range = try self.ast.addExtra(statements.items);
        const defer_range = try self.ast.addExtra(deferred.items);

        const end_pos = self.previousEnd();

        return try self.ast.addBlock(stmt_range, defer_range, start_pos, end_pos);
    }

    fn parseStatement(self: *Parser) ParseError!NodeIndex {
        const token = self.peek() orelse {
            try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
            return error.UnexpectedEof;
        };

        return switch (token.kind) {
            .return_ => try self.parseReturn(),
            .defer_ => try self.parseDefer(),
            .mut => try self.parseVarDecl(),
            .identifier => blk: {
                // could be var decl or assignment
                const next = self.peekOffset(1) orelse {
                    try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
                    return error.UnexpectedEof;
                };
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
            .if_ => try self.parseIfStmt(),
            else => try self.parseExpression(),
        };
    }

    fn parseReturn(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        try self.expectToken(.return_, .unexpected_token);

        const expr = try self.parseExpression();

        const end_pos = self.previousEnd();

        return try self.ast.addReturn(expr, start_pos, end_pos);
    }

    fn parseDefer(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        try self.expectToken(.defer_, .unexpected_token);

        const stmt = try self.parseStatement();

        const end_pos = self.previousEnd();

        return try self.ast.addDefer(stmt, start_pos, end_pos);
    }

    fn parseVarDecl(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // optional 'mut'
        const is_mutable = self.match(.mut);

        // parse name
        const name = try self.parseIdentifier();

        // expect :
        try self.expectToken(.colon, .expected_colon);

        // optional type
        const type_node = if (self.check(.identifier))
            try self.parseIdentifier()
        else
            null;

        // expect =
        try self.expectToken(.equal, .expected_equal);

        // parse value
        const value = try self.parseExpression();

        const end_pos = self.previousEnd();

        return try self.ast.addVarDecl(name, type_node, value, is_mutable, start_pos, end_pos);
    }

    fn parseAssignment(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        const target = try self.parseIdentifier();

        // check what kind of assignment
        const token = self.peek() orelse {
            try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
            return error.UnexpectedEof;
        };

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

            const binary_op = try self.ast.addBinaryOp(op, target_ref, rhs, start_pos, self.currentStart());

            const end_pos = self.previousEnd();

            return try self.ast.addAssignment(target, binary_op, start_pos, end_pos);
        }

        // regular assignment
        try self.expectToken(.equal, .expected_equal);

        const value = try self.parseExpression();

        const end_pos = self.previousEnd();

        return try self.ast.addAssignment(target, value, start_pos, end_pos);
    }

    fn parseIfStmt(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // expect 'if'
        try self.expectToken(.if_, .unexpected_token);

        // expect boolean expression
        const if_guard = if (self.check(.left_paren)) blk: {
            self.advance(); // consume left
            const if_guard = try self.parseBooleanExpr();
            try self.expectToken(.right_paren, .expected_right_paren);
            break :blk if_guard;
        } else blk: {
            break :blk try self.parseBooleanExpr();
        };

        // expect if block
        const if_block = try self.parseBlock();

        // parse 'else if's / collect interleaved (guard, block) pairs
        var else_ifs = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 4);
        defer else_ifs.deinit(self.allocator);

        var i: NodeIndex = 0;
        while (self.check(.else_)) : (i += 1) {
            // check for 'else if'
            if (!self.checkOffset(.if_, 1) or i > 10) {
                break;
            }

            // consume 'else if'
            self.advanceN(2);

            // expect boolean expression
            const guard = if (self.check(.left_paren)) guard_blk: {
                self.advance(); // consume left
                const guard = try self.parseBooleanExpr();
                try self.expectToken(.right_paren, .expected_right_paren);
                break :guard_blk guard;
            } else guard_blk: {
                break :guard_blk try self.parseBooleanExpr();
            };

            const block = try self.parseBlock();

            // store as interleaved pair
            try else_ifs.append(self.allocator, guard);
            try else_ifs.append(self.allocator, block);
        }

        // convert to range
        const else_ifs_range = try self.ast.addExtra(else_ifs.items);

        // parse else block
        const else_block: ?NodeIndex = if (self.check(.else_)) blk: {
            self.advance();
            break :blk try self.parseBlock();
        } else blk: {
            break :blk null;
        };

        const end_pos = self.previousEnd();

        return try self.ast.addIf(
            if_guard,
            if_block,
            else_ifs_range,
            else_block,
            start_pos,
            end_pos,
        );
    }

    fn parseExpression(self: *Parser) ParseError!NodeIndex {
        // TODO: also parse call expressions
        return try self.parseLogicalOr();
    }

    fn parseBooleanExpr(self: *Parser) ParseError!NodeIndex {
        return try self.parseLogicalOr();
    }

    fn parseLogicalOr(self: *Parser) ParseError!NodeIndex {
        var left = try self.parseLogicalAnd();

        while (self.match(.or_)) {
            const start_pos = self.ast.getLocation(left).start;
            const right = try self.parseLogicalAnd();
            const end_pos = self.previousEnd();

            left = try self.ast.addBinaryOp(.or_, left, right, start_pos, end_pos);
        }

        return left;
    }

    fn parseLogicalAnd(self: *Parser) ParseError!NodeIndex {
        var left = try self.parseComparative();

        while (self.match(.and_)) {
            const start_pos = self.ast.getLocation(left).start;
            const right = try self.parseComparative();
            const end_pos = self.previousEnd();

            left = try self.ast.addBinaryOp(.and_, left, right, start_pos, end_pos);
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
                const end_pos = self.previousEnd();

                left = try self.ast.addBinaryOp(binary_op, left, right, start_pos, end_pos);
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
                const end_pos = self.previousEnd();

                left = try self.ast.addBinaryOp(binary_op, left, right, start_pos, end_pos);
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
                const end_pos = self.previousEnd();

                left = try self.ast.addBinaryOp(binary_op, left, right, start_pos, end_pos);
            } else {
                break;
            }
        }

        return left;
    }

    fn parseUnary(self: *Parser) ParseError!NodeIndex {
        const token = self.peek() orelse {
            try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
            return error.UnexpectedEof;
        };

        const op: ?ast.UnaryOp.Op = switch (token.kind) {
            .minus => .negate,
            .not => .not,
            else => null,
        };

        if (op) |unary_op| {
            const start_pos = self.currentStart();
            self.advance();
            const operand = try self.parseUnary(); // right associative
            const end_pos = self.previousEnd();

            return try self.ast.addUnaryOp(unary_op, operand, start_pos, end_pos);
        }

        return try self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ParseError!NodeIndex {
        const token = self.peek() orelse {
            try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
            return error.UnexpectedEof;
        };

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
            .number, .bool => try self.parseLiteral(),
            .left_paren => blk: {
                const paren_start = self.currentStart();
                self.advance();
                const expr = try self.parseExpression();
                if (!self.check(.right_paren)) {
                    try self.addError(.unclosed_paren, paren_start, self.currentStart());
                }
                try self.expectToken(.right_paren, .expected_right_paren);
                break :blk expr;
            },
            else => {
                try self.addErrorWithFound(.expected_expression, token.kind, token.start, token.start + token.len);
                return error.UnexpectedToken;
            },
        };
    }

    fn parseIdentifier(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        const token = self.peek() orelse {
            try self.addError(.unexpected_eof, start_pos, start_pos);
            return error.UnexpectedEof;
        };

        if (token.kind != .identifier) {
            try self.addErrorWithFound(.expected_identifier, token.kind, token.start, token.start + token.len);
            return error.UnexpectedToken;
        }

        self.advance();

        const end_pos = self.previousEnd();
        const token_idx: TokenIndex = @intCast(self.pos - 1);

        return try self.ast.addIdentifier(token_idx, start_pos, end_pos);
    }

    fn parseLiteral(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();
        const token_idx: TokenIndex = @intCast(self.pos);

        self.advance();

        const end_pos = self.previousEnd();

        return try self.ast.addLiteral(token_idx, start_pos, end_pos);
    }

    fn parseCallExpr(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        const func = try self.parseIdentifier();

        const paren_start = self.currentStart();
        try self.expectToken(.left_paren, .expected_left_paren);

        var args = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 1);
        defer args.deinit(self.allocator);

        while (!self.check(.right_paren) and !self.check(.eof)) {
            const arg = try self.parseExpression();
            try args.append(self.allocator, arg);

            if (!self.match(.comma)) break;
        }

        if (self.check(.eof)) {
            try self.addError(.unclosed_paren, paren_start, self.currentStart());
        }

        try self.expectToken(.right_paren, .expected_right_paren);

        const args_range = try self.ast.addExtra(args.items);
        const end_pos = self.previousEnd();

        return try self.ast.addCallExpr(func, args_range, start_pos, end_pos);
    }

    fn peek(self: *const Parser) ?Token {
        if (self.pos < self.tokens.len) {
            return self.tokens[self.pos];
        }
        return null;
    }

    fn peekOffset(self: *const Parser, offset: TokenIndex) ?Token {
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

    fn checkOffset(self: *const Parser, kind: TokenKind, offset: TokenIndex) bool {
        const token = self.peekOffset(offset) orelse return false;
        return token.kind == kind;
    }

    fn match(self: *Parser, kind: TokenKind) bool {
        if (self.check(kind)) {
            self.advance();
            return true;
        }
        return false;
    }

    fn expectToken(self: *Parser, kind: TokenKind, error_kind: ParseErrorKind) ParseError!void {
        if (self.check(kind)) {
            self.advance();
            return;
        }

        const token = self.peek();
        if (token) |tok| {
            try self.errors.addExpected(
                error_kind,
                kind,
                tok.kind,
                tok.start,
                tok.start + tok.len,
            );
        } else {
            try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
        }

        return error.UnexpectedToken;
    }

    fn advance(self: *Parser) void {
        self.pos += 1;
    }

    fn advanceN(self: *Parser, n: TokenIndex) void {
        self.pos += n;
    }

    /// Return the start position of the current token (before consuming).
    fn currentStart(self: *const Parser) SourceIndex {
        if (self.pos < self.tokens.len) {
            return self.tokens[self.pos].start;
        }
        // at end, return end of last token
        if (self.tokens.len > 0) {
            const last = self.tokens[self.tokens.len - 1];
            return last.start + last.len;
        }
        return 0;
    }

    /// Return the end position of the previous token (after consuming).
    fn previousEnd(self: *const Parser) SourceIndex {
        if (self.pos > 0) {
            const prev = self.tokens[self.pos - 1];
            return prev.start + prev.len;
        }
        return 0;
    }

    fn synchronize(self: *Parser) void {
        // always advance at least once to ensure progress.
        // this prevents infinite loops when we're already at what looks like
        // a declaration boundary but parsing failed (e.g., "CONST : 42").
        if (self.peek()) |token| {
            if (token.kind != .eof) {
                self.advance();
            }
        }

        // skip tokens until we find what looks like a new declaration:
        // identifier followed by ::, :, or :=
        while (self.peek()) |token| {
            if (token.kind == .eof) return;

            if (token.kind == .identifier) {
                if (self.peekOffset(1)) |next| {
                    if (next.kind == .double_colon or next.kind == .colon) {
                        return; // found declaration boundary
                    }
                }
            }

            // also stop at 'mut' which starts a var decl
            if (token.kind == .mut) return;

            self.advance();
        }
    }

    fn synchronizeStatement(self: *Parser) void {
        // skip tokens until we find what looks like a new statement
        while (self.peek()) |token| {
            if (token.kind == .eof) return;
            if (token.kind == .right_curly) return;

            // statement starters
            if (token.kind == .return_ or
                token.kind == .defer_ or
                token.kind == .if_ or
                token.kind == .mut)
            {
                return;
            }

            // identifier could start assignment or var decl
            if (token.kind == .identifier) {
                if (self.peekOffset(1)) |next| {
                    if (next.kind == .colon or
                        next.kind == .equal or
                        next.kind == .plus_equal or
                        next.kind == .minus_equal)
                    {
                        return;
                    }
                }
            }

            self.advance();
        }
    }

    fn addError(self: *Parser, kind: ParseErrorKind, start: SourceIndex, end: SourceIndex) !void {
        try self.errors.addSimple(kind, start, end);
    }

    fn addErrorWithFound(self: *Parser, kind: ParseErrorKind, found: TokenKind, start: SourceIndex, end: SourceIndex) !void {
        try self.errors.add(.{
            .kind = kind,
            .start = start,
            .end = end,
            .found = found,
        });
    }
};
