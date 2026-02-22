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
const SourceCode = @import("../source/source.zig").SourceCode;

const parse_error = @import("error.zig");
const ParseErrorKind = parse_error.ParseErrorKind;
const ErrorList = parse_error.ErrorList;

pub const error_printer = @import("error-printer.zig");

pub fn parse(allocator: mem.Allocator, tokens: TokenList, src: *const SourceCode) !ParseResult {
    var parser = try Parser.init(allocator, tokens, src);
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
    src: *const SourceCode,
    pos: TokenIndex,
    ast: Ast,

    pub fn init(allocator: mem.Allocator, tokens: TokenList, src: *const SourceCode) !Parser {
        return .{
            .allocator = allocator,
            .errors = try ErrorList.init(allocator),
            .tokens = tokens.items,
            .src = src,
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

        // Parse imports first (must appear before other declarations)
        while (self.check(.import) or self.isNamedImport()) {
            const import_node = if (self.isNamedImport())
                try self.parseNamedImportDecl()
            else
                try self.parseImportDecl(null);
            try decls.append(self.allocator, import_node);
        }

        while (!self.check(.eof)) {
            const decl = self.parseTopLevelDeclaration() catch {
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

    fn parseImportDecl(self: *Parser, name_token: ?u32) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // consume 'import'
        try self.expectToken(.import, .unexpected_token);

        // Check for C import variants: `import c include "path"` or `import c { ... }`
        if (self.isIdentifierText(0, "c")) {
            self.advance(); // consume 'c'

            // Block form: import c { include "..." define "..." }
            if (self.check(.left_curly)) {
                if (name_token == null) {
                    // Block form requires an explicit name — recover by skipping the block
                    try self.addError(.expected_identifier, self.currentStart(), self.currentStart());
                    self.advance(); // consume '{'
                    var depth: u32 = 1;
                    while (depth > 0 and !self.check(.eof)) {
                        if (self.check(.left_curly)) depth += 1;
                        if (self.check(.right_curly)) {
                            depth -= 1;
                            if (depth == 0) break;
                        }
                        self.advance();
                    }
                    if (self.check(.right_curly)) self.advance();
                    return try self.ast.addError("c import block requires a name", start_pos, self.previousEnd());
                }
                return try self.parseCImportBlock(name_token.?, start_pos);
            }

            // `import c include "path"` is invalid — use `import c "path"` instead
            if (self.isIdentifierText(0, "include")) {
                const tok = self.peek().?;
                try self.addError(.unexpected_token, tok.start, tok.start + tok.len);
                self.advance(); // skip 'include'
                // Skip the string literal too if present, so we don't misparse it
                if (self.peek()) |next| {
                    if (next.kind == .string_literal) self.advance();
                }
                return try self.ast.addError("use `import c \"path\"` instead of `import c include \"path\"`", start_pos, self.previousEnd());
            }

            // Simple include: `import c "path"`
            {
                const token = self.peek() orelse {
                    try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
                    return error.UnexpectedEof;
                };

                if (token.kind == .string_literal) {
                    const path_token_idx: u32 = @intCast(self.pos);
                    self.advance();

                    const end_pos = self.previousEnd();
                    return try self.ast.addCIncludeDecl(path_token_idx, name_token, start_pos, end_pos);
                }
            }

            // `import c` followed by something unexpected
            const token = self.peek() orelse {
                try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
                return error.UnexpectedEof;
            };
            try self.addErrorWithFound(.unexpected_token, token.kind, token.start, token.start + token.len);
            return error.UnexpectedToken;
        }

        // Standard import: import "file.hon"
        const token = self.peek() orelse {
            try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
            return error.UnexpectedEof;
        };

        if (token.kind != .string_literal) {
            try self.addErrorWithFound(.expected_string_literal, token.kind, token.start, token.start + token.len);
            return error.UnexpectedToken;
        }

        const path_token_idx: u32 = @intCast(self.pos);
        self.advance();

        const end_pos = self.previousEnd();

        return try self.ast.addImportDecl(path_token_idx, name_token, start_pos, end_pos);
    }

    /// Parse `import c { include "..." define "..." }` block form.
    fn parseCImportBlock(self: *Parser, name_token: u32, start_pos: SourceIndex) ParseError!NodeIndex {
        // consume '{'
        try self.expectToken(.left_curly, .expected_left_curly);

        var includes = try std.ArrayList(ast.NodeIndex).initCapacity(self.allocator, 4);
        defer includes.deinit(self.allocator);

        var defines = try std.ArrayList(ast.NodeIndex).initCapacity(self.allocator, 4);
        defer defines.deinit(self.allocator);

        while (!self.check(.right_curly) and !self.check(.eof)) {
            if (self.isIdentifierText(0, "include")) {
                self.advance(); // consume 'include'

                const token = self.peek() orelse {
                    try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
                    return error.UnexpectedEof;
                };
                if (token.kind != .string_literal) {
                    try self.addErrorWithFound(.expected_string_literal, token.kind, token.start, token.start + token.len);
                    return error.UnexpectedToken;
                }
                try includes.append(self.allocator, @intCast(self.pos));
                self.advance();
            } else if (self.isIdentifierText(0, "define")) {
                self.advance(); // consume 'define'

                const token = self.peek() orelse {
                    try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
                    return error.UnexpectedEof;
                };
                if (token.kind != .string_literal) {
                    try self.addErrorWithFound(.expected_string_literal, token.kind, token.start, token.start + token.len);
                    return error.UnexpectedToken;
                }
                try defines.append(self.allocator, @intCast(self.pos));
                self.advance();
            } else {
                const token = self.peek() orelse {
                    try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
                    return error.UnexpectedEof;
                };
                try self.addErrorWithFound(.unexpected_token, token.kind, token.start, token.start + token.len);
                return error.UnexpectedToken;
            }
        }

        try self.expectToken(.right_curly, .expected_right_curly);

        const includes_range = try self.ast.addExtra(includes.items);
        const defines_range = try self.ast.addExtra(defines.items);

        const end_pos = self.previousEnd();

        return try self.ast.addCImportBlock(name_token, includes_range, defines_range, start_pos, end_pos);
    }

    /// Check if the current position looks like `identifier :: import ...`
    fn isNamedImport(self: *const Parser) bool {
        if (!self.check(.identifier)) return false;
        const next = self.peekOffset(1) orelse return false;
        if (next.kind != .double_colon) return false;
        const after = self.peekOffset(2) orelse return false;
        return after.kind == .import;
    }

    /// Parse `name :: import ...` — consume the name and ::, then delegate to parseImportDecl.
    fn parseNamedImportDecl(self: *Parser) ParseError!NodeIndex {
        const name_token_idx: u32 = @intCast(self.pos);
        self.advance(); // consume identifier
        self.advance(); // consume ::
        return try self.parseImportDecl(name_token_idx);
    }

    /// Parse a top-level declaration, supporting 'pub' annotation for exported files.
    fn parseTopLevelDeclaration(self: *Parser) ParseError!NodeIndex {
        if (self.check(.@"pub")) {
            const pub_start = self.currentStart();
            self.advance(); // consume 'pub'

            const inner = try self.parseDeclaration();

            const pub_end = self.previousEnd();
            return try self.ast.addPubDecl(inner, pub_start, pub_end);
        }

        return try self.parseDeclaration();
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
                } else if (after.kind == .@"struct") {
                    return try self.parseStructDecl();
                } else if (after.kind == .namespace) {
                    return try self.parseNamespaceDecl();
                } else if (after.kind == .@"opaque") {
                    return try self.parseOpaqueDecl();
                } else if (after.kind == .identifier and self.isCallingConvention(2)) {
                    // check if calling convention is followed by func or struct
                    const after_cc = self.peekOffset(3) orelse {
                        try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
                        return error.UnexpectedEof;
                    };
                    if (after_cc.kind == .func) {
                        return try self.parseFuncDecl();
                    } else if (after_cc.kind == .@"struct") {
                        return try self.parseStructDecl();
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
                } else if (after.kind == .left_bracket) {
                    // `identifier : [...]type ...` => skip past array/slice type to find = or ::
                    var skip: u32 = 3; // start after `[`

                    // Check if next token after `[` is `]` (slice) or number/_ (array)
                    if (self.peekOffset(skip)) |len_tok| {
                        if (len_tok.kind == .number or len_tok.kind == .identifier) skip += 1;
                        // else: `]` follows immediately → slice type, don't skip
                    }
                    // skip `]`
                    if (self.peekOffset(skip)) |rb| {
                        if (rb.kind == .right_bracket) skip += 1;
                    }
                    // skip optional `mut`
                    if (self.peekOffset(skip)) |m| {
                        if (m.kind == .mut) skip += 1;
                    }
                    // skip element type identifier
                    if (self.peekOffset(skip)) |et| {
                        if (et.kind == .identifier) skip += 1;
                    }
                    // check what follows: `::` => const, `=` => var
                    if (self.peekOffset(skip)) |delim| {
                        if (delim.kind == .double_colon) {
                            return try self.parseConstDecl();
                        }
                    }
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
            try self.parseType()
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

        // parse optional calling convention (c, cobol, fortran)
        const calling_conv = self.matchCallingConvention();

        // expect 'func'
        try self.expectToken(.func, .unexpected_token);

        // expect (
        try self.expectToken(.left_paren, .expected_left_paren);

        // parse parameters, checking for variadic ...
        var is_variadic = false;
        const params = try self.parseParameters();
        if (self.check(.ellipsis)) {
            self.advance(); // consume ...
            is_variadic = true;
        }

        // expect )
        try self.expectToken(.right_paren, .expected_right_paren);

        // parse return type
        const return_type = try self.parseType();

        // parse body (optional for external functions)
        const body: ?NodeIndex = if (self.check(.left_curly))
            try self.parseBlock()
        else
            null;

        const end_pos = self.previousEnd();

        return try self.ast.addFuncDecl(name, params, return_type, body, calling_conv, is_variadic, start_pos, end_pos);
    }

    fn parseStructDecl(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // parse name
        const name = try self.parseIdentifier();

        // expect ::
        try self.expectToken(.double_colon, .expected_double_colon);

        // parse optional calling convention (c, cobol, fortran)
        const calling_conv = self.matchCallingConvention();

        // expect 'struct'
        try self.expectToken(.@"struct", .unexpected_token);

        // expect {
        try self.expectToken(.left_curly, .expected_left_curly);

        // Detect tuple vs named struct: if identifier followed by colon → named fields;
        // otherwise → positional (tuple) fields
        const is_tuple = blk: {
            if (self.check(.right_curly)) break :blk false; // empty struct
            if (self.peekOffset(1)) |after_first| {
                break :blk after_first.kind != .colon;
            }
            break :blk false;
        };

        // parse fields
        const fields = if (is_tuple)
            try self.parseTupleFields()
        else
            try self.parseStructFields();

        // expect }
        try self.expectToken(.right_curly, .expected_right_curly);

        const end_pos = self.previousEnd();

        return try self.ast.addStructDecl(name, fields, calling_conv, is_tuple, start_pos, end_pos);
    }

    fn parseOpaqueDecl(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // parse name
        const name = try self.parseIdentifier();

        // expect ::
        try self.expectToken(.double_colon, .expected_double_colon);

        // expect 'opaque'
        try self.expectToken(.@"opaque", .unexpected_token);

        const end_pos = self.previousEnd();

        return try self.ast.addOpaqueDecl(name, start_pos, end_pos);
    }

    fn parseStructFields(self: *Parser) ParseError!ast.Range {
        var fields = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 4);
        defer fields.deinit(self.allocator);

        while (!self.check(.right_curly)) {
            // parse field name
            const field_name = try self.parseIdentifier();
            try fields.append(self.allocator, field_name);

            // expect :
            try self.expectToken(.colon, .expected_colon);

            // parse field type
            const field_type = try self.parseType();
            try fields.append(self.allocator, field_type);

            // handle optional comma
            if (self.check(.comma)) {
                self.advance();
            } else {
                break;
            }
        }

        return try self.ast.addExtra(fields.items);
    }

    /// Parse tuple fields: comma-separated types with placeholder name nodes.
    /// Produces (name, type) pairs like parseStructFields, but name nodes use a sentinel
    /// value (maxInt) indicating the field should use a synthetic positional name.
    fn parseTupleFields(self: *Parser) ParseError!ast.Range {
        var fields = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 4);
        defer fields.deinit(self.allocator);

        while (!self.check(.right_curly)) {
            // Sentinel for positional field name — semantic analysis maps to "0", "1", etc.
            try fields.append(self.allocator, std.math.maxInt(NodeIndex));

            // parse field type
            const field_type = try self.parseType();
            try fields.append(self.allocator, field_type);

            // handle optional comma
            if (self.check(.comma)) {
                self.advance();
            } else {
                break;
            }
        }

        return try self.ast.addExtra(fields.items);
    }

    fn parseNamespaceDecl(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // parse name
        const name = try self.parseIdentifier();

        // expect ::
        try self.expectToken(.double_colon, .expected_double_colon);

        // expect 'namespace'
        try self.expectToken(.namespace, .unexpected_token);

        // expect {
        try self.expectToken(.left_curly, .expected_left_curly);

        // parse namespace body
        const declarations = try self.parseNamespaceBody();

        // expect }
        try self.expectToken(.right_curly, .expected_right_curly);

        const end_pos = self.previousEnd();

        return try self.ast.addNamespaceDecl(name, declarations, start_pos, end_pos);
    }

    fn parseNamespaceBody(self: *Parser) ParseError!ast.Range {
        var members = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 4);
        defer members.deinit(self.allocator);

        while (!self.check(.right_curly) and !self.check(.eof)) {
            const member = blk: {
                if (self.check(.@"pub")) {
                    const pub_start = self.currentStart();
                    self.advance(); // consume 'pub'

                    const inner = self.parseDeclaration() catch {
                        self.synchronize();
                        continue;
                    };

                    const pub_end = self.previousEnd();
                    break :blk try self.ast.addPubDecl(inner, pub_start, pub_end);
                } else {
                    break :blk self.parseDeclaration() catch {
                        self.synchronize();
                        continue;
                    };
                }
            };
            try members.append(self.allocator, member);
        }

        return try self.ast.addExtra(members.items);
    }

    fn parseParameters(self: *Parser) ParseError!ast.Range {
        var params = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 1);
        defer params.deinit(self.allocator);

        while (!self.check(.right_paren) and !self.check(.ellipsis)) {
            // parse parameter name (identifier)
            const param_name = try self.parseIdentifier();
            try params.append(self.allocator, param_name);

            // expect :
            try self.expectToken(.colon, .expected_colon);

            // parse parameter type
            const param_type = try self.parseType();
            try params.append(self.allocator, param_type);

            // handle comma (may be followed by ... for variadics)
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
            .@"return" => try self.parseReturn(),
            .@"defer" => try self.parseDefer(),
            .mut => try self.parseVarDecl(),
            .identifier => blk: {
                // could be var decl, assignment, field assignment, or expression
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
                } else if (next.kind == .dot or next.kind == .caret or next.kind == .left_bracket) {
                    // could be field access, dereference, or assignment through either
                    break :blk try self.parseExpressionOrFieldAssignment();
                } else {
                    // expression statement
                    break :blk try self.parseExpression();
                }
            },
            .@"if" => try self.parseIfStmt(),
            else => try self.parseExpression(),
        };
    }

    fn parseReturn(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        try self.expectToken(.@"return", .unexpected_token);

        // bare return (void) — next token cannot start an expression
        const expr = if (self.peek()) |token| switch (token.kind) {
            .identifier, .number, .bool, .left_paren, .left_bracket, .minus, .not, .ampersand => try self.parseExpression(),
            else => try self.ast.addVoidLiteral(start_pos, self.previousEnd()),
        } else try self.ast.addVoidLiteral(start_pos, self.previousEnd());

        const end_pos = self.previousEnd();

        return try self.ast.addReturn(expr, start_pos, end_pos);
    }

    fn parseDefer(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        try self.expectToken(.@"defer", .unexpected_token);

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
        const type_node = if (self.check(.identifier) or self.check(.at) or self.check(.star) or self.check(.left_bracket))
            try self.parseType()
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

    /// Parse an expression that might turn out to be a field assignment.
    /// Handles: `p.x = 10`, `r.origin.x = 10`, or just `p.x` as expression statement.
    fn parseExpressionOrFieldAssignment(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // Parse the full expression (handles field access chains via parsePrimary)
        const expr = try self.parseExpression();

        // Check if followed by assignment operator
        const token = self.peek() orelse return expr;

        if (token.kind == .equal) {
            // field/deref assignment: p.x = value, p^ = value
            self.advance(); // consume '='
            const value = try self.parseExpression();
            const end_pos = self.previousEnd();
            return try self.ast.addAssignment(expr, value, start_pos, end_pos);
        }

        // compound assignment: p^ += value, p.x -= value, etc.
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

            // desugar: p^ += y  becomes  p^ = p^ + y
            const target_ref = try self.ast.duplicateExpr(expr);
            const binary_op = try self.ast.addBinaryOp(op, target_ref, rhs, start_pos, self.currentStart());
            const end_pos = self.previousEnd();
            return try self.ast.addAssignment(expr, binary_op, start_pos, end_pos);
        }

        // Not an assignment, just an expression statement
        return expr;
    }

    fn parseIfStmt(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // expect 'if'
        try self.expectToken(.@"if", .unexpected_token);

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
        while (self.check(.@"else")) : (i += 1) {
            // check for 'else if'
            if (!self.checkOffset(.@"if", 1) or i > 10) {
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
        const else_block: ?NodeIndex = if (self.check(.@"else")) blk: {
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

        while (self.match(.@"or")) {
            const start_pos = self.ast.getLocation(left).start;
            const right = try self.parseLogicalAnd();
            const end_pos = self.previousEnd();

            left = try self.ast.addBinaryOp(.@"or", left, right, start_pos, end_pos);
        }

        return left;
    }

    fn parseLogicalAnd(self: *Parser) ParseError!NodeIndex {
        var left = try self.parseComparative();

        while (self.match(.@"and")) {
            const start_pos = self.ast.getLocation(left).start;
            const right = try self.parseComparative();
            const end_pos = self.previousEnd();

            left = try self.ast.addBinaryOp(.@"and", left, right, start_pos, end_pos);
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

        // address-of: &expr
        if (token.kind == .ampersand) {
            const start_pos = self.currentStart();
            self.advance();
            const operand = try self.parseUnary(); // right associative
            const end_pos = self.previousEnd();
            return try self.ast.addAddressOf(operand, start_pos, end_pos);
        }

        return try self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ParseError!NodeIndex {
        const token = self.peek() orelse {
            try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
            return error.UnexpectedEof;
        };

        var expr = switch (token.kind) {
            .identifier => blk: {
                if (self.peekOffset(1)) |next| {
                    if (next.kind == .left_paren) {
                        break :blk try self.parseCallExpr();
                    }
                    // struct/tuple literal: Identifier { .field = ... } or Identifier { val, val }
                    if (next.kind == .left_curly) {
                        if (self.peekOffset(2)) |after_curly| {
                            if (after_curly.kind == .dot or after_curly.kind == .right_curly) {
                                // named struct literal: starts with . or empty
                                break :blk try self.parseStructLiteral();
                            }
                            // tuple literal: Identifier { expr, expr }
                            // Check for expr followed by comma or } to distinguish from blocks
                            // We parse it and look for comma after first expression
                            if (after_curly.kind == .number or after_curly.kind == .string_literal or after_curly.kind == .bool or after_curly.kind == .left_bracket or after_curly.kind == .left_paren) {
                                // likely a tuple literal (expression start, not a keyword)
                                const name_node = try self.parseIdentifier();
                                break :blk try self.parseTupleLiteralWithType(name_node);
                            }
                            // Could be Identifier { identifier, ... } — check if the identifier
                            // after { is followed by comma (tuple) vs other (block/statement)
                            if (after_curly.kind == .identifier) {
                                if (self.peekOffset(3)) |after_first_expr| {
                                    if (after_first_expr.kind == .comma) {
                                        const name_node = try self.parseIdentifier();
                                        break :blk try self.parseTupleLiteralWithType(name_node);
                                    }
                                }
                            }
                        }
                    }
                }
                break :blk try self.parseIdentifier();
            },
            .number, .bool, .string_literal => try self.parseLiteral(),
            .left_bracket => try self.parseArrayLiteral(),
            .left_curly => blk: {
                // Anonymous tuple literal: { expr, expr, ... }
                // Only valid when { appears in expression position with comma-separated values
                break :blk try self.parseAnonymousTupleLiteral();
            },
            .left_paren => blk: {
                const paren_start = self.currentStart();
                self.advance();
                const inner = try self.parseExpression();
                if (!self.check(.right_paren)) {
                    try self.addError(.unclosed_paren, paren_start, self.currentStart());
                }
                try self.expectToken(.right_paren, .expected_right_paren);
                break :blk inner;
            },
            else => {
                try self.addErrorWithFound(.expected_expression, token.kind, token.start, token.start + token.len);
                return error.UnexpectedToken;
            },
        };

        // postfix: field access, dereference, postfix call, struct literal, and array index
        while (self.check(.dot) or self.check(.caret) or self.check(.left_paren) or self.check(.left_curly) or self.check(.left_bracket)) {
            if (self.check(.dot)) {
                const start_pos = self.ast.getLocation(expr).start;
                self.advance(); // consume dot
                // Allow .0, .1 for tuple field access (number after dot)
                const field = if (self.check(.number))
                    try self.parseLiteral() // creates a literal node with the number token
                else
                    try self.parseIdentifier();
                const end_pos = self.previousEnd();
                expr = try self.ast.addFieldAccess(expr, field, start_pos, end_pos);
            } else if (self.check(.caret)) {
                const start_pos = self.ast.getLocation(expr).start;
                self.advance(); // consume ^
                const end_pos = self.previousEnd();
                expr = try self.ast.addDeref(expr, start_pos, end_pos);
            } else if (self.check(.left_paren)) {
                // postfix call: expr(args...)
                const start_pos = self.ast.getLocation(expr).start;
                const paren_start = self.currentStart();
                self.advance(); // consume (

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
                expr = try self.ast.addCallExpr(expr, args_range, start_pos, end_pos);
            } else if (self.check(.left_bracket)) {
                // array index: expr[i], or slice: expr[s..e], expr[s..], expr[..e], expr[..]
                const start_pos = self.ast.getLocation(expr).start;
                self.advance(); // consume [
                if (self.check(.dot_dot)) {
                    // arr[..end] or arr[..]
                    self.advance(); // consume ..
                    if (self.check(.right_bracket)) {
                        self.advance(); // consume ]
                        const end_pos = self.previousEnd();
                        expr = try self.ast.addArraySlice(expr, null, null, start_pos, end_pos);
                    } else {
                        const range_end = try self.parseExpression();
                        try self.expectToken(.right_bracket, .unexpected_token);
                        const end_pos = self.previousEnd();
                        expr = try self.ast.addArraySlice(expr, null, range_end, start_pos, end_pos);
                    }
                } else {
                    const first = try self.parseExpression();
                    if (self.check(.dot_dot)) {
                        // arr[start..end] or arr[start..]
                        self.advance(); // consume ..
                        if (self.check(.right_bracket)) {
                            self.advance(); // consume ]
                            const end_pos = self.previousEnd();
                            expr = try self.ast.addArraySlice(expr, first, null, start_pos, end_pos);
                        } else {
                            const range_end = try self.parseExpression();
                            try self.expectToken(.right_bracket, .unexpected_token);
                            const end_pos = self.previousEnd();
                            expr = try self.ast.addArraySlice(expr, first, range_end, start_pos, end_pos);
                        }
                    } else {
                        // arr[index]
                        try self.expectToken(.right_bracket, .unexpected_token);
                        const end_pos = self.previousEnd();
                        expr = try self.ast.addArrayIndex(expr, first, start_pos, end_pos);
                    }
                }
            } else if (self.check(.left_curly)) {
                // struct literal: expr { .field = value, ... }
                if (self.peekOffset(1)) |after_curly| {
                    if (after_curly.kind == .dot or after_curly.kind == .right_curly) {
                        expr = try self.parseStructLiteralWithType(expr);
                        continue;
                    }
                }
                break; // not a struct literal, exit postfix loop
            }
        }

        return expr;
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

    fn parseType(self: *Parser) ParseError!NodeIndex {
        if (self.check(.at)) {
            const start_pos = self.currentStart();
            self.advance(); // consume @
            const is_mutable = self.match(.mut);
            const inner = try self.parseType(); // recursive for @@T
            const end_pos = self.previousEnd();
            return try self.ast.addPointerType(inner, is_mutable, false, start_pos, end_pos);
        }
        if (self.check(.star)) {
            const start_pos = self.currentStart();
            self.advance(); // consume *
            const is_mutable = self.match(.mut);
            const inner = try self.parseType(); // recursive for **T
            const end_pos = self.previousEnd();
            return try self.ast.addPointerType(inner, is_mutable, true, start_pos, end_pos);
        }
        if (self.check(.left_bracket)) {
            return try self.parseArrayOrSliceType();
        }
        return try self.parseIdentifier();
    }

    fn parseArrayOrSliceType(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // consume [
        try self.expectToken(.left_bracket, .unexpected_token);

        // check for slice type: []T or sentinel slice: [:S]T
        if (self.check(.right_bracket)) {
            self.advance(); // consume ]
            const is_mutable = self.match(.mut);
            const element_type = try self.parseType();
            const end_pos = self.previousEnd();
            return try self.ast.addSliceType(element_type, is_mutable, null, start_pos, end_pos);
        }

        if (self.check(.colon)) {
            // sentinel-terminated slice: [:S]T
            self.advance(); // consume :
            const sentinel = try self.parseSentinelValue();
            try self.expectToken(.right_bracket, .unexpected_token);
            const is_mutable = self.match(.mut);
            const element_type = try self.parseType();
            const end_pos = self.previousEnd();
            return try self.ast.addSliceType(element_type, is_mutable, sentinel, start_pos, end_pos);
        }

        // parse length (number literal, or '_' for inferred)
        const length_token = self.peek() orelse {
            try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
            return error.UnexpectedEof;
        };

        var length: ?u32 = null;

        if (length_token.kind == .identifier) {
            const text = self.src.getSlice(length_token.start, length_token.start + length_token.len);
            if (std.mem.eql(u8, text, "_")) {
                self.advance(); // consume _
            } else {
                try self.addErrorWithFound(.unexpected_token, length_token.kind, length_token.start, length_token.start + length_token.len);
                return error.UnexpectedToken;
            }
        } else if (length_token.kind == .number) {
            const length_str = self.src.getSlice(length_token.start, length_token.start + length_token.len);
            length = std.fmt.parseInt(u32, length_str, 10) catch {
                try self.addError(.unexpected_token, length_token.start, length_token.start + length_token.len);
                return error.UnexpectedToken;
            };
            self.advance(); // consume number
        } else {
            try self.addErrorWithFound(.unexpected_token, length_token.kind, length_token.start, length_token.start + length_token.len);
            return error.UnexpectedToken;
        }

        // check for sentinel: [N:S]T or [_:S]T
        var sentinel: ?u8 = null;
        if (self.check(.colon)) {
            self.advance(); // consume :
            sentinel = try self.parseSentinelValue();
        }

        // consume ]
        try self.expectToken(.right_bracket, .unexpected_token);

        // check for 'mut' keyword (mutable elements)
        const is_mutable = self.match(.mut);

        // parse element type
        const element_type = try self.parseType();

        const end_pos = self.previousEnd();

        return try self.ast.addArrayType(element_type, length, is_mutable, sentinel, start_pos, end_pos);
    }

    fn parseSentinelValue(self: *Parser) ParseError!u8 {
        const token = self.peek() orelse {
            try self.addError(.unexpected_eof, self.currentStart(), self.currentStart());
            return error.UnexpectedEof;
        };
        if (token.kind != .number) {
            try self.addErrorWithFound(.unexpected_token, token.kind, token.start, token.start + token.len);
            return error.UnexpectedToken;
        }
        const value_str = self.src.getSlice(token.start, token.start + token.len);
        const value = std.fmt.parseInt(u8, value_str, 10) catch {
            try self.addError(.unexpected_token, token.start, token.start + token.len);
            return error.UnexpectedToken;
        };
        self.advance(); // consume number
        return value;
    }

    fn parseArrayLiteral(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // consume [
        try self.expectToken(.left_bracket, .unexpected_token);

        var elements = try std.ArrayList(ast.NodeIndex).initCapacity(self.allocator, 4);
        defer elements.deinit(self.allocator);

        while (!self.check(.right_bracket) and !self.check(.eof)) {
            const elem = try self.parseExpression();
            try elements.append(self.allocator, elem);
            if (!self.match(.comma)) break;
        }

        try self.expectToken(.right_bracket, .unexpected_token);

        const elements_range = try self.ast.addExtra(elements.items);
        const end_pos = self.previousEnd();

        return try self.ast.addArrayLiteral(elements_range, start_pos, end_pos);
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

    fn parseStructLiteral(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // parse type name identifier
        const type_name = try self.parseIdentifier();

        // expect {
        try self.expectToken(.left_curly, .expected_left_curly);

        // parse .field = value pairs
        var fields = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 4);
        defer fields.deinit(self.allocator);

        while (!self.check(.right_curly) and !self.check(.eof)) {
            // expect .
            try self.expectToken(.dot, .unexpected_token);

            // parse field name
            const field_name = try self.parseIdentifier();
            try fields.append(self.allocator, field_name);

            // expect =
            try self.expectToken(.equal, .expected_equal);

            // parse value expression
            const value = try self.parseExpression();
            try fields.append(self.allocator, value);

            // optional comma
            if (!self.match(.comma)) break;
        }

        // expect }
        try self.expectToken(.right_curly, .expected_right_curly);

        const fields_range = try self.ast.addExtra(fields.items);
        const end_pos = self.previousEnd();

        return try self.ast.addStructLiteral(type_name, fields_range, start_pos, end_pos);
    }

    /// Parse a struct literal when the type name has already been parsed (e.g. as a field_access).
    fn parseStructLiteralWithType(self: *Parser, type_name: NodeIndex) ParseError!NodeIndex {
        const start_pos = self.ast.getLocation(type_name).start;

        // expect {
        try self.expectToken(.left_curly, .expected_left_curly);

        // parse .field = value pairs
        var fields = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 4);
        defer fields.deinit(self.allocator);

        while (!self.check(.right_curly) and !self.check(.eof)) {
            // expect .
            try self.expectToken(.dot, .unexpected_token);

            // parse field name
            const field_name = try self.parseIdentifier();
            try fields.append(self.allocator, field_name);

            // expect =
            try self.expectToken(.equal, .expected_equal);

            // parse value expression
            const value = try self.parseExpression();
            try fields.append(self.allocator, value);

            // optional comma
            if (!self.match(.comma)) break;
        }

        // expect }
        try self.expectToken(.right_curly, .expected_right_curly);

        const fields_range = try self.ast.addExtra(fields.items);
        const end_pos = self.previousEnd();

        return try self.ast.addStructLiteral(type_name, fields_range, start_pos, end_pos);
    }

    /// Parse a tuple literal when the type name has already been parsed: TypeName { val1, val2 }
    fn parseTupleLiteralWithType(self: *Parser, type_name: NodeIndex) ParseError!NodeIndex {
        const start_pos = self.ast.getLocation(type_name).start;

        // expect {
        try self.expectToken(.left_curly, .expected_left_curly);

        // parse positional values with sentinel name nodes
        var fields = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 4);
        defer fields.deinit(self.allocator);

        while (!self.check(.right_curly) and !self.check(.eof)) {
            // sentinel name node — semantic analysis maps to "0", "1", etc.
            try fields.append(self.allocator, std.math.maxInt(NodeIndex));

            // parse value expression
            const value = try self.parseExpression();
            try fields.append(self.allocator, value);

            // optional comma
            if (!self.match(.comma)) break;
        }

        // expect }
        try self.expectToken(.right_curly, .expected_right_curly);

        const fields_range = try self.ast.addExtra(fields.items);
        const end_pos = self.previousEnd();

        return try self.ast.addStructLiteral(type_name, fields_range, start_pos, end_pos);
    }

    /// Parse an anonymous tuple literal: { expr, expr, ... }
    /// Uses maxInt(NodeIndex) for type_name to indicate anonymous (type inferred from elements).
    fn parseAnonymousTupleLiteral(self: *Parser) ParseError!NodeIndex {
        const start_pos = self.currentStart();

        // expect {
        try self.expectToken(.left_curly, .expected_left_curly);

        var fields = try std.ArrayList(NodeIndex).initCapacity(self.allocator, 4);
        defer fields.deinit(self.allocator);

        while (!self.check(.right_curly) and !self.check(.eof)) {
            // sentinel name node for positional field
            try fields.append(self.allocator, std.math.maxInt(NodeIndex));

            // parse value expression
            const value = try self.parseExpression();
            try fields.append(self.allocator, value);

            // optional comma
            if (!self.match(.comma)) break;
        }

        // expect }
        try self.expectToken(.right_curly, .expected_right_curly);

        const fields_range = try self.ast.addExtra(fields.items);
        const end_pos = self.previousEnd();

        // Use maxInt for type_name to indicate anonymous tuple
        return try self.ast.addStructLiteral(std.math.maxInt(NodeIndex), fields_range, start_pos, end_pos);
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

    /// Get the source text for a token at the given offset from current position.
    fn getTokenText(self: *const Parser, offset: TokenIndex) ?[]const u8 {
        const token = self.peekOffset(offset) orelse return null;
        return self.src.getSlice(token.start, token.start + token.len);
    }

    /// Check if the token at the given offset is an identifier matching the given text.
    fn isIdentifierText(self: *const Parser, offset: TokenIndex, text: []const u8) bool {
        const token = self.peekOffset(offset) orelse return false;
        if (token.kind != .identifier) return false;
        const token_text = self.src.getSlice(token.start, token.start + token.len);
        return mem.eql(u8, token_text, text);
    }

    /// Check if the token at the given offset is a calling convention identifier.
    fn isCallingConvention(self: *const Parser, offset: TokenIndex) bool {
        return self.isIdentifierText(offset, "c") or
            self.isIdentifierText(offset, "cobol") or
            self.isIdentifierText(offset, "fortran");
    }

    /// Try to match and consume a calling convention identifier. Returns the convention if matched.
    fn matchCallingConvention(self: *Parser) ast.CallingConvention {
        const token = self.peek() orelse return .honey;
        if (token.kind != .identifier) return .honey;

        const text = self.src.getSlice(token.start, token.start + token.len);
        if (mem.eql(u8, text, "c")) {
            self.advance();
            return .c;
        } else if (mem.eql(u8, text, "cobol")) {
            self.advance();
            return .cobol;
        } else if (mem.eql(u8, text, "fortran")) {
            self.advance();
            return .fortran;
        }
        return .honey;
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

            // also stop at 'mut' which starts a var decl, or 'import'
            if (token.kind == .mut or token.kind == .import) return;

            self.advance();
        }
    }

    fn synchronizeStatement(self: *Parser) void {
        // skip tokens until we find what looks like a new statement
        while (self.peek()) |token| {
            if (token.kind == .eof) return;
            if (token.kind == .right_curly) return;

            // statement starters
            if (token.kind == .@"return" or
                token.kind == .@"defer" or
                token.kind == .@"if" or
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
