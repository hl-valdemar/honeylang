const std = @import("std");
const Ast = @import("ast.zig").Ast;
const NodeIndex = @import("ast.zig").NodeIndex;
const NodeKind = @import("ast.zig").NodeKind;
const Token = @import("../lexer/token.zig").Token;
const TokenList = @import("../lexer/token.zig").TokenList;
const SourceCode = @import("../source/source.zig").SourceCode;

pub fn print(ast: *const Ast, tokens: *const TokenList, src: *const SourceCode) void {
    printSummary(ast, tokens, src);
    std.debug.print("\n", .{});
    printTree(ast, tokens, src);
}

pub fn printSummary(ast: *const Ast, tokens: *const TokenList, src: *const SourceCode) void {
    const count = ast.nodeCount();

    if (count == 0) {
        std.debug.print("(no nodes)\n", .{});
        return;
    }

    // print header
    std.debug.print("{s:<5} {s:<14} {s:<12} {s}\n", .{
        "idx",
        "kind",
        "location",
        "info",
    });
    std.debug.print("{s:-<5} {s:-<14} {s:-<12} {s:-<24}\n", .{
        "",
        "",
        "",
        "",
    });

    // print each node
    for (0..count) |i| {
        const idx: NodeIndex = @intCast(i);
        printNodeSummary(ast, tokens, src, idx);
    }
}

fn printNodeSummary(ast: *const Ast, tokens: *const TokenList, src: *const SourceCode, idx: NodeIndex) void {
    const kind = ast.getKind(idx);
    const loc = ast.getLocation(idx);
    const kind_str = @tagName(kind);

    // format location
    var loc_buf: [12]u8 = undefined;
    const loc_str = std.fmt.bufPrint(&loc_buf, "{d}..{d}", .{ loc.start, loc.end }) catch "?";

    // get additional info based on node kind
    const info = getNodeInfo(ast, tokens, src, idx, kind);

    std.debug.print("{d:<5} {s:<14} {s:<12} {s}\n", .{
        idx,
        kind_str,
        loc_str,
        info,
    });
}

fn getNodeInfo(
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    idx: NodeIndex,
    kind: NodeKind,
) []const u8 {
    // static buffer for formatting - reused each call
    const S = struct {
        var buf: [64]u8 = undefined;
    };

    return switch (kind) {
        .program => blk: {
            const prog = ast.getProgram(idx);
            break :blk std.fmt.bufPrint(&S.buf, "decls: {d}", .{prog.declarations.len}) catch "?";
        },
        .const_decl => blk: {
            const decl = ast.getConstDecl(idx);
            const name = getIdentifierName(ast, tokens, src, decl.name);
            break :blk std.fmt.bufPrint(&S.buf, "name: {s}", .{name}) catch "?";
        },
        .func_decl => blk: {
            const decl = ast.getFuncDecl(idx);
            const name = getIdentifierName(ast, tokens, src, decl.name);
            const param_count = decl.params.len / 2; // params stored as name, type pairs
            break :blk std.fmt.bufPrint(&S.buf, "name: {s}, params: {d}", .{ name, param_count }) catch "?";
        },
        .var_decl => blk: {
            const decl = ast.getVarDecl(idx);
            const name = getIdentifierName(ast, tokens, src, decl.name);
            const mut_str = if (decl.is_mutable) "mut" else "immut";
            break :blk std.fmt.bufPrint(&S.buf, "name: {s}, {s}", .{ name, mut_str }) catch "?";
        },
        .identifier => blk: {
            const name = getIdentifierName(ast, tokens, src, idx);
            break :blk std.fmt.bufPrint(&S.buf, "\"{s}\"", .{name}) catch "?";
        },
        .literal => blk: {
            const lit = ast.getLiteral(idx);
            const token = tokens.items[lit.token_idx];
            const value = src.getSlice(token.start, token.start + token.len);
            break :blk std.fmt.bufPrint(&S.buf, "\"{s}\"", .{value}) catch "?";
        },
        .binary_op => blk: {
            const op = ast.getBinaryOp(idx);
            break :blk std.fmt.bufPrint(&S.buf, "op: {s}", .{@tagName(op.op)}) catch "?";
        },
        .unary_op => blk: {
            const op = ast.getUnaryOp(idx);
            break :blk std.fmt.bufPrint(&S.buf, "op: {s}", .{@tagName(op.op)}) catch "?";
        },
        .block => blk: {
            const block = ast.getBlock(idx);
            break :blk std.fmt.bufPrint(&S.buf, "stmts: {d}, deferred: {d}", .{
                block.statements.len,
                block.deferred.len,
            }) catch "?";
        },
        .return_stmt => "return",
        .defer_stmt => "defer",
        .if_stmt => "if",
        .assignment => blk: {
            const assign = ast.getAssignment(idx);
            const name = getIdentifierName(ast, tokens, src, assign.target);
            break :blk std.fmt.bufPrint(&S.buf, "target: {s}", .{name}) catch "?";
        },
        .call_expr => blk: {
            const call = ast.getCallExpr(idx);
            const name = getIdentifierName(ast, tokens, src, call.func);
            break :blk std.fmt.bufPrint(&S.buf, "func: {s}, args: {d}", .{ name, call.args.len }) catch "?";
        },
        .err => blk: {
            const err = ast.getError(idx);
            break :blk std.fmt.bufPrint(&S.buf, "\"{s}\"", .{err.msg}) catch "?";
        },
    };
}

fn getIdentifierName(ast: *const Ast, tokens: *const TokenList, src: *const SourceCode, idx: NodeIndex) []const u8 {
    const ident = ast.getIdentifier(idx);
    const token = tokens.items[ident.token_idx];
    return src.getSlice(token.start, token.start + token.len);
}

pub fn printTree(ast: *const Ast, tokens: *const TokenList, src: *const SourceCode) void {
    const root = ast.root;
    printNode(ast, tokens, src, root, "", true);
}

fn printNode(
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    idx: NodeIndex,
    prefix: []const u8,
    is_last: bool,
) void {
    // print the connector (unless we're at root)
    if (prefix.len > 0) {
        std.debug.print("{s}", .{prefix});
        if (is_last) {
            std.debug.print("└─ ", .{});
        } else {
            std.debug.print("├─ ", .{});
        }
    }

    const kind = ast.getKind(idx);

    // calculate child prefix (don't bother freeing - debug only)
    const child_prefix = if (prefix.len == 0)
        " "
    else if (is_last)
        std.fmt.allocPrint(std.heap.page_allocator, "{s}    ", .{prefix}) catch unreachable
    else
        std.fmt.allocPrint(std.heap.page_allocator, "{s}│   ", .{prefix}) catch unreachable;

    switch (kind) {
        .program => {
            std.debug.print("program:\n", .{});
            const prog = ast.getProgram(idx);
            const decls = ast.getExtra(prog.declarations);

            for (decls, 0..) |child, i| {
                const is_last_child = (i == decls.len - 1);
                printNode(ast, tokens, src, child, child_prefix, is_last_child);
            }
        },

        .const_decl => {
            std.debug.print("const_decl:\n", .{});
            const decl = ast.getConstDecl(idx);

            std.debug.print("{s}├─ name: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, decl.name);
            std.debug.print("\n", .{});

            if (decl.type_id) |type_idx| {
                std.debug.print("{s}├─ type: ", .{child_prefix});
                printIdentifierValue(ast, tokens, src, type_idx);
                std.debug.print("\n", .{});
            } else {
                std.debug.print("{s}├─ type: <inferred>\n", .{child_prefix});
            }

            std.debug.print("{s}└─ value:\n", .{child_prefix});
            const value_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}    ", .{child_prefix}) catch unreachable;
            printNode(ast, tokens, src, decl.value, value_prefix, true);
        },

        .func_decl => {
            std.debug.print("func_decl:\n", .{});
            const decl = ast.getFuncDecl(idx);

            std.debug.print("{s}├─ name: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, decl.name);
            std.debug.print("\n", .{});

            std.debug.print("{s}├─ return: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, decl.return_type);
            std.debug.print("\n", .{});

            // print parameters
            const params = ast.getExtra(decl.params);
            std.debug.print("{s}├─ params: {d}\n", .{ child_prefix, params.len / 2 });

            if (params.len > 0) {
                const param_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}│  ", .{child_prefix}) catch unreachable;

                var i: usize = 0;
                while (i < params.len) : (i += 2) {
                    const param_name = params[i];
                    const param_type = params[i + 1];
                    const is_last_param = (i + 2 >= params.len);

                    std.debug.print("{s}", .{param_prefix});
                    if (is_last_param) {
                        std.debug.print("└─ ", .{});
                    } else {
                        std.debug.print("├─ ", .{});
                    }

                    printIdentifierValue(ast, tokens, src, param_name);
                    std.debug.print(": ", .{});
                    printIdentifierValue(ast, tokens, src, param_type);
                    std.debug.print("\n", .{});
                }
            }

            std.debug.print("{s}└─ body:\n", .{child_prefix});
            const body_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}    ", .{child_prefix}) catch unreachable;
            printNode(ast, tokens, src, decl.body, body_prefix, true);
        },

        .var_decl => {
            std.debug.print("var_decl:\n", .{});
            const decl = ast.getVarDecl(idx);

            std.debug.print("{s}├─ name: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, decl.name);
            std.debug.print("\n", .{});

            if (decl.type_id) |type_idx| {
                std.debug.print("{s}├─ type: ", .{child_prefix});
                printIdentifierValue(ast, tokens, src, type_idx);
                std.debug.print("\n", .{});
            } else {
                std.debug.print("{s}├─ type: <inferred>\n", .{child_prefix});
            }

            std.debug.print("{s}├─ mutable: {}\n", .{ child_prefix, decl.is_mutable });

            std.debug.print("{s}└─ value:\n", .{child_prefix});
            const value_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}    ", .{child_prefix}) catch unreachable;
            printNode(ast, tokens, src, decl.value, value_prefix, true);
        },

        .binary_op => {
            const op = ast.getBinaryOp(idx);
            std.debug.print("binary_op: {s}\n", .{@tagName(op.op)});

            printNode(ast, tokens, src, op.left, child_prefix, false);
            printNode(ast, tokens, src, op.right, child_prefix, true);
        },

        .unary_op => {
            const op = ast.getUnaryOp(idx);
            std.debug.print("unary_op: {s}\n", .{@tagName(op.op)});

            printNode(ast, tokens, src, op.operand, child_prefix, true);
        },

        .call_expr => {
            std.debug.print("call_expr:\n", .{});
            const call = ast.getCallExpr(idx);

            std.debug.print("{s}├─ func: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, call.func);
            std.debug.print("\n", .{});

            const args = ast.getExtra(call.args);
            std.debug.print("{s}└─ args: {d}\n", .{ child_prefix, args.len });

            if (args.len > 0) {
                const arg_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}   ", .{child_prefix}) catch unreachable;
                for (args, 0..) |arg, i| {
                    const is_last_arg = (i == args.len - 1);
                    printNode(ast, tokens, src, arg, arg_prefix, is_last_arg);
                }
            }
        },

        .identifier => {
            std.debug.print("identifier: ", .{});
            printIdentifierValue(ast, tokens, src, idx);
            std.debug.print("\n", .{});
        },

        .literal => {
            std.debug.print("literal: ", .{});
            printLiteralValue(ast, tokens, src, idx);
            std.debug.print("\n", .{});
        },

        .block => {
            std.debug.print("block:\n", .{});
            const block = ast.getBlock(idx);

            const stmts = ast.getExtra(block.statements);
            const defers = ast.getExtra(block.deferred);

            std.debug.print("{s}├─ statements: {d}\n", .{ child_prefix, stmts.len });
            if (stmts.len > 0) {
                const stmt_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}│   ", .{child_prefix}) catch unreachable;
                for (stmts, 0..) |stmt, i| {
                    const is_last_stmt = (i == stmts.len - 1);
                    printNode(ast, tokens, src, stmt, stmt_prefix, is_last_stmt);
                }
            }

            std.debug.print("{s}└─ deferred: {d}\n", .{ child_prefix, defers.len });
            if (defers.len > 0) {
                const defer_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}   ", .{child_prefix}) catch unreachable;
                for (defers, 0..) |def, i| {
                    const is_last_defer = (i == defers.len - 1);
                    printNode(ast, tokens, src, def, defer_prefix, is_last_defer);
                }
            }
        },

        .return_stmt => {
            std.debug.print("return:\n", .{});
            const ret = ast.getReturn(idx);

            printNode(ast, tokens, src, ret.expr, child_prefix, true);
        },

        .defer_stmt => {
            std.debug.print("defer:\n", .{});
            const def = ast.getDefer(idx);

            printNode(ast, tokens, src, def.stmt, child_prefix, true);
        },

        .if_stmt => {
            const if_ = ast.getIf(idx);
            std.debug.print("if:\n", .{});

            std.debug.print("{s}├─ guard:\n", .{child_prefix});
            const if_guard_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}│   ", .{child_prefix}) catch unreachable;
            printNode(ast, tokens, src, if_.if_guard, if_guard_prefix, true);

            const has_else_ifs = if_.elseIfCount() > 0;
            const has_else = if_.else_block != null;

            printNode(ast, tokens, src, if_.if_block, child_prefix, !has_else_ifs and !has_else);

            for (0..if_.elseIfCount()) |i| {
                const pair = if_.getElseIf(ast, i) orelse unreachable;
                const is_last_else_if = i == if_.elseIfCount() - 1;
                const is_last_inner = is_last_else_if and !has_else;

                if (is_last_inner) {
                    std.debug.print("{s}└─ else if:\n", .{child_prefix});
                } else {
                    std.debug.print("{s}├─ else if:\n", .{child_prefix});
                }

                const blk_prefix = if (is_last_inner)
                    std.fmt.allocPrint(std.heap.page_allocator, "{s}    ", .{child_prefix}) catch unreachable
                else
                    std.fmt.allocPrint(std.heap.page_allocator, "{s}│   ", .{child_prefix}) catch unreachable;

                std.debug.print("{s}├─ guard:\n", .{blk_prefix});
                const blk_child_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}│   ", .{blk_prefix}) catch unreachable;
                printNode(ast, tokens, src, pair.guard, blk_child_prefix, true);
                printNode(ast, tokens, src, pair.block, blk_prefix, true);
            }

            if (if_.else_block) |blk| {
                std.debug.print("{s}└─ else:\n", .{child_prefix});
                const else_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}    ", .{child_prefix}) catch unreachable;
                printNode(ast, tokens, src, blk, else_prefix, true);
            }
        },

        .assignment => {
            std.debug.print("assignment:\n", .{});
            const assign = ast.getAssignment(idx);

            std.debug.print("{s}├─ target: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, assign.target);
            std.debug.print("\n", .{});

            std.debug.print("{s}└─ value:\n", .{child_prefix});
            const value_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}    ", .{child_prefix}) catch unreachable;
            printNode(ast, tokens, src, assign.value, value_prefix, true);
        },

        .err => {
            const err_data = ast.getError(idx);
            std.debug.print("error: {s}\n", .{err_data.msg});
        },
    }
}

fn printIdentifierValue(
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    idx: NodeIndex,
) void {
    const ident = ast.getIdentifier(idx);
    const token = tokens.items[ident.token_idx];
    const value = src.getSlice(token.start, token.start + token.len);
    std.debug.print("{s}", .{value});
}

fn printLiteralValue(
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    idx: NodeIndex,
) void {
    const lit = ast.getLiteral(idx);
    const token = tokens.items[lit.token_idx];
    const value = src.getSlice(token.start, token.start + token.len);
    std.debug.print("{s}", .{value});
}
