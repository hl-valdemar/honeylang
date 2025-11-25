const std = @import("std");
const Ast = @import("ast.zig").Ast;
const NodeIndex = @import("ast.zig").NodeIndex;
const NodeKind = @import("ast.zig").NodeKind;
const Token = @import("../lexer/token.zig").Token;
const TokenList = @import("../lexer/token.zig").TokenList;
const SourceCode = @import("../source/source.zig").SourceCode;

pub fn print(ast: *const Ast, tokens: *const TokenList, src: *const SourceCode) void {
    const root = ast.root;
    printNode(ast, tokens, src, root, "", true);
}

// don't bother freeing the strings as this will only be called in debug
// and the memory will be freed on program exit anyways
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

    // calculate child prefix
    const child_prefix = if (prefix.len == 0)
        " " // root's children start at position 1 (second character)
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
            std.debug.print("declaration (const):\n", .{});
            const decl = ast.getConstDecl(idx);

            std.debug.print("{s}├─ name: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, decl.name);
            std.debug.print("\n", .{});

            if (decl.type_node) |type_idx| {
                std.debug.print("{s}├─ type: ", .{child_prefix});
                printIdentifierValue(ast, tokens, src, type_idx);
                std.debug.print("\n", .{});
            } else {
                std.debug.print("{s}├─ type: <unresolved>\n", .{child_prefix});
            }

            std.debug.print("{s}└─ value:\n", .{child_prefix});
            const value_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}   ", .{child_prefix}) catch unreachable;
            printNode(ast, tokens, src, decl.value, value_prefix, true);
        },

        .func_decl => {
            std.debug.print("declaration (func):\n", .{});
            const decl = ast.getFuncDecl(idx);

            std.debug.print("{s}├─ name: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, decl.name);
            std.debug.print("\n", .{});

            if (decl.return_type) |ret_type| {
                std.debug.print("{s}├─ type: ", .{child_prefix});
                printIdentifierValue(ast, tokens, src, ret_type);
                std.debug.print("\n", .{});
            } else {
                std.debug.print("{s}├─ type: void\n", .{child_prefix});
            }

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

            std.debug.print("{s}└─ block:\n", .{child_prefix});
            const body_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}   ", .{child_prefix}) catch unreachable;
            const block = ast.getBlock(decl.body);

            const stmts = ast.getExtra(block.statements);
            const defers = ast.getExtra(block.deferred);

            std.debug.print("{s}├─ statements: {d}\n", .{ body_prefix, stmts.len });
            if (stmts.len > 0) {
                const stmt_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}│  ", .{body_prefix}) catch unreachable;
                for (stmts, 0..) |stmt, j| {
                    const is_last_stmt = (j == stmts.len - 1);
                    printNode(ast, tokens, src, stmt, stmt_prefix, is_last_stmt);
                }
            }

            std.debug.print("{s}└─ deferred: {d}\n", .{ body_prefix, defers.len });
            if (defers.len > 0) {
                const defer_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}   ", .{body_prefix}) catch unreachable;
                for (defers, 0..) |def, j| {
                    const is_last_defer = (j == defers.len - 1);
                    printNode(ast, tokens, src, def, defer_prefix, is_last_defer);
                }
            }
        },

        .var_decl => {
            std.debug.print("var decl:\n", .{});
            const decl = ast.getVarDecl(idx);

            std.debug.print("{s}├─ name: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, decl.name);
            std.debug.print("\n", .{});

            if (decl.type_node) |type_idx| {
                std.debug.print("{s}├─ type: ", .{child_prefix});
                printIdentifierValue(ast, tokens, src, type_idx);
                std.debug.print("\n", .{});
            } else {
                std.debug.print("{s}├─ type: <unresolved>\n", .{child_prefix});
            }

            std.debug.print("{s}├─ mutable: {}\n", .{ child_prefix, decl.is_mutable });

            std.debug.print("{s}└─ value:\n", .{child_prefix});
            const value_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}   ", .{child_prefix}) catch unreachable;
            printNode(ast, tokens, src, decl.value, value_prefix, true);
        },

        .binary_op => {
            const op = ast.getBinaryOp(idx);
            std.debug.print("binary op: {s}\n", .{@tagName(op.op)});

            printNode(ast, tokens, src, op.left, child_prefix, false);
            printNode(ast, tokens, src, op.right, child_prefix, true);
        },

        .unary_op => {
            const op = ast.getUnaryOp(idx);
            std.debug.print("unary op: {s}\n", .{@tagName(op.op)});

            printNode(ast, tokens, src, op.operand, child_prefix, true);
        },

        .call_expr => {
            std.debug.print("call expr:\n", .{});
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
                const stmt_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}│  ", .{child_prefix}) catch unreachable;
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

        .assignment => {
            std.debug.print("assignment:\n", .{});
            const assign = ast.getAssignment(idx);

            std.debug.print("{s}├─ target: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, assign.target);
            std.debug.print("\n", .{});

            std.debug.print("{s}└─ value:\n", .{child_prefix});
            const value_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}   ", .{child_prefix}) catch unreachable;
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
