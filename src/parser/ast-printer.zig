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
        .struct_decl => blk: {
            const decl = ast.getStructDecl(idx);
            const name = getIdentifierName(ast, tokens, src, decl.name);
            const field_count = decl.fields.len / 2;
            const cc_str = if (decl.call_conv == .c)
                "c "
            else if (decl.call_conv == .cobol)
                "cobol "
            else if (decl.call_conv == .fortran)
                "fortran "
            else
                "";
            break :blk std.fmt.bufPrint(&S.buf, "{s}struct {s}, fields: {d}", .{ cc_str, name, field_count }) catch "?";
        },
        .func_decl => blk: {
            const decl = ast.getFuncDecl(idx);
            const name = getIdentifierName(ast, tokens, src, decl.name);
            const param_count = decl.params.len / 2; // params stored as (name, type) pairs
            const cc_str = if (decl.call_conv == .c)
                "c "
            else if (decl.call_conv == .cobol)
                "cobol "
            else if (decl.call_conv == .fortran)
                "fortran "
            else
                "";
            const extern_str = if (decl.body == null) " (extern)" else "";
            break :blk std.fmt.bufPrint(&S.buf, "{s}func {s}, params: {d}{s}", .{ cc_str, name, param_count, extern_str }) catch "?";
        },
        .var_decl => blk: {
            const decl = ast.getVarDecl(idx);
            const name = getIdentifierName(ast, tokens, src, decl.name);
            const mut_str = if (decl.is_mutable) "mut" else "immut";
            break :blk std.fmt.bufPrint(&S.buf, "name: {s}, {s}", .{ name, mut_str }) catch "?";
        },
        .namespace_decl => blk: {
            const decl = ast.getNamespaceDecl(idx);
            const name = getIdentifierName(ast, tokens, src, decl.name);
            const member_count = decl.declarations.len;
            break :blk std.fmt.bufPrint(&S.buf, "namespace {s}, members: {d}", .{ name, member_count }) catch "?";
        },
        .pub_decl => "pub",
        .import_decl => blk: {
            const decl = ast.getImportDecl(idx);
            const token = tokens.items[decl.path_token];
            const path = src.getSlice(token.start, token.start + token.len);
            break :blk std.fmt.bufPrint(&S.buf, "import \"{s}\"", .{path}) catch "?";
        },
        .c_include_decl => blk: {
            const decl = ast.getImportDecl(idx);
            const token = tokens.items[decl.path_token];
            const path = src.getSlice(token.start, token.start + token.len);
            break :blk std.fmt.bufPrint(&S.buf, "import c include \"{s}\"", .{path}) catch "?";
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
            if (ast.getKind(assign.target) == .field_access) {
                break :blk std.fmt.bufPrint(&S.buf, "target: {s}", .{
                    getFieldAccessPath(ast, tokens, src, assign.target),
                }) catch "?";
            }
            if (ast.getKind(assign.target) == .deref) {
                break :blk "target: deref";
            }
            const name = getIdentifierName(ast, tokens, src, assign.target);
            break :blk std.fmt.bufPrint(&S.buf, "target: {s}", .{name}) catch "?";
        },
        .call_expr => blk: {
            const call = ast.getCallExpr(idx);
            if (ast.getKind(call.func) == .identifier) {
                const name = getIdentifierName(ast, tokens, src, call.func);
                break :blk std.fmt.bufPrint(&S.buf, "func: {s}, args: {d}", .{ name, call.args.len }) catch "?";
            } else if (ast.getKind(call.func) == .field_access) {
                const path = getFieldAccessPath(ast, tokens, src, call.func);
                break :blk std.fmt.bufPrint(&S.buf, "func: {s}, args: {d}", .{ path, call.args.len }) catch "?";
            } else {
                break :blk std.fmt.bufPrint(&S.buf, "func: <expr>, args: {d}", .{call.args.len}) catch "?";
            }
        },
        .field_access => blk: {
            const access = ast.getFieldAccess(idx);
            const field_name = getIdentifierName(ast, tokens, src, access.field);
            break :blk std.fmt.bufPrint(&S.buf, ".{s}", .{field_name}) catch "?";
        },
        .struct_literal => blk: {
            const lit = ast.getStructLiteral(idx);
            const name = if (ast.getKind(lit.type_name) == .field_access)
                getIdentifierName(ast, tokens, src, ast.getFieldAccess(lit.type_name).field)
            else
                getIdentifierName(ast, tokens, src, lit.type_name);
            const field_count = lit.fields.len / 2;
            break :blk std.fmt.bufPrint(&S.buf, "{s}{{ fields: {d} }}", .{ name, field_count }) catch "?";
        },
        .address_of => "address_of",
        .deref => "deref",
        .pointer_type => blk: {
            const ptr = ast.getPointerType(idx);
            const prefix = if (ptr.is_many_item) "*" else "@";
            const mut_str = if (ptr.is_mutable) "mut" else "";
            break :blk std.fmt.bufPrint(&S.buf, "{s}{s}", .{ prefix, mut_str }) catch "?";
        },
        .void_literal => "void",
        .err => blk: {
            const err = ast.getError(idx);
            break :blk std.fmt.bufPrint(&S.buf, "\"{s}\"", .{err.msg}) catch "?";
        },
    };
}

fn getFieldAccessPath(ast: *const Ast, tokens: *const TokenList, src: *const SourceCode, idx: NodeIndex) []const u8 {
    const Path = struct {
        var buf: [256]u8 = undefined;
    };
    var pos: usize = 0;

    // collect path segments by walking the chain
    var segments: [16][]const u8 = undefined;
    var count: usize = 0;
    var current = idx;
    while (ast.getKind(current) == .field_access and count < 16) {
        const access = ast.getFieldAccess(current);
        segments[count] = getIdentifierName(ast, tokens, src, access.field);
        count += 1;
        current = access.object;
    }
    // current is the root identifier
    if (ast.getKind(current) == .identifier) {
        const root = getIdentifierName(ast, tokens, src, current);
        if (pos + root.len < Path.buf.len) {
            @memcpy(Path.buf[pos .. pos + root.len], root);
            pos += root.len;
        }
    }
    // append segments in reverse order
    var i = count;
    while (i > 0) {
        i -= 1;
        if (pos + 1 + segments[i].len < Path.buf.len) {
            Path.buf[pos] = '.';
            pos += 1;
            @memcpy(Path.buf[pos .. pos + segments[i].len], segments[i]);
            pos += segments[i].len;
        }
    }
    return Path.buf[0..pos];
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
                printTypeValue(ast, tokens, src, type_idx);
                std.debug.print("\n", .{});
            } else {
                std.debug.print("{s}├─ type: <inferred>\n", .{child_prefix});
            }

            std.debug.print("{s}└─ value:\n", .{child_prefix});
            const value_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}    ", .{child_prefix}) catch unreachable;
            printNode(ast, tokens, src, decl.value, value_prefix, true);
        },

        .struct_decl => {
            const decl = ast.getStructDecl(idx);
            const cc_str = if (decl.call_conv == .c)
                "c "
            else if (decl.call_conv == .cobol)
                "cobol "
            else if (decl.call_conv == .fortran)
                "fortran "
            else
                "";
            std.debug.print("{s}struct_decl:\n", .{cc_str});

            std.debug.print("{s}├─ name: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, decl.name);
            std.debug.print("\n", .{});

            const fields = ast.getExtra(decl.fields);
            const field_count = fields.len / 2;
            std.debug.print("{s}└─ fields: {d}\n", .{ child_prefix, field_count });

            var fi: usize = 0;
            while (fi < fields.len) : (fi += 2) {
                const fname = getIdentifierName(ast, tokens, src, fields[fi]);
                const is_last_field = (fi + 2 >= fields.len);
                const connector: []const u8 = if (is_last_field) "└" else "├";
                std.debug.print("{s}    {s}─ {s}: ", .{ child_prefix, connector, fname });
                printTypeValue(ast, tokens, src, fields[fi + 1]);
                std.debug.print("\n", .{});
            }
        },

        .func_decl => {
            const decl = ast.getFuncDecl(idx);
            const cc_str = if (decl.call_conv == .c)
                "c "
            else if (decl.call_conv == .cobol)
                "cobol "
            else if (decl.call_conv == .fortran)
                "fortran "
            else
                "";
            std.debug.print("{s}func_decl:\n", .{cc_str});

            std.debug.print("{s}├─ name: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, decl.name);
            std.debug.print("\n", .{});

            std.debug.print("{s}├─ convention: {s}\n", .{ child_prefix, @tagName(decl.call_conv) });

            std.debug.print("{s}├─ return: ", .{child_prefix});
            printTypeValue(ast, tokens, src, decl.return_type);
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
                    printTypeValue(ast, tokens, src, param_type);
                    std.debug.print("\n", .{});
                }
            }

            if (decl.body) |body_idx| {
                std.debug.print("{s}└─ body:\n", .{child_prefix});
                const body_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}    ", .{child_prefix}) catch unreachable;
                printNode(ast, tokens, src, body_idx, body_prefix, true);
            } else {
                std.debug.print("{s}└─ body: (external)\n", .{child_prefix});
            }
        },

        .var_decl => {
            std.debug.print("var_decl:\n", .{});
            const decl = ast.getVarDecl(idx);

            std.debug.print("{s}├─ name: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, decl.name);
            std.debug.print("\n", .{});

            if (decl.type_id) |type_idx| {
                std.debug.print("{s}├─ type: ", .{child_prefix});
                printTypeValue(ast, tokens, src, type_idx);
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

            if (ast.getKind(call.func) == .identifier) {
                std.debug.print("{s}├─ func: ", .{child_prefix});
                printIdentifierValue(ast, tokens, src, call.func);
                std.debug.print("\n", .{});
            } else {
                std.debug.print("{s}├─ func:\n", .{child_prefix});
                const func_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}│   ", .{child_prefix}) catch unreachable;
                printNode(ast, tokens, src, call.func, func_prefix, true);
            }

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

        .field_access => {
            const access = ast.getFieldAccess(idx);
            std.debug.print("field_access:\n", .{});

            std.debug.print("{s}├─ object:\n", .{child_prefix});
            const obj_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}│   ", .{child_prefix}) catch unreachable;
            printNode(ast, tokens, src, access.object, obj_prefix, true);

            std.debug.print("{s}└─ field: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, access.field);
            std.debug.print("\n", .{});
        },

        .struct_literal => {
            const lit = ast.getStructLiteral(idx);
            std.debug.print("struct_literal:\n", .{});

            std.debug.print("{s}├─ type: ", .{child_prefix});
            if (ast.getKind(lit.type_name) == .field_access) {
                printNode(ast, tokens, src, lit.type_name, child_prefix, false);
            } else {
                printIdentifierValue(ast, tokens, src, lit.type_name);
                std.debug.print("\n", .{});
            }

            const field_data = ast.getExtra(lit.fields);
            const field_count = field_data.len / 2;
            std.debug.print("{s}└─ fields: {d}\n", .{ child_prefix, field_count });

            var fi: usize = 0;
            while (fi < field_data.len) : (fi += 2) {
                const fname = getIdentifierName(ast, tokens, src, field_data[fi]);
                const is_last_field = (fi + 2 >= field_data.len);
                const connector: []const u8 = if (is_last_field) "└" else "├";
                std.debug.print("{s}    {s}─ .{s} =\n", .{ child_prefix, connector, fname });

                const val_prefix = if (is_last_field)
                    std.fmt.allocPrint(std.heap.page_allocator, "{s}        ", .{child_prefix}) catch unreachable
                else
                    std.fmt.allocPrint(std.heap.page_allocator, "{s}    │   ", .{child_prefix}) catch unreachable;
                printNode(ast, tokens, src, field_data[fi + 1], val_prefix, true);
            }
        },

        .namespace_decl => {
            std.debug.print("namespace_decl:\n", .{});
            const decl = ast.getNamespaceDecl(idx);

            std.debug.print("{s}├─ name: ", .{child_prefix});
            printIdentifierValue(ast, tokens, src, decl.name);
            std.debug.print("\n", .{});

            const members = ast.getExtra(decl.declarations);
            std.debug.print("{s}└─ members: {d}\n", .{ child_prefix, members.len });

            if (members.len > 0) {
                const member_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}    ", .{child_prefix}) catch unreachable;
                for (members, 0..) |member, i| {
                    const is_last_member = (i == members.len - 1);
                    printNode(ast, tokens, src, member, member_prefix, is_last_member);
                }
            }
        },

        .pub_decl => {
            std.debug.print("pub:\n", .{});
            const decl = ast.getPubDecl(idx);
            printNode(ast, tokens, src, decl.inner, child_prefix, true);
        },

        .import_decl => {
            const decl = ast.getImportDecl(idx);
            const token = tokens.items[decl.path_token];
            const path = src.getSlice(token.start, token.start + token.len);
            std.debug.print("import \"{s}\"\n", .{path});
        },

        .c_include_decl => {
            const decl = ast.getImportDecl(idx);
            const token = tokens.items[decl.path_token];
            const path = src.getSlice(token.start, token.start + token.len);
            std.debug.print("import c include \"{s}\"\n", .{path});
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

        .void_literal => {
            std.debug.print("void\n", .{});
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

            if (ast.getKind(assign.target) == .field_access) {
                std.debug.print("{s}├─ target: {s}\n", .{
                    child_prefix,
                    getFieldAccessPath(ast, tokens, src, assign.target),
                });
            } else if (ast.getKind(assign.target) == .deref) {
                std.debug.print("{s}├─ target:\n", .{child_prefix});
                const target_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}│   ", .{child_prefix}) catch unreachable;
                printNode(ast, tokens, src, assign.target, target_prefix, true);
            } else {
                std.debug.print("{s}├─ target: ", .{child_prefix});
                printIdentifierValue(ast, tokens, src, assign.target);
                std.debug.print("\n", .{});
            }

            std.debug.print("{s}└─ value:\n", .{child_prefix});
            const value_prefix = std.fmt.allocPrint(std.heap.page_allocator, "{s}    ", .{child_prefix}) catch unreachable;
            printNode(ast, tokens, src, assign.value, value_prefix, true);
        },

        .address_of => {
            std.debug.print("address_of:\n", .{});
            const addr = ast.getAddressOf(idx);
            printNode(ast, tokens, src, addr.operand, child_prefix, true);
        },

        .deref => {
            std.debug.print("deref:\n", .{});
            const deref_node = ast.getDeref(idx);
            printNode(ast, tokens, src, deref_node.operand, child_prefix, true);
        },

        .pointer_type => {
            const ptr = ast.getPointerType(idx);
            const ptr_prefix = if (ptr.is_many_item) "*" else "@";
            const mut_str = if (ptr.is_mutable) "mut " else "";
            std.debug.print("pointer_type: {s}{s}\n", .{ ptr_prefix, mut_str });
            printNode(ast, tokens, src, ptr.pointee, child_prefix, true);
        },

        .err => {
            const err_data = ast.getError(idx);
            std.debug.print("error: {s}\n", .{err_data.msg});
        },
    }
}

fn printTypeValue(
    ast: *const Ast,
    tokens: *const TokenList,
    src: *const SourceCode,
    idx: NodeIndex,
) void {
    if (ast.getKind(idx) == .pointer_type) {
        const ptr = ast.getPointerType(idx);
        const prefix = if (ptr.is_many_item) "*" else "@";
        if (ptr.is_mutable) {
            std.debug.print("{s}mut ", .{prefix});
        } else {
            std.debug.print("{s}", .{prefix});
        }
        printTypeValue(ast, tokens, src, ptr.pointee);
    } else {
        printIdentifierValue(ast, tokens, src, idx);
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
