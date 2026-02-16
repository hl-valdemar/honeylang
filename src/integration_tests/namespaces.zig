const std = @import("std");
const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;
const NodeKind = @import("../parser/ast.zig").NodeKind;

test "named import c header" {
    var r = try compileTo(.parser,
        \\math :: import c include "math.h"
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "named import honey file" {
    var r = try compileTo(.parser,
        \\utils :: import "utils.hon"
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "bare import still works" {
    var r = try compileTo(.parser,
        \\import "utils.hon"
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "bare c import still works" {
    var r = try compileTo(.parser,
        \\import c include "math.h"
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "named import produces correct AST node kind" {
    var r = try compileTo(.parser,
        \\mylib :: import "lib.hon"
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();

    const ast = &r.parse.?.ast;
    const program = ast.getProgram(ast.root);
    const decls = ast.getExtra(program.declarations);
    try std.testing.expectEqual(@as(usize, 1), decls.len);
    const import_node = decls[0];
    try std.testing.expectEqual(NodeKind.import_decl, ast.getKind(import_node));

    const import_decl = ast.getImportDecl(import_node);
    try std.testing.expect(import_decl.name_token != null);
}

test "named c import produces correct AST node kind" {
    var r = try compileTo(.parser,
        \\math :: import c include "math.h"
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();

    const ast = &r.parse.?.ast;
    const program = ast.getProgram(ast.root);
    const decls = ast.getExtra(program.declarations);
    try std.testing.expectEqual(@as(usize, 1), decls.len);
    const import_node = decls[0];
    try std.testing.expectEqual(NodeKind.c_include_decl, ast.getKind(import_node));

    const import_decl = ast.getImportDecl(import_node);
    try std.testing.expect(import_decl.name_token != null);
}

test "bare import has null name_token" {
    var r = try compileTo(.parser,
        \\import "lib.hon"
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();

    const ast = &r.parse.?.ast;
    const program = ast.getProgram(ast.root);
    const decls = ast.getExtra(program.declarations);
    const import_node = decls[0];
    const import_decl = ast.getImportDecl(import_node);
    try std.testing.expect(import_decl.name_token == null);
}
