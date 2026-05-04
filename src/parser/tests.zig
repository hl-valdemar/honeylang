const std = @import("std");
const mem = std.mem;

const StringPool = @import("../util/StringPool.zig");
const Source = @import("../source/Source.zig");
const Diagnostic = @import("../diagnostic/Store.zig");
const Lexer = @import("../lexer/Lexer.zig");
const Token = @import("../lexer/Token.zig");
const AST = @import("AST.zig");
const Parser = @import("Parser.zig");

/// helper: parse source code and return ast render.
fn parse(alloc: mem.Allocator, src_str: []const u8) ![]const u8 {
    var src = try Source.init.fromStr(alloc, src_str, Source.ID.fromInt(0));
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = Diagnostic.init();
    defer diagnostics.deinit(alloc);

    var lexer = Lexer.init(.{ .src = &src, .str_pool = &str_pool, .shared_alloc = alloc, .diagnostics = &diagnostics });
    defer lexer.deinit(alloc);

    try lexer.scan(alloc);
    const tokens = lexer.tokens.slice();

    var parser = Parser.init(.{ .src = &src, .tokens = tokens, .diagnostic_alloc = alloc, .diagnostics = &diagnostics });
    defer parser.deinit(alloc);

    const ast = try parser.parse(alloc);
    return try ast.render(alloc, src.contents, &str_pool);
}

test "parse const decl" {
    const alloc = std.testing.allocator;
    const src_str =
        \\pi :: 3.14
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "parse var decl" {
    const alloc = std.testing.allocator;
    const src_str =
        \\x = 0xbeaf
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
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
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "parse arithmetic expr" {
    const alloc = std.testing.allocator;
    const src_str =
        \\x = 1 + 2 * 3
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "parse simple if statement with grouped condition" {
    const alloc = std.testing.allocator;
    const src_str =
        \\main :: func() void {
        \\    if (true) {
        \\        a + b
        \\    }
        \\}
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "parse simple if statement with non-grouped condition" {
    const alloc = std.testing.allocator;
    const src_str =
        \\main :: func() void {
        \\    if true {
        \\        a + b
        \\    }
        \\}
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "ast typed payload accessors cover funcs branches and ref lists" {
    const alloc = std.testing.allocator;
    var src = try Source.init.fromStr(alloc,
        \\main :: func(a int) void {
        \\    if true {
        \\        return
        \\    } else {
        \\        return
        \\    }
        \\}
        \\
    , Source.ID.fromInt(0));
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = Diagnostic.init();
    defer diagnostics.deinit(alloc);

    var lexer = Lexer.init(.{ .src = &src, .str_pool = &str_pool, .shared_alloc = alloc, .diagnostics = &diagnostics });
    defer lexer.deinit(alloc);
    try lexer.scan(alloc);

    var parser = Parser.init(.{ .src = &src, .tokens = lexer.tokens.slice(), .diagnostic_alloc = alloc, .diagnostics = &diagnostics });
    defer parser.deinit(alloc);
    const ast = try parser.parse(alloc);

    const root_data = ast.nodeData(@enumFromInt(0));
    const root_refs = ast.refSlice(root_data.a, root_data.b);
    try std.testing.expectEqual(@as(usize, 1), root_refs.len);

    const func_ref = root_refs[0];
    try std.testing.expectEqual(AST.Node.Tag.func_decl, ast.nodeTag(func_ref));
    const func = ast.funcInfo(AST.asFuncRef(ast.nodeData(func_ref).b));
    try std.testing.expectEqual(@as(usize, 1), ast.refSlice(func.params_start, func.params_end).len);

    const body_refs = ast.refSlice(ast.nodeData(func.body).a, ast.nodeData(func.body).b);
    try std.testing.expectEqual(@as(usize, 1), body_refs.len);
    const branch = ast.branchInfo(AST.asBranchRef(ast.nodeData(body_refs[0]).b));
    try std.testing.expect(ast.nodeTag(branch.body) == .block);
    try std.testing.expect(ast.nodeTag(branch.else_node) == .block);
}

const HIR = @import("HIR.zig");

/// helper: parse source and lower to HIR, return instruction tags.
fn lowerToTags(alloc: mem.Allocator, src_str: []const u8) ![]const HIR.Inst.Tag {
    var src = try Source.init.fromStr(alloc, src_str, Source.ID.fromInt(0));
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = Diagnostic.init();
    defer diagnostics.deinit(alloc);

    var lexer = Lexer.init(.{ .src = &src, .str_pool = &str_pool, .shared_alloc = alloc, .diagnostics = &diagnostics });
    defer lexer.deinit(alloc);

    try lexer.scan(alloc);
    const tokens = lexer.tokens.slice();

    var parser = Parser.init(.{ .src = &src, .tokens = tokens, .diagnostic_alloc = alloc, .diagnostics = &diagnostics });
    defer parser.deinit(alloc);

    const ast = try parser.parse(alloc);

    var hir = HIR.init(.{ .ast = &ast, .src = src.contents, .source_id = src.id, .str_pool = &str_pool, .diagnostic_alloc = alloc, .diagnostics = &diagnostics });
    defer hir.deinit(alloc);

    const root: HIR.Inst.Ref = @enumFromInt(0);
    _ = try hir.lower(alloc, root);

    return try alloc.dupe(HIR.Inst.Tag, hir.insts.items(.tag));
}

test "lower const decl" {
    const alloc = std.testing.allocator;
    const tags = try lowerToTags(alloc,
        \\x :: 42
        \\
    );
    defer alloc.free(tags);

    // x :: 42  →  %0 = int(42), %1 = decl_const(x, value=%0)
    try std.testing.expectEqualSlices(HIR.Inst.Tag, &.{
        .int_literal,
        .decl_const,
    }, tags);
}

test "lower arithmetic" {
    const alloc = std.testing.allocator;
    const tags = try lowerToTags(alloc,
        \\x :: 1 + 2
        \\
    );
    defer alloc.free(tags);

    // x :: 1 + 2  →  %0 = int(1), %1 = int(2), %2 = add(%0, %1), %3 = decl_const(x, value=%2)
    try std.testing.expectEqualSlices(HIR.Inst.Tag, &.{
        .int_literal,
        .int_literal,
        .add,
        .decl_const,
    }, tags);
}

test "parse namespace decl" {
    const alloc = std.testing.allocator;
    const src_str =
        \\person {
        \\    x :: 1
        \\}
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "parse implicit import decl" {
    const alloc = std.testing.allocator;
    const src_str =
        \\import "math.hon"
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "parse explicit import decl" {
    const alloc = std.testing.allocator;
    const src_str =
        \\math_alias :: import "math.hon"
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "parse qualified reference" {
    const alloc = std.testing.allocator;
    const src_str =
        \\x :: math.value
        \\
    ;
    const rendered_str = try parse(alloc, src_str);
    defer alloc.free(rendered_str);
    try std.testing.expectEqualStrings(src_str, rendered_str);
}

test "lower namespace decl" {
    const alloc = std.testing.allocator;
    const tags = try lowerToTags(alloc,
        \\person {
        \\    x :: 1
        \\}
        \\
    );
    defer alloc.free(tags);

    try std.testing.expectEqualSlices(HIR.Inst.Tag, &.{
        .int_literal,
        .decl_const,
        .block,
        .decl_namespace,
    }, tags);
}

test "lower import decl" {
    const alloc = std.testing.allocator;
    const tags = try lowerToTags(alloc,
        \\import "math.hon"
        \\
    );
    defer alloc.free(tags);

    try std.testing.expectEqualSlices(HIR.Inst.Tag, &.{
        .import_decl,
    }, tags);
}

test "hir typed payload accessors cover decls funcs branches and ref lists" {
    const alloc = std.testing.allocator;
    var src = try Source.init.fromStr(alloc,
        \\x :: 1
        \\main :: func(a int) void {
        \\    if true {
        \\        return
        \\    } else {
        \\        return
        \\    }
        \\}
        \\
    , Source.ID.fromInt(0));
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = Diagnostic.init();
    defer diagnostics.deinit(alloc);

    var lexer = Lexer.init(.{ .src = &src, .str_pool = &str_pool, .shared_alloc = alloc, .diagnostics = &diagnostics });
    defer lexer.deinit(alloc);
    try lexer.scan(alloc);

    var parser = Parser.init(.{ .src = &src, .tokens = lexer.tokens.slice(), .diagnostic_alloc = alloc, .diagnostics = &diagnostics });
    defer parser.deinit(alloc);
    const ast = try parser.parse(alloc);

    var hir = HIR.init(.{ .ast = &ast, .src = src.contents, .source_id = src.id, .str_pool = &str_pool, .diagnostic_alloc = alloc, .diagnostics = &diagnostics });
    defer hir.deinit(alloc);
    const root: HIR.Inst.Ref = @enumFromInt(0);
    _ = try hir.lower(alloc, root);

    const root_refs = hir.rootDecls();
    try std.testing.expectEqual(@as(usize, 2), root_refs.len);

    const decl_inst = hir.insts.get(@intFromEnum(root_refs[0]));
    try std.testing.expectEqual(HIR.Inst.Tag.decl_const, decl_inst.tag);
    const decl = hir.declInfo(HIR.asDeclRef(decl_inst.data.b));
    try std.testing.expect(decl.value != .none);

    const func_inst = hir.insts.get(@intFromEnum(root_refs[1]));
    try std.testing.expectEqual(HIR.Inst.Tag.decl_func, func_inst.tag);
    const func = hir.funcInfo(HIR.asFuncRef(func_inst.data.b));
    try std.testing.expectEqual(@as(usize, 1), hir.refSlice(func.params_start, func.params_end).len);

    const body_inst = hir.insts.get(@intFromEnum(func.body));
    const body_refs = hir.refSlice(body_inst.data.a, body_inst.data.b);
    try std.testing.expectEqual(@as(usize, 1), body_refs.len);
    const if_inst = hir.insts.get(@intFromEnum(body_refs[0]));
    const branch = hir.branchInfo(HIR.asBranchRef(if_inst.data.b));
    try std.testing.expect(branch.body != .none);
    try std.testing.expect(branch.else_node != .none);
}
