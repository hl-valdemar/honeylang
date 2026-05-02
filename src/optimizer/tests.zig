const std = @import("std");
const mem = std.mem;

const StringPool = @import("../util/StringPool.zig");
const Source = @import("../source/Source.zig");
const Lexer = @import("../lexer/Lexer.zig");
const Parser = @import("../parser/Parser.zig");
const Diagnostic = @import("../diagnostic/Store.zig");
const Sema = @import("../sema/Sema.zig");
const Optimizer = @import("Optimizer.zig");

fn optimizedMirRender(alloc: mem.Allocator, src_str: []const u8) ![]const u8 {
    var src = try Source.init.fromStr(alloc, src_str, 0);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = Diagnostic.init();
    defer diagnostics.deinit(alloc);

    var lexer = Lexer.init(.{ .src = &src, .str_pool = &str_pool, .diagnostics = &diagnostics, .shared_alloc = alloc });
    defer lexer.deinit(alloc);
    try lexer.scan(alloc);

    var parser = Parser.init(.{ .src = &src, .tokens = lexer.tokens.slice(), .diagnostics = &diagnostics, .diagnostic_alloc = alloc });
    defer parser.deinit(alloc);
    const ast = try parser.parse(alloc);

    var hir = try Parser.lowerWithDiagnostics(alloc, &ast, &str_pool, &diagnostics, alloc);
    defer hir.deinit(alloc);

    var sema = Sema.init(&hir, &str_pool, &diagnostics, alloc);
    defer sema.deinit(alloc);
    try sema.analyze(alloc);
    try std.testing.expect(!diagnostics.hasErrors());

    var opt = Optimizer.init(.{ .mir = &sema.mir, .str_pool = &str_pool, .diagnostics = &diagnostics, .shared_alloc = alloc });
    defer opt.deinit(alloc);
    try opt.optimize(alloc);

    return try opt.mir_optimized.render(alloc, &str_pool);
}

fn expectOptimizedContains(src: []const u8, expected: []const u8) !void {
    const alloc = std.testing.allocator;
    const rendered = try optimizedMirRender(alloc, src);
    defer alloc.free(rendered);
    try std.testing.expect(std.mem.containsAtLeast(u8, rendered, 1, expected));
}

test "fold multiply" {
    try expectOptimizedContains(
        \\x :: 2 * 3
        \\
    , "int(6)");
}

test "fold subtract" {
    try expectOptimizedContains(
        \\x :: 8 - 3
        \\
    , "int(5)");
}

test "fold divide" {
    try expectOptimizedContains(
        \\x :: 8 / 2
        \\
    , "int(4)");
}

test "division by zero emits trap" {
    try expectOptimizedContains(
        \\x :: 8 / 0
        \\
    , "trap(D");
}

test "fold negate" {
    try expectOptimizedContains(
        \\x :: -1
        \\
    , "int(-1)");
}

test "fold not" {
    try expectOptimizedContains(
        \\x :: !false
        \\
    , "bool(1)");
}

test "preserve if else in live function body" {
    try expectOptimizedContains(
        \\main :: func() void {
        \\    if true {
        \\        return
        \\    } else {
        \\        return
        \\    }
        \\}
        \\
    , "if_else");
}

test "preserve external function params" {
    try expectOptimizedContains(
        \\puts :: c func(s int) int
        \\
    , "decl_func(\"puts\", cc=c, params=(%0), ret=i32, body=<extern>)");
}
