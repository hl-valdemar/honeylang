const std = @import("std");
const mem = std.mem;

const StringPool = @import("../util/StringPool.zig");
const Source = @import("../source/Source.zig");
const Lexer = @import("../lexer/Lexer.zig");
const Parser = @import("../parser/Parser.zig");
const Sema = @import("Sema.zig");

fn analyze(alloc: mem.Allocator, src_str: []const u8) !void {
    var src = try Source.init.fromStr(alloc, src_str, 0);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var lexer = Lexer.init(.{ .src = &src, .str_pool = &str_pool });
    defer lexer.deinit(alloc);
    try lexer.scan(alloc);

    var parser = Parser.init(.{ .src = &src, .tokens = lexer.tokens.slice() });
    defer parser.deinit(alloc);

    const ast = try parser.parse(alloc);
    var hir = try Parser.lower(alloc, &ast, &str_pool);
    defer hir.deinit(alloc);

    var sema = Sema.init(&hir, &str_pool);
    defer sema.deinit(alloc);
    try sema.analyze(alloc);
}

test "analyze namespace decl" {
    try analyze(std.testing.allocator,
        \\person {
        \\    x :: 1
        \\}
        \\
    );
}

test "sibling namespaces can reuse member names" {
    try analyze(std.testing.allocator,
        \\a {
        \\    x :: 1
        \\}
        \\
        \\b {
        \\    x :: 2
        \\}
        \\
    );
}

test "root declaration forward reference" {
    try analyze(std.testing.allocator,
        \\x :: y + 1
        \\y :: 2
        \\
    );
}

test "namespace declaration forward reference" {
    try analyze(std.testing.allocator,
        \\a {
        \\    x :: y + 1
        \\    y :: 2
        \\}
        \\
    );
}

test "namespace member can reference parent declaration" {
    try analyze(std.testing.allocator,
        \\x :: 1
        \\
        \\a {
        \\    y :: x + 1
        \\}
        \\
    );
}

test "duplicate declaration in namespace fails" {
    try std.testing.expectError(error.DuplicateDeclaration, analyze(std.testing.allocator,
        \\a {
        \\    x :: 1
        \\    x :: 2
        \\}
        \\
    ));
}

test "child declaration cannot shadow parent declaration" {
    try std.testing.expectError(error.DuplicateDeclaration, analyze(std.testing.allocator,
        \\x :: 1
        \\
        \\a {
        \\    x :: 2
        \\}
        \\
    ));
}

test "function parameter cannot shadow parent declaration" {
    try std.testing.expectError(error.DuplicateDeclaration, analyze(std.testing.allocator,
        \\x :: 1
        \\
        \\main :: func(x int) void {
        \\    return
        \\}
        \\
    ));
}

test "namespace cannot be used as expression value" {
    try std.testing.expectError(error.NamespaceNotValue, analyze(std.testing.allocator,
        \\a {
        \\    y :: 1
        \\}
        \\
        \\x :: a
        \\
    ));
}

test "namespace cannot be used as type" {
    try std.testing.expectError(error.UndefinedType, analyze(std.testing.allocator,
        \\a {
        \\    y :: 1
        \\}
        \\
        \\x a = 1
        \\
    ));
}

test "direct declaration cycle fails" {
    try std.testing.expectError(error.DeclarationCycle, analyze(std.testing.allocator,
        \\x :: x
        \\
    ));
}

test "indirect declaration cycle fails" {
    try std.testing.expectError(error.DeclarationCycle, analyze(std.testing.allocator,
        \\x :: y
        \\y :: x
        \\
    ));
}

test "long declaration cycle fails" {
    try std.testing.expectError(error.DeclarationCycle, analyze(std.testing.allocator,
        \\x :: y
        \\y :: z
        \\z :: x
        \\
    ));
}

test "return type mismatch fails" {
    try std.testing.expectError(error.TypeMismatch, analyze(std.testing.allocator,
        \\main :: func() int {
        \\    return 3.14
        \\}
        \\
    ));
}

test "bare return in non-void function fails" {
    try std.testing.expectError(error.TypeMismatch, analyze(std.testing.allocator,
        \\main :: func() int {
        \\    return
        \\}
        \\
    ));
}

test "return value in void function fails" {
    try std.testing.expectError(error.TypeMismatch, analyze(std.testing.allocator,
        \\main :: func() void {
        \\    return 1
        \\}
        \\
    ));
}

test "if condition must be bool" {
    try std.testing.expectError(error.TypeMismatch, analyze(std.testing.allocator,
        \\main :: func() void {
        \\    if 1 {
        \\        return
        \\    }
        \\}
        \\
    ));
}

test "not operand must be bool" {
    try std.testing.expectError(error.TypeMismatch, analyze(std.testing.allocator,
        \\x :: !1
        \\
    ));
}

test "negate operand must be numeric" {
    try std.testing.expectError(error.TypeMismatch, analyze(std.testing.allocator,
        \\x :: -true
        \\
    ));
}
