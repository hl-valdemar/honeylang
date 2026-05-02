const std = @import("std");
const mem = std.mem;

const StringPool = @import("../util/StringPool.zig");
const Source = @import("../source/Source.zig");
const Lexer = @import("../lexer/Lexer.zig");
const Parser = @import("../parser/Parser.zig");
const Diagnostic = @import("../diagnostic/Store.zig");
const Sema = @import("Sema.zig");
const MIR = @import("MIR.zig");

const AnalyzeResult = struct {
    diagnostics: Diagnostic,
    mir_tags: []const MIR.Inst.Tag,

    fn deinit(self: *AnalyzeResult, alloc: mem.Allocator) void {
        self.diagnostics.deinit(alloc);
        alloc.free(self.mir_tags);
    }
};

fn analyze(alloc: mem.Allocator, src_str: []const u8) !void {
    var result = try analyzeResult(alloc, src_str);
    defer result.deinit(alloc);
    try std.testing.expect(!result.diagnostics.hasErrors());
}

fn analyzeResult(alloc: mem.Allocator, src_str: []const u8) !AnalyzeResult {
    var src = try Source.init.fromStr(alloc, src_str, 0);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = Diagnostic.init();
    errdefer diagnostics.deinit(alloc);

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

    return .{
        .diagnostics = diagnostics,
        .mir_tags = try alloc.dupe(MIR.Inst.Tag, sema.mir.insts.items(.tag)),
    };
}

fn expectDiagnostic(src: []const u8, tag: Diagnostic.Tag) !void {
    var result = try analyzeResult(std.testing.allocator, src);
    defer result.deinit(std.testing.allocator);

    var found_diagnostic = false;
    for (0..result.diagnostics.len()) |idx| {
        const diag = result.diagnostics.get(idx);
        if (diag.tag == tag) {
            found_diagnostic = true;
            break;
        }
    }

    var found_trap = false;
    for (result.mir_tags) |mir_tag| {
        if (mir_tag == .trap) {
            found_trap = true;
            break;
        }
    }

    try std.testing.expect(found_diagnostic);
    try std.testing.expect(found_trap);
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
    try expectDiagnostic(
        \\a {
        \\    x :: 1
        \\    x :: 2
        \\}
        \\
    , .sema_duplicate_declaration);
}

test "child declaration cannot shadow parent declaration" {
    try expectDiagnostic(
        \\x :: 1
        \\
        \\a {
        \\    x :: 2
        \\}
        \\
    , .sema_duplicate_declaration);
}

test "function parameter cannot shadow parent declaration" {
    try expectDiagnostic(
        \\x :: 1
        \\
        \\main :: func(x int) void {
        \\    return
        \\}
        \\
    , .sema_duplicate_declaration);
}

test "namespace cannot be used as expression value" {
    try expectDiagnostic(
        \\a {
        \\    y :: 1
        \\}
        \\
        \\x :: a
        \\
    , .sema_namespace_not_value);
}

test "namespace cannot be used as type" {
    try expectDiagnostic(
        \\a {
        \\    y :: 1
        \\}
        \\
        \\x a = 1
        \\
    , .sema_undefined_type);
}

test "direct declaration cycle fails" {
    try expectDiagnostic(
        \\x :: x
        \\
    , .sema_declaration_cycle);
}

test "indirect declaration cycle fails" {
    try expectDiagnostic(
        \\x :: y
        \\y :: x
        \\
    , .sema_declaration_cycle);
}

test "long declaration cycle fails" {
    try expectDiagnostic(
        \\x :: y
        \\y :: z
        \\z :: x
        \\
    , .sema_declaration_cycle);
}

test "return type mismatch fails" {
    try expectDiagnostic(
        \\main :: func() int {
        \\    return 3.14
        \\}
        \\
    , .sema_type_mismatch);
}

test "bare return in non-void function fails" {
    try expectDiagnostic(
        \\main :: func() int {
        \\    return
        \\}
        \\
    , .sema_type_mismatch);
}

test "return value in void function fails" {
    try expectDiagnostic(
        \\main :: func() void {
        \\    return 1
        \\}
        \\
    , .sema_type_mismatch);
}

test "if condition must be bool" {
    try expectDiagnostic(
        \\main :: func() void {
        \\    if 1 {
        \\        return
        \\    }
        \\}
        \\
    , .sema_type_mismatch);
}

test "not operand must be bool" {
    try expectDiagnostic(
        \\x :: !1
        \\
    , .sema_type_mismatch);
}

test "negate operand must be numeric" {
    try expectDiagnostic(
        \\x :: -true
        \\
    , .sema_type_mismatch);
}

test "missing expression becomes diagnostic trap" {
    try expectDiagnostic("x ::", .parser_expected_expression);
}

test "if else analyzes" {
    try analyze(std.testing.allocator,
        \\main :: func() void {
        \\    if true {
        \\        return
        \\    } else {
        \\        return
        \\    }
        \\}
        \\
    );
}

test "external c function analyzes" {
    try analyze(std.testing.allocator,
        \\puts :: c func(s int) int
        \\
    );
}
