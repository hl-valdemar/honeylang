const std = @import("std");
const honey = @import("root.zig");

const LexerErrorKind = @import("lexer/error.zig").LexerErrorKind;
const ParseErrorKind = @import("parser/error.zig").ParseErrorKind;
const SemanticErrorKind = @import("semantic/error.zig").SemanticErrorKind;

// -- helpers --

fn compileTo(phase: Phase, src_input: []const u8) !CompileResult {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    errdefer arena.deinit();

    const allocator = arena.allocator();
    const src = honey.source.fromStr(src_input, 0);

    // lexer
    const lex = try honey.lexer.scan(allocator, &src);
    if (phase == .lexer) return .{ .arena = arena, .lex = lex, .parse = null, .sem = null, .codegen = null };

    // parser
    const parse = try honey.parser.parse(allocator, lex.tokens, &src);
    if (phase == .parser) return .{ .arena = arena, .lex = lex, .parse = parse, .sem = null, .codegen = null };

    // semantic
    var sem = try honey.semantic.analyze(allocator, &parse.ast, &lex.tokens, &src);
    if (phase == .semantic) return .{ .arena = arena, .lex = lex, .parse = parse, .sem = sem, .codegen = null };

    // comptime
    const comptime_result = try honey.comptime_.evaluate(allocator, &parse.ast, &lex.tokens, &src, &sem.symbols);

    // codegen
    const target: honey.codegen.Target = .{ .arch = .aarch64, .os = .darwin };
    const codegen_result = try honey.codegen.generate(allocator, target, &comptime_result, &sem.symbols, &sem.types, &sem.node_types, &sem.skip_nodes, &parse.ast, &lex.tokens, &src);
    return .{ .arena = arena, .lex = lex, .parse = parse, .sem = sem, .codegen = codegen_result };
}

const Phase = enum { lexer, parser, semantic, codegen };

const CompileResult = struct {
    arena: std.heap.ArenaAllocator,
    lex: honey.lexer.LexerResult,
    parse: ?honey.parser.ParseResult,
    sem: ?honey.semantic.SemanticResult,
    codegen: ?honey.codegen.CodeGenResult,

    fn deinit(self: *CompileResult) void {
        self.arena.deinit();
    }

    fn expectNoErrors(self: *const CompileResult) !void {
        try std.testing.expect(!self.lex.errors.hasErrors());
        if (self.parse) |p| try std.testing.expect(!p.errors.hasErrors());
        if (self.sem) |s| try std.testing.expect(!s.errors.hasErrors());
    }

    fn expectLLVMContains(self: *const CompileResult, expected: []const u8) !void {
        const cg = self.codegen orelse return error.TestExpectedEqual;
        if (std.mem.indexOf(u8, cg.output, expected) == null) {
            std.debug.print("\nExpected LLVM IR to contain: {s}\n", .{expected});
            std.debug.print("Actual LLVM IR:\n{s}\n", .{cg.output});
            return error.TestExpectedEqual;
        }
    }

    fn expectLexerError(self: *const CompileResult, expected: LexerErrorKind) !void {
        for (self.lex.errors.errors.items) |err| {
            if (err.kind == expected) return;
        }
        return error.TestExpectedEqual;
    }

    fn expectParseError(self: *const CompileResult, expected: ParseErrorKind) !void {
        const p = self.parse orelse return error.TestExpectedEqual;
        for (p.errors.errors.items) |err| {
            if (err.kind == expected) return;
        }
        return error.TestExpectedEqual;
    }

    fn expectSemanticError(self: *const CompileResult, expected: SemanticErrorKind) !void {
        const s = self.sem orelse return error.TestExpectedEqual;
        for (s.errors.errors.items) |err| {
            if (err.kind == expected) return;
        }
        return error.TestExpectedEqual;
    }
};

// ============================================================
// correct programs: declarations
// ============================================================

test "constant declaration" {
    var r = try compileTo(.semantic,
        \\X :: 42
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "typed constant declaration" {
    var r = try compileTo(.semantic,
        \\Y: f32 :: 0.4
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "boolean constant" {
    var r = try compileTo(.semantic,
        \\DEBUG :: true
        \\RELEASE :: false
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "variable declaration" {
    var r = try compileTo(.semantic,
        \\x: i32 = 42
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "mutable variable declaration" {
    var r = try compileTo(.semantic,
        \\mut x := 10
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "multiple declarations" {
    var r = try compileTo(.semantic,
        \\DEBUG :: true
        \\X :: -42
        \\Y: f32 :: 0.4
        \\Z :: X * Y
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: numeric types
// ============================================================

test "signed integer types" {
    var r = try compileTo(.semantic,
        \\a: i8 :: 1
        \\b: i16 :: 2
        \\c: i32 :: 3
        \\d: i64 :: 4
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "unsigned integer types" {
    var r = try compileTo(.semantic,
        \\a: u8 :: 1
        \\b: u16 :: 2
        \\c: u32 :: 3
        \\d: u64 :: 4
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "float types" {
    var r = try compileTo(.semantic,
        \\a: f16 :: 1.0
        \\b: f32 :: 2.0
        \\c: f64 :: 3.0
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: functions
// ============================================================

test "simple function" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  return 42
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "void function" {
    var r = try compileTo(.semantic,
        \\noop :: func() void {
        \\  return
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "function with parameters" {
    var r = try compileTo(.semantic,
        \\add :: func(a: i32, b: i32) i32 {
        \\  return a + b
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "function with three parameters" {
    var r = try compileTo(.semantic,
        \\add3 :: func(a: i32, b: i32, c: i32) i32 {
        \\  return a + b + c
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "function call" {
    var r = try compileTo(.semantic,
        \\add :: func(a: i32, b: i32) i32 {
        \\  return a + b
        \\}
        \\
        \\main :: func() i32 {
        \\  return add(40, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "multiple functions calling each other" {
    var r = try compileTo(.semantic,
        \\double :: func(x: i32) i32 {
        \\  return x + x
        \\}
        \\
        \\quadruple :: func(x: i32) i32 {
        \\  return double(double(x))
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "external c function declaration" {
    var r = try compileTo(.semantic,
        \\exit :: c func(code: i32) void
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: arithmetic operators
// ============================================================

test "addition" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  x: i32 = 1
        \\  y: i32 = 2
        \\  return x + y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "subtraction" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  x: i32 = 10
        \\  y: i32 = 3
        \\  return x - y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "multiplication" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  x: i32 = 6
        \\  y: i32 = 7
        \\  return x * y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "division" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  x: i32 = 42
        \\  y: i32 = 6
        \\  return x / y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "unary negation" {
    var r = try compileTo(.semantic,
        \\X :: -42
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "unary not" {
    var r = try compileTo(.semantic,
        \\main :: func() bool {
        \\  x: bool = true
        \\  return not x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: comparison operators
// ============================================================

test "equality comparison" {
    var r = try compileTo(.semantic,
        \\main :: func() bool {
        \\  x: i32 = 1
        \\  y: i32 = 1
        \\  return x == y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "inequality comparison" {
    var r = try compileTo(.semantic,
        \\main :: func() bool {
        \\  x: i32 = 1
        \\  y: i32 = 2
        \\  return x != y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "less than comparison" {
    var r = try compileTo(.semantic,
        \\main :: func() bool {
        \\  x: i32 = 1
        \\  y: i32 = 2
        \\  return x < y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "greater than comparison" {
    var r = try compileTo(.semantic,
        \\main :: func() bool {
        \\  x: i32 = 1
        \\  y: i32 = 2
        \\  return x > y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "less equal comparison" {
    var r = try compileTo(.semantic,
        \\main :: func() bool {
        \\  x: i32 = 1
        \\  y: i32 = 2
        \\  return x <= y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "greater equal comparison" {
    var r = try compileTo(.semantic,
        \\main :: func() bool {
        \\  x: i32 = 1
        \\  y: i32 = 2
        \\  return x >= y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: logical operators
// ============================================================

test "logical and" {
    var r = try compileTo(.semantic,
        \\main :: func() bool {
        \\  a: bool = true
        \\  b: bool = false
        \\  return a and b
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "logical or" {
    var r = try compileTo(.semantic,
        \\main :: func() bool {
        \\  a: bool = true
        \\  b: bool = false
        \\  return a or b
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: compound assignment
// ============================================================

test "plus equals" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  mut x := 3
        \\  x += 4
        \\  return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "minus equals" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  mut x := 10
        \\  x -= 3
        \\  return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "star equals" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  mut x := 5
        \\  x *= 2
        \\  return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "slash equals" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  mut x := 10
        \\  x /= 2
        \\  return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: control flow
// ============================================================

test "if statement" {
    var r = try compileTo(.semantic,
        \\SKIP :: true
        \\
        \\main :: func() i32 {
        \\  if SKIP {
        \\    return 1
        \\  }
        \\  return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "if else statement" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  x: bool = true
        \\  if x {
        \\    return 1
        \\  } else {
        \\    return 0
        \\  }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "if else-if else statement" {
    var r = try compileTo(.semantic,
        \\main :: func(x: i32) i32 {
        \\  if x == 1 {
        \\    return 10
        \\  } else if x == 2 {
        \\    return 20
        \\  } else {
        \\    return 30
        \\  }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "defer statement" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  defer y := 10
        \\  return 42
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: globals used in functions
// ============================================================

test "constant used in function" {
    var r = try compileTo(.semantic,
        \\FACTOR :: 2
        \\
        \\double :: func(x: i32) i32 {
        \\  return x * FACTOR
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "global mutable variable" {
    var r = try compileTo(.semantic,
        \\mut counter := 0
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// lexer errors
// ============================================================

test "unexpected character produces lexer error" {
    var r = try compileTo(.lexer,
        \\a :: 10 $ 20
        \\
    );
    defer r.deinit();
    try r.expectLexerError(.unexpected_character);
}

test "standalone bang produces lexer error" {
    var r = try compileTo(.lexer,
        \\x :: 10 ! 20
        \\
    );
    defer r.deinit();
    try r.expectLexerError(.unexpected_character);
}

test "multiple decimal points produces lexer error" {
    var r = try compileTo(.lexer,
        \\pi :: 3.14.159
        \\
    );
    defer r.deinit();
    try r.expectLexerError(.multiple_decimal_points);
}

// ============================================================
// parse errors
// ============================================================

test "missing closing brace produces parse error" {
    var r = try compileTo(.parser,
        \\main :: func() i32 {
        \\  return 42
        \\
    );
    defer r.deinit();
    try r.expectParseError(.unclosed_brace);
}

test "missing expression after double colon" {
    var r = try compileTo(.parser,
        \\x ::
        \\
    );
    defer r.deinit();
    try r.expectParseError(.expected_expression);
}

test "missing closing paren in call" {
    var r = try compileTo(.parser,
        \\add :: func(a: i32, b: i32) i32 {
        \\  return a + b
        \\}
        \\
        \\main :: func() i32 {
        \\  return add(1, 2
        \\}
        \\
    );
    defer r.deinit();
    try r.expectParseError(.expected_right_paren);
}

test "missing left paren in func declaration" {
    var r = try compileTo(.parser,
        \\main :: func i32 {
        \\  return 42
        \\}
        \\
    );
    defer r.deinit();
    try r.expectParseError(.expected_left_paren);
}

test "missing double colon in constant" {
    var r = try compileTo(.parser,
        \\x 42
        \\
    );
    defer r.deinit();
    try r.expectParseError(.expected_declaration);
}

test "unexpected eof" {
    var r = try compileTo(.parser,
        \\main :: func(
    );
    defer r.deinit();
    try r.expectParseError(.expected_identifier);
}

// ============================================================
// semantic errors: type system
// ============================================================

test "bool assigned integer literal" {
    var r = try compileTo(.semantic,
        \\main :: func() void {
        \\  flag: bool = 1
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.type_mismatch);
}

test "negation into unsigned constant" {
    var r = try compileTo(.semantic,
        \\X: u32 :: -42
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.type_mismatch);
}

test "type mismatch in variable: i32 to u32" {
    var r = try compileTo(.semantic,
        \\main :: func() void {
        \\  x: i32 = 10
        \\  y: u32 = x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.type_mismatch);
}

test "unknown type name" {
    var r = try compileTo(.semantic,
        \\x: foo :: 42
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.unknown_type);
}

// ============================================================
// semantic errors: symbols
// ============================================================

test "duplicate symbol" {
    var r = try compileTo(.semantic,
        \\X :: 1
        \\X :: 2
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.duplicate_symbol);
}

test "undefined symbol in expression" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  return y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.undefined_symbol);
}

// ============================================================
// semantic errors: functions
// ============================================================

test "argument count mismatch" {
    var r = try compileTo(.semantic,
        \\add :: func(a: i32, b: i32) i32 {
        \\  return a + b
        \\}
        \\
        \\main :: func() i32 {
        \\  return add(1)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.argument_count_mismatch);
}

test "argument type mismatch" {
    var r = try compileTo(.semantic,
        \\add :: func(a: i32, b: i32) i32 {
        \\  return a + b
        \\}
        \\
        \\main :: func() i32 {
        \\  x: bool = true
        \\  return add(x, 1)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.argument_type_mismatch);
}

test "return type mismatch" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  x: bool = true
        \\  return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.return_type_mismatch);
}

test "calling a non-function" {
    var r = try compileTo(.semantic,
        \\X :: 42
        \\
        \\main :: func() i32 {
        \\  return X(1)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.not_callable);
}

// ============================================================
// semantic errors: operators
// ============================================================

test "arithmetic on bool operands" {
    var r = try compileTo(.semantic,
        \\main :: func() bool {
        \\  a: bool = true
        \\  b: bool = false
        \\  return a + b
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.arithmetic_op_requires_numeric);
}

test "logical and on integer operands" {
    var r = try compileTo(.semantic,
        \\main :: func() bool {
        \\  a: i32 = 1
        \\  b: i32 = 2
        \\  return a and b
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.logical_op_requires_bool);
}

test "not on integer operand" {
    var r = try compileTo(.semantic,
        \\main :: func() bool {
        \\  a: i32 = 1
        \\  return not a
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.logical_op_requires_bool);
}

// ============================================================
// semantic errors: mutability and control flow
// ============================================================

test "assignment to immutable variable" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  x := 10
        \\  x += 1
        \\  return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.assignment_to_immutable);
}

test "condition must be bool" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  x: i32 = 1
        \\  if x {
        \\    return 1
        \\  }
        \\  return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.condition_not_bool);
}

// ============================================================
// correct programs: struct declarations
// ============================================================

test "c struct declaration" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "c struct used as function parameter type" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\get_x :: func(p: Point) i32 {
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// semantic errors: structs
// ============================================================

test "struct field access" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\get_x :: func(p: Point) i32 {
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "chained struct field access" {
    var r = try compileTo(.semantic,
        \\Inner :: c struct {
        \\    value: i32,
        \\}
        \\
        \\Outer :: c struct {
        \\    inner: Inner,
        \\}
        \\
        \\get_value :: func(o: Outer) i32 {
        \\    return o.inner.value
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// semantic errors: structs
// ============================================================

test "duplicate field in struct" {
    var r = try compileTo(.semantic,
        \\Bad :: c struct {
        \\    x: i32,
        \\    x: i32,
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.duplicate_field);
}

test "no such field on struct" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\bad :: func(p: Point) i32 {
        \\    return p.z
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.no_such_field);
}

test "field access on non-struct type" {
    var r = try compileTo(.semantic,
        \\bad :: func(x: i32) i32 {
        \\    return x.y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.field_access_on_non_struct);
}

// ============================================================
// codegen: struct type emission and field access
// ============================================================

test "codegen emits LLVM struct type definition" {
    var r = try compileTo(.codegen,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\main :: func() i32 {
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Point = type { i32, i32 }");
}

test "codegen struct field access emits GEP and load" {
    var r = try compileTo(.codegen,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\get_x :: func(p: Point) i32 {
        \\    return p.x
        \\}
        \\
        \\main :: func() i32 {
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Point = type { i32, i32 }");
    // get_x is unused from main, so it will be skipped — check struct type only
}

test "codegen struct field access with referenced function" {
    var r = try compileTo(.codegen,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\get_x :: func(p: Point) i32 {
        \\    return p.x
        \\}
        \\
        \\main :: func() i32 {
        \\    return get_x(0)
        \\}
        \\
    );
    defer r.deinit();
    // argument type mismatch, but "always compile" — codegen still runs
    try r.expectLLVMContains("%Point = type { i32, i32 }");
    try r.expectLLVMContains("getelementptr inbounds %Point");
    try r.expectLLVMContains("ptr %arg0");
}

// ============================================================
// correct programs: struct literals
// ============================================================

test "struct literal initialization" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 3, .y = 4 }
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// semantic errors: struct literals
// ============================================================

test "struct literal missing field" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 3 }
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.missing_field);
}

test "struct literal duplicate field" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 3, .x = 4 }
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.duplicate_literal_field);
}

test "struct literal field type mismatch" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\main :: func() i32 {
        \\    flag: bool = true
        \\    p := Point{ .x = 3, .y = flag }
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.type_mismatch);
}

// ============================================================
// codegen: struct literals
// ============================================================

test "codegen struct literal emits alloca and GEP stores" {
    var r = try compileTo(.codegen,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 3, .y = 4 }
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Point = type { i32, i32 }");
    try r.expectLLVMContains("alloca %Point");
    try r.expectLLVMContains("getelementptr inbounds %Point");
    try r.expectLLVMContains("store i32");
}

// ============================================================
// nested structs
// ============================================================

test "nested struct inline type definition" {
    var r = try compileTo(.codegen,
        \\Vec2 :: c struct { x: i32, y: i32, }
        \\Rect :: c struct { origin: Vec2, size: Vec2, }
        \\
        \\main :: func() i32 {
        \\    r := Rect{
        \\        .origin = Vec2{ .x = 1, .y = 2 },
        \\        .size = Vec2{ .x = 3, .y = 4 },
        \\    }
        \\    return r.origin.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // struct fields are inline, not pointers
    try r.expectLLVMContains("%Rect = type { %Vec2, %Vec2 }");
    // struct field store uses memcpy
    try r.expectLLVMContains("@llvm.memcpy.p0.p0.i64");
    // struct field access is GEP-only (no "load ptr")
    try r.expectLLVMContains("getelementptr inbounds %Rect");
    try r.expectLLVMContains("getelementptr inbounds %Vec2");
}

// ============================================================
// correct programs: pointers
// ============================================================

test "pointer deref and write" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut x := 42
        \\    p := &x
        \\    p^ = 100
        \\    return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("getelementptr i8, ptr %local.0, i32 0");
    try r.expectLLVMContains("store i32 %");
}

test "pointer compound assignment" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut x := 42
        \\    p := &x
        \\    p^ += 100
        \\    return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // desugared to p^ = p^ + 100: should have a load_ptr then add then store_ptr
    try r.expectLLVMContains("load i32, ptr %");
    try r.expectLLVMContains("store i32 %");
}

test "pointer as function parameter" {
    var r = try compileTo(.codegen,
        \\set_value :: func(p: @mut i32, val: i32) void {
        \\    p^ = val
        \\}
        \\
        \\main :: func() i32 {
        \\    mut x := 0
        \\    set_value(&x, 42)
        \\    return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("call fastcc void @set_value(ptr %");
}

test "pointer type annotation" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut x := 10
        \\    p : @mut i32 = &x
        \\    q : @i32 = &x
        \\    return q^
        \\}
        \\
    );
    defer r.deinit();
    // only a warning (unused p), no errors
    try std.testing.expect(!r.lex.errors.hasErrors());
    if (r.parse) |p| try std.testing.expect(!p.errors.hasErrors());
    if (r.sem) |s| try std.testing.expect(!s.errors.hasErrors());
    try r.expectLLVMContains("load ptr, ptr %local.1");
    try r.expectLLVMContains("load i32, ptr %");
}

test "pointer to struct" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\get_x :: func(p: @Point) i32 { return p^.x }
        \\
        \\main :: func() i32 {
        \\    pt := Point{ .x = 7, .y = 3 }
        \\    return get_x(&pt)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i32 @get_x(ptr %arg0)");
    try r.expectLLVMContains("getelementptr inbounds %Point");
}

test "nested struct reverse declaration order" {
    var r = try compileTo(.codegen,
        \\Rect :: c struct { origin: Vec2, size: Vec2, }
        \\Vec2 :: c struct { x: i32, y: i32, }
        \\
        \\main :: func() i32 {
        \\    r := Rect{
        \\        .origin = Vec2{ .x = 5, .y = 6 },
        \\        .size = Vec2{ .x = 7, .y = 8 },
        \\    }
        \\    return r.size.y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Rect = type { %Vec2, %Vec2 }");
}

// ============================================================
// codegen: float types
// ============================================================

test "f32 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: f32, b: f32) f32 {
        \\    return a + b
        \\}
        \\
        \\main :: func() f32 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc float @add(float %arg0, float %arg1)");
    try r.expectLLVMContains("fadd float");
    try r.expectLLVMContains("ret float");
}

test "f64 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: f64, b: f64) f64 {
        \\    return a + b
        \\}
        \\
        \\main :: func() f64 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc double @add(double %arg0, double %arg1)");
    try r.expectLLVMContains("fadd double");
    try r.expectLLVMContains("ret double");
}

test "f32 local variable and literal" {
    var r = try compileTo(.codegen,
        \\identity :: func(x: f32) f32 { return x }
        \\
        \\main :: func() f32 {
        \\    x: f32 = 42
        \\    return identity(x)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("fadd float 0.0, 0x");
    try r.expectLLVMContains("alloca float");
    try r.expectLLVMContains("store float %");
}

test "f32 arithmetic operations" {
    var r = try compileTo(.codegen,
        \\main :: func() f32 {
        \\    a: f32 = 10
        \\    b: f32 = 3
        \\    return a - b
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("load float, ptr %local.");
    try r.expectLLVMContains("fsub float");
}

test "f32 multiply and divide" {
    var r = try compileTo(.codegen,
        \\mul :: func(a: f32, b: f32) f32 {
        \\    return a * b
        \\}
        \\
        \\div :: func(a: f32, b: f32) f32 {
        \\    return a / b
        \\}
        \\
        \\main :: func() f32 {
        \\    return mul(div(6, 2), 3)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("fmul float");
    try r.expectLLVMContains("fdiv float");
}

test "struct with f32 fields" {
    var r = try compileTo(.codegen,
        \\Point :: struct { x: f32, y: f32 }
        \\
        \\get_x :: func(p: Point) f32 { return p.x }
        \\
        \\main :: func() f32 {
        \\    p := Point{ .x = 1, .y = 2 }
        \\    return get_x(p)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Point = type { float, float }");
    try r.expectLLVMContains("store float %");
}

test "struct with f32 field access" {
    var r = try compileTo(.codegen,
        \\Point :: struct { x: f32, y: f32 }
        \\
        \\get_x :: func(p: Point) f32 {
        \\    return p.x
        \\}
        \\
        \\main :: func() f32 {
        \\    p := Point{ .x = 5, .y = 10 }
        \\    return get_x(p)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("load float, ptr %gep.");
}

test "global struct with f32 fields comptime init" {
    var r = try compileTo(.codegen,
        \\Point :: struct { x: f32, y: f32 }
        \\
        \\p := Point{ .x = 52, .y = 44 }
        \\
        \\main :: func() i32 {
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Point = type { float, float }");
    // 52.0 as f64 bits = 0x404A000000000000
    try r.expectLLVMContains("float 0x404A000000000000");
    // 44.0 as f64 bits = 0x4046000000000000
    try r.expectLLVMContains("float 0x4046000000000000");
}

test "f32 function call with struct field args" {
    var r = try compileTo(.codegen,
        \\Point :: struct { x: f32, y: f32 }
        \\
        \\p := Point{ .x = 52, .y = 44 }
        \\
        \\add :: func(a: f32, b: f32) f32 { return a + b }
        \\
        \\main :: func() f32 { return add(p.x, p.y) }
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("call fastcc float @add(float %");
    try r.expectLLVMContains("fadd float");
}

test "f32 constant via comptime" {
    var r = try compileTo(.codegen,
        \\PI: f32 :: 3
        \\
        \\main :: func() f32 {
        \\    return PI
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("fadd float 0.0, 0x");
    try r.expectLLVMContains("ret float");
}

test "global f32 variable" {
    var r = try compileTo(.codegen,
        \\x: f32 = 10
        \\
        \\main :: func() f32 {
        \\    return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("global float 0x");
    try r.expectLLVMContains("load float, ptr @x");
}

test "mixed f32 and i32 struct fields" {
    var r = try compileTo(.codegen,
        \\Entity :: struct { id: i32, x: f32, y: f32 }
        \\
        \\main :: func() i32 {
        \\    e := Entity{ .id = 1, .x = 5, .y = 10 }
        \\    return e.id
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Entity = type { i32, float, float }");
    try r.expectLLVMContains("store i32");
    try r.expectLLVMContains("store float");
}

// ============================================================
// correct programs: backward type inference through address-of
// ============================================================

test "return address-of with unresolved local" {
    var r = try compileTo(.codegen,
        \\main :: func() @i32 {
        \\    mut x := 42
        \\    return &x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "address-of through intermediate variable" {
    var r = try compileTo(.codegen,
        \\main :: func() @i32 {
        \\    mut x := 42
        \\    p := &x
        \\    return p
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "address-of chain through two intermediates" {
    var r = try compileTo(.codegen,
        \\main :: func() @i32 {
        \\    mut x := 42
        \\    p := &x
        \\    q := p
        \\    return q
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: many-item pointers
// ============================================================

test "many-item pointer type annotation" {
    var r = try compileTo(.semantic,
        \\main :: func() void {
        \\    mut x: i32 = 10
        \\    p: *mut i32 = &x
        \\    q: *i32 = &x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "many-item pointer arithmetic add" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut x: i32 = 10
        \\    p: *mut i32 = &x
        \\    q: *i32 = p + 1
        \\    return q^
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("getelementptr i8, ptr %");
}

test "many-item pointer arithmetic sub" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut x: i32 = 10
        \\    p: *mut i32 = &x
        \\    q: *i32 = p + 2
        \\    r: *i32 = q - 1
        \\    return r^
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("sub i64 0, %");
    try r.expectLLVMContains("getelementptr i8, ptr %");
}

test "many-item pointer int + ptr commutative" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut x: i32 = 10
        \\    p: *mut i32 = &x
        \\    q: *i32 = 2 + p
        \\    return q^
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("getelementptr i8, ptr %");
}

test "many-item pointer as function parameter" {
    var r = try compileTo(.codegen,
        \\read_at :: func(base: *i32, offset: i32) i32 {
        \\    p: *i32 = base + offset
        \\    return p^
        \\}
        \\
        \\main :: func() i32 {
        \\    mut x: i32 = 42
        \\    p: *mut i32 = &x
        \\    return read_at(p, 0)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i32 @read_at(ptr %arg0, i32 %arg1)");
    try r.expectLLVMContains("getelementptr i8, ptr %");
}

test "many-item pointer scales by pointee size" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut x: i32 = 10
        \\    p: *mut i32 = &x
        \\    q: *i32 = p + 1
        \\    return q^
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // i32 is 4 bytes, so offset should be multiplied by 4
    try r.expectLLVMContains("mul i64 %");
    try r.expectLLVMContains(", 4");
}

// ============================================================
// semantic errors: many-item pointers
// ============================================================

test "single-item pointer arithmetic still rejected" {
    var r = try compileTo(.semantic,
        \\main :: func() void {
        \\    mut x: i32 = 42
        \\    p: @mut i32 = &x
        \\    bad: i32 = p + 1
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.pointer_arithmetic);
}

test "many-item and single-item pointers are distinct types" {
    var r = try compileTo(.semantic,
        \\main :: func() void {
        \\    mut x: i32 = 42
        \\    p: *i32 = &x
        \\    q: @i32 = p
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.type_mismatch);
}

// ============================================================
// codegen: address-of struct field
// ============================================================

test "address-of struct field emits GEP" {
    var r = try compileTo(.codegen,
        \\Buf :: c struct { a: i32, b: i32 }
        \\
        \\main :: func() i32 {
        \\    mut buf := Buf{ .a = 10, .b = 20 }
        \\    p: @mut i32 = &buf.a
        \\    return p^
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("getelementptr inbounds %Buf");
}

// ============================================================
// codegen: struct return (SRET)
// ============================================================

test "function returning struct uses sret" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\make_point :: func(x: i32, y: i32) Point {
        \\    return Point{ .x = x, .y = y }
        \\}
        \\
        \\main :: func() i32 {
        \\    p := make_point(3, 4)
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // function definition uses sret hidden parameter
    try r.expectLLVMContains("define fastcc void @make_point(ptr sret(%Point) %arg0");
    // caller allocates struct and passes as sret
    try r.expectLLVMContains("call fastcc void @make_point(ptr sret(%Point)");
}

test "sret with nested struct return" {
    var r = try compileTo(.codegen,
        \\Vec2 :: c struct { x: i32, y: i32 }
        \\Rect :: c struct { origin: Vec2, size: Vec2 }
        \\
        \\make_rect :: func() Rect {
        \\    return Rect{
        \\        .origin = Vec2{ .x = 0, .y = 0 },
        \\        .size = Vec2{ .x = 10, .y = 20 },
        \\    }
        \\}
        \\
        \\main :: func() i32 {
        \\    r := make_rect()
        \\    return r.origin.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc void @make_rect(ptr sret(%Rect) %arg0");
    try r.expectLLVMContains("call fastcc void @make_rect(ptr sret(%Rect)");
}

// ============================================================
// codegen: struct by-value parameter passing
// ============================================================

test "struct by-value parameter emits memcpy at call site" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\sum :: func(p: Point) i32 {
        \\    return p.x
        \\}
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 10, .y = 20 }
        \\    return sum(p)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // struct parameter is passed as pointer
    try r.expectLLVMContains("define fastcc i32 @sum(ptr %arg0)");
    // field access via GEP inside function
    try r.expectLLVMContains("getelementptr inbounds %Point");
}

// ============================================================
// codegen: struct copy assignment
// ============================================================

test "struct copy via assignment emits memcpy" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 5, .y = 6 }
        \\    q := p
        \\    return q.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // copy from p to q uses memcpy
    try r.expectLLVMContains("@llvm.memcpy.p0.p0.i64");
}

// ============================================================
// codegen: mutable struct field write
// ============================================================

test "mutable struct field store" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\main :: func() i32 {
        \\    mut p := Point{ .x = 1, .y = 2 }
        \\    p.x = 42
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("getelementptr inbounds %Point");
    try r.expectLLVMContains("store i32");
}

// ============================================================
// codegen: global struct with i32 fields
// ============================================================

test "global struct i32 codegen" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\origin := Point{ .x = 0, .y = 0 }
        \\
        \\main :: func() i32 {
        \\    return origin.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Point = type { i32, i32 }");
    try r.expectLLVMContains("@origin = global %Point { i32 0, i32 0 }");
}

// ============================================================
// codegen: c calling convention with structs
// ============================================================

test "c function taking struct parameter" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\process :: c func(p: Point) i32 {
        \\    return p.x
        \\}
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 7, .y = 8 }
        \\    return process(p)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // C calling convention function with struct param
    try r.expectLLVMContains("define i32 @process(ptr %arg0)");
    try r.expectLLVMContains("call i32 @process(ptr");
}

test "c function returning struct uses sret" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\make :: c func(x: i32, y: i32) Point {
        \\    return Point{ .x = x, .y = y }
        \\}
        \\
        \\main :: func() i32 {
        \\    p := make(3, 4)
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // C convention sret
    try r.expectLLVMContains("define void @make(ptr sret(%Point) %arg0");
    try r.expectLLVMContains("call void @make(ptr sret(%Point)");
}

// ============================================================
// codegen: primitive type widths
// ============================================================

test "i8 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: i8, b: i8) i8 {
        \\    return a + b
        \\}
        \\
        \\main :: func() i8 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i8 @add(i8 %arg0, i8 %arg1)");
    try r.expectLLVMContains("add i8");
    try r.expectLLVMContains("ret i8");
}

test "u8 function parameter and return" {
    var r = try compileTo(.codegen,
        \\identity :: func(x: u8) u8 { return x }
        \\
        \\main :: func() u8 {
        \\    return identity(42)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i8 @identity(i8 %arg0)");
    try r.expectLLVMContains("ret i8");
}

test "i16 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: i16, b: i16) i16 {
        \\    return a + b
        \\}
        \\
        \\main :: func() i16 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i16 @add(i16 %arg0, i16 %arg1)");
    try r.expectLLVMContains("add i16");
    try r.expectLLVMContains("ret i16");
}

test "u16 function parameter and return" {
    var r = try compileTo(.codegen,
        \\identity :: func(x: u16) u16 { return x }
        \\
        \\main :: func() u16 {
        \\    return identity(42)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i16 @identity(i16 %arg0)");
    try r.expectLLVMContains("ret i16");
}

test "u32 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: u32, b: u32) u32 {
        \\    return a + b
        \\}
        \\
        \\main :: func() u32 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i32 @add(i32 %arg0, i32 %arg1)");
    try r.expectLLVMContains("add i32");
}

test "i64 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: i64, b: i64) i64 {
        \\    return a + b
        \\}
        \\
        \\main :: func() i64 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i64 @add(i64 %arg0, i64 %arg1)");
    try r.expectLLVMContains("add i64");
    try r.expectLLVMContains("ret i64");
}

test "u64 function parameter and return" {
    var r = try compileTo(.codegen,
        \\identity :: func(x: u64) u64 { return x }
        \\
        \\main :: func() u64 {
        \\    return identity(42)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i64 @identity(i64 %arg0)");
    try r.expectLLVMContains("ret i64");
}

test "f16 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: f16, b: f16) f16 {
        \\    return a + b
        \\}
        \\
        \\main :: func() f16 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc half @add(half %arg0, half %arg1)");
    try r.expectLLVMContains("fadd half");
    try r.expectLLVMContains("ret half");
}

test "bool local variable and branch" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    flag: bool = true
        \\    if flag {
        \\        return 1
        \\    }
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("alloca i8");
    try r.expectLLVMContains("store i8");
    try r.expectLLVMContains("load i8");
    try r.expectLLVMContains("icmp ne i8");
}

test "comparison produces i8 bool" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    x: i32 = 5
        \\    y: i32 = 3
        \\    if x > y {
        \\        return 1
        \\    }
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // comparison on i32 operands
    try r.expectLLVMContains("icmp sgt i32");
    // result zext'd to i8 (bool), not i32
    try r.expectLLVMContains("zext i1 %cmp");
    // branch condition uses i8
    try r.expectLLVMContains("icmp ne i8");
}

test "unsigned comparison uses correct predicate" {
    var r = try compileTo(.codegen,
        \\main :: func() u32 {
        \\    x: u32 = 5
        \\    y: u32 = 3
        \\    if x > y {
        \\        return 1
        \\    }
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // unsigned comparison
    try r.expectLLVMContains("icmp ugt i32");
}

test "i8 local variable" {
    var r = try compileTo(.codegen,
        \\main :: func() i8 {
        \\    x: i8 = 42
        \\    return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("alloca i8");
    try r.expectLLVMContains("store i8");
    try r.expectLLVMContains("load i8");
}

test "i16 local variable" {
    var r = try compileTo(.codegen,
        \\main :: func() i16 {
        \\    x: i16 = 1000
        \\    return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("alloca i16");
    try r.expectLLVMContains("store i16");
    try r.expectLLVMContains("load i16");
}

test "mixed width struct fields" {
    var r = try compileTo(.codegen,
        \\Packet :: c struct { tag: u8, len: u16, data: i32 }
        \\
        \\main :: func() i32 {
        \\    p := Packet{ .tag = 1, .len = 100, .data = 42 }
        \\    return p.data
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Packet = type { i8, i16, i32 }");
    try r.expectLLVMContains("store i8");
    try r.expectLLVMContains("store i16");
    try r.expectLLVMContains("store i32");
}
