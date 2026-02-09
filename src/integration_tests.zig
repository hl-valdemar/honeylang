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
    if (phase == .lexer) return .{ .arena = arena, .lex = lex, .parse = null, .sem = null };

    // parser
    const parse = try honey.parser.parse(allocator, lex.tokens, &src);
    if (phase == .parser) return .{ .arena = arena, .lex = lex, .parse = parse, .sem = null };

    // semantic
    const sem = try honey.semantic.analyze(allocator, &parse.ast, &lex.tokens, &src);
    return .{ .arena = arena, .lex = lex, .parse = parse, .sem = sem };
}

const Phase = enum { lexer, parser, semantic };

const CompileResult = struct {
    arena: std.heap.ArenaAllocator,
    lex: honey.lexer.LexerResult,
    parse: ?honey.parser.ParseResult,
    sem: ?honey.semantic.SemanticResult,

    fn deinit(self: *CompileResult) void {
        self.arena.deinit();
    }

    fn expectNoErrors(self: *const CompileResult) !void {
        try std.testing.expect(!self.lex.errors.hasErrors());
        if (self.parse) |p| try std.testing.expect(!p.errors.hasErrors());
        if (self.sem) |s| try std.testing.expect(!s.errors.hasErrors());
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
