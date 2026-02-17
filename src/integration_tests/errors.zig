const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

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

test "missing return in non-void function" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  if true { return 0 }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.missing_return);
}

test "missing return with else-if but no else" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  if true { return 0 }
        \\  else if false { return 1 }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.missing_return);
}

test "no missing return when all branches return" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  if true { return 0 }
        \\  else { return 1 }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// codegen: error recovery traps
// ============================================================

test "return type mismatch emits trap" {
    var r = try compileTo(.codegen,
        \\Point :: struct { x: f32, y: f32 }
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 1, .y = 2 }
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.return_type_mismatch);
    try r.expectLLVMContains("call void @llvm.trap()");
    try r.expectLLVMContains("unreachable");
}
