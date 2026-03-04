const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

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
