const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

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
        \\  return !x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

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
