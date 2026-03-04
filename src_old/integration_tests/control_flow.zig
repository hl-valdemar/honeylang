const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

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
