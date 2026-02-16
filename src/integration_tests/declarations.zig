const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

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
