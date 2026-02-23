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

// ============================================================
// usize type
// ============================================================

test "usize type annotation" {
    var r = try compileTo(.semantic,
        \\main :: func() usize {
        \\    x: usize = 42
        \\    return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "usize codegen emits i64" {
    var r = try compileTo(.codegen,
        \\main :: func() usize {
        \\    x: usize = 10
        \\    return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("i64");
}

// ============================================================
// hex literals
// ============================================================

test "hex literal constant" {
    var r = try compileTo(.semantic,
        \\X :: 0xff
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "hex literal variable" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  x: i32 = 0xFF
        \\  return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "hex literal codegen" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\  x: i32 = 0x1A
        \\  return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("26");
}

test "hex literal arithmetic" {
    var r = try compileTo(.semantic,
        \\MASK: i32 :: 0xff
        \\SHIFT: i32 :: 0x10
        \\COMBINED :: MASK + SHIFT
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "usize as function parameter" {
    var r = try compileTo(.semantic,
        \\identity :: func(n: usize) usize {
        \\    return n
        \\}
        \\main :: func() usize {
        \\    return identity(42)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}
