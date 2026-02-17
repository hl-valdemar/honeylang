const std = @import("std");
const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

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
// many-item pointers
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
    try r.expectLLVMContains("mul i64 %");
    try r.expectLLVMContains(", 4");
}

// ============================================================
// semantic errors: pointers
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

test "many-item to single-item pointer coercion is allowed (tightening)" {
    var r = try compileTo(.semantic,
        \\main :: func() void {
        \\    mut x: i32 = 42
        \\    p: *i32 = &x
        \\    q: @i32 = p
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "single-item to many-item pointer coercion is rejected (loosening)" {
    var r = try compileTo(.semantic,
        \\main :: func() void {
        \\    mut x: i32 = 42
        \\    p: @i32 = &x
        \\    q: *i32 = p
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.type_mismatch);
}
