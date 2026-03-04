const std = @import("std");
const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

// ============================================================
// correct programs: named tuple struct declarations
// ============================================================

test "tuple struct declaration" {
    var r = try compileTo(.semantic,
        \\Pair :: struct {i32, i32}
        \\main :: func() i32 {
        \\    p := Pair{10, 20}
        \\    return p.0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "tuple struct with mixed types" {
    var r = try compileTo(.semantic,
        \\Info :: struct {i32, bool}
        \\main :: func() i32 {
        \\    info := Info{42, true}
        \\    return info.0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "tuple struct second field access" {
    var r = try compileTo(.codegen,
        \\Pair :: struct {i32, i32}
        \\main :: func() i32 {
        \\    p := Pair{10, 20}
        \\    return p.1
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("getelementptr inbounds %Pair");
}

// ============================================================
// correct programs: anonymous tuple literals
// ============================================================

test "anonymous tuple literal" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    t := {10, 20}
        \\    return t.0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "anonymous tuple with string" {
    var r = try compileTo(.semantic,
        \\main :: func() u8 {
        \\    t := {42, "hello"}
        \\    return t.0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "anonymous tuple codegen" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    t := {10, 20}
        \\    return t.1
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // anonymous tuple creates a struct type
    try r.expectLLVMContains("alloca %__anon_tuple_");
}

// ============================================================
// codegen: C variadic functions
// ============================================================

test "codegen C variadic declaration emits ellipsis" {
    var r = try compileTo(.codegen,
        \\printf :: c func(fmt: [:0]u8, ...) i32
        \\main :: func() void {
        \\    printf("hello\n")
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("declare i32 @printf(ptr, ...)");
}

test "codegen C variadic call with extra args" {
    var r = try compileTo(.codegen,
        \\printf :: c func(fmt: [:0]u8, ...) i32
        \\main :: func() void {
        \\    x: i32 = 42
        \\    printf("x=%d\n", x)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // variadic call uses explicit function type
    try r.expectLLVMContains("call i32 (ptr, ...) @printf");
}

test "C variadic with string arg extracts data pointer" {
    var r = try compileTo(.codegen,
        \\printf :: c func(fmt: [:0]u8, ...) i32
        \\main :: func() void {
        \\    printf("hello %s\n", "world")
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // string variadic arg should be passed as ptr (not byval slice)
    try r.expectLLVMContains("call i32 (ptr, ...) @printf(ptr");
}

// ============================================================
// semantic: variadic argument checking
// ============================================================

test "variadic function allows extra args" {
    var r = try compileTo(.semantic,
        \\printf :: c func(fmt: [:0]u8, ...) i32
        \\main :: func() void {
        \\    printf("hello %s %d\n", "world", 42)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "variadic function rejects too few fixed args" {
    var r = try compileTo(.semantic,
        \\printf :: c func(fmt: [:0]u8, ...) i32
        \\main :: func() void {
        \\    printf()
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.argument_count_mismatch);
}

test "non-variadic function rejects extra args" {
    var r = try compileTo(.semantic,
        \\add :: func(a: i32, b: i32) i32 {
        \\    return a + b
        \\}
        \\main :: func() i32 {
        \\    return add(1, 2, 3)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.argument_count_mismatch);
}
