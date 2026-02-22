const std = @import("std");
const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

// ============================================================
// correct programs: string literals
// ============================================================

test "string literal type is []u8" {
    var r = try compileTo(.semantic,
        \\main :: func() u8 {
        \\    s := "hello"
        \\    return s[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "string literal passed to function taking []u8" {
    var r = try compileTo(.semantic,
        \\get_first :: func(data: []u8) u8 {
        \\    return data[0]
        \\}
        \\main :: func() u8 {
        \\    return get_first("hello")
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "string literal .len access" {
    var r = try compileTo(.semantic,
        \\get_len :: func(data: []u8) usize {
        \\    return data.len
        \\}
        \\main :: func() usize {
        \\    return get_len("hello")
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "string literal stored in variable" {
    var r = try compileTo(.semantic,
        \\main :: func() u8 {
        \\    s: []u8 = "hello"
        \\    return s[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "string literal slicing" {
    var r = try compileTo(.semantic,
        \\get_first :: func(data: []u8) u8 {
        \\    return data[0]
        \\}
        \\main :: func() u8 {
        \\    s := "hello"
        \\    return get_first(s[1..3])
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// codegen: string literals
// ============================================================

test "codegen string literal emits global constant" {
    var r = try compileTo(.codegen,
        \\get_first :: func(data: []u8) u8 {
        \\    return data[0]
        \\}
        \\main :: func() u8 {
        \\    return get_first("hello")
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // string stored as global byte array (5 chars + null terminator)
    try r.expectLLVMContains("private constant [6 x i8]");
    // individual bytes
    try r.expectLLVMContains("i8 104");  // 'h'
    try r.expectLLVMContains("i8 0");    // null terminator
    // fat pointer construction
    try r.expectLLVMContains("store ptr");
    try r.expectLLVMContains("store i64");
}

test "codegen empty string" {
    var r = try compileTo(.codegen,
        \\get_len :: func(data: []u8) usize {
        \\    return data.len
        \\}
        \\main :: func() usize {
        \\    return get_len("")
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // empty string is just the null terminator
    try r.expectLLVMContains("private constant [1 x i8]");
    try r.expectLLVMContains("i8 0");
}

test "codegen string with escape sequences" {
    var r = try compileTo(.codegen,
        \\main :: func() u8 {
        \\    s := "a\nb"
        \\    return s[1]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // 3 content bytes + null terminator: 'a' (97), '\n' (10), 'b' (98), 0
    try r.expectLLVMContains("private constant [4 x i8]");
    try r.expectLLVMContains("i8 10");  // \n
}

// ============================================================
// codegen: comptime string constants
// ============================================================

test "codegen comptime string constant passed to function" {
    var r = try compileTo(.codegen,
        \\get_first :: func(data: []u8) u8 {
        \\    return data[0]
        \\}
        \\GREETING :: "hello"
        \\main :: func() u8 {
        \\    return get_first(GREETING)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // comptime string constant emitted as global byte array
    try r.expectLLVMContains("private constant [6 x i8]");
    // fat pointer construction (ptr + length)
    try r.expectLLVMContains("store ptr");
    try r.expectLLVMContains("store i64");
}

// ============================================================
// sentinel type: coercion
// ============================================================

test "string literal ([:0]u8) passed where []u8 expected" {
    var r = try compileTo(.semantic,
        \\get_first :: func(data: []u8) u8 {
        \\    return data[0]
        \\}
        \\main :: func() u8 {
        \\    return get_first("hello")
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "sentinel slice type annotation" {
    var r = try compileTo(.semantic,
        \\main :: func() u8 {
        \\    s: [:0]u8 = "hello"
        \\    return s[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "non-sentinel slice cannot be passed where sentinel expected" {
    var r = try compileTo(.semantic,
        \\needs_sentinel :: func(data: [:0]u8) u8 {
        \\    return data[0]
        \\}
        \\main :: func() u8 {
        \\    arr: [3]u8 = [65, 66, 67]
        \\    s: []u8 = arr[..]
        \\    return needs_sentinel(s)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.argument_type_mismatch);
}

// ============================================================
// semantic errors: strings
// ============================================================

test "string literal is immutable" {
    var r = try compileTo(.semantic,
        \\main :: func() u8 {
        \\    s := "hello"
        \\    s[0] = 65
        \\    return s[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.assign_to_immutable_element);
}
