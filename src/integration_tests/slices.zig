const std = @import("std");
const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

// ============================================================
// correct programs: slice as function parameter
// ============================================================

test "slice parameter type" {
    var r = try compileTo(.semantic,
        \\sum :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    return sum(arr)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "slice parameter with i64 elements" {
    var r = try compileTo(.semantic,
        \\first :: func(data: []i64) i64 {
        \\    return data[0]
        \\}
        \\main :: func() i64 {
        \\    arr: [2]i64 = [10, 20]
        \\    return first(arr)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: slice .len access
// ============================================================

test "slice .len property" {
    var r = try compileTo(.semantic,
        \\length :: func(data: []i32) i64 {
        \\    return data.len
        \\}
        \\main :: func() i64 {
        \\    arr: [5]i32 = [1, 2, 3, 4, 5]
        \\    return length(arr)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: slice indexing
// ============================================================

test "slice indexing with literal" {
    var r = try compileTo(.semantic,
        \\get :: func(data: []i32) i32 {
        \\    return data[2]
        \\}
        \\main :: func() i32 {
        \\    arr: [3]i32 = [10, 20, 30]
        \\    return get(arr)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "slice indexing with variable" {
    var r = try compileTo(.semantic,
        \\get_at :: func(data: []i32, i: i64) i32 {
        \\    return data[i]
        \\}
        \\main :: func() i32 {
        \\    arr: [3]i32 = [10, 20, 30]
        \\    idx: i64 = 1
        \\    return get_at(arr, idx)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// codegen: slice type definition
// ============================================================

test "codegen emits slice type definition" {
    var r = try compileTo(.codegen,
        \\sum :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    return sum(arr)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%slice = type { ptr, i64 }");
}

// ============================================================
// codegen: slice parameter passing (byval)
// ============================================================

test "codegen slice parameter has byval attribute" {
    var r = try compileTo(.codegen,
        \\first :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    return first(arr)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("byval(%slice)");
}

// ============================================================
// codegen: array-to-slice coercion at call site
// ============================================================

test "codegen array-to-slice coercion creates fat pointer" {
    var r = try compileTo(.codegen,
        \\sum :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    return sum(arr)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // should have alloca for the temporary slice fat pointer
    try r.expectLLVMContains("alloca { ptr, i64 }");
    // should store the array pointer and length into the fat pointer
    try r.expectLLVMContains("store ptr");
    try r.expectLLVMContains("store i64");
}

// ============================================================
// codegen: slice indexing
// ============================================================

test "codegen slice indexing extracts data pointer and GEPs" {
    var r = try compileTo(.codegen,
        \\get :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    arr: [3]i32 = [10, 20, 30]
        \\    return get(arr)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // extract data pointer from fat pointer (field 0)
    try r.expectLLVMContains("getelementptr inbounds { ptr, i64 }");
    // GEP into the data array
    try r.expectLLVMContains("getelementptr inbounds i32");
}

// ============================================================
// codegen: slice .len access
// ============================================================

test "codegen slice .len extracts length field" {
    var r = try compileTo(.codegen,
        \\length :: func(data: []i32) i64 {
        \\    return data.len
        \\}
        \\main :: func() i64 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    return length(arr)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // extract length from fat pointer (field 1)
    try r.expectLLVMContains("getelementptr inbounds { ptr, i64 }");
    try r.expectLLVMContains("load i64");
}

// ============================================================
// semantic errors: slices
// ============================================================

test "type mismatch: passing wrong type where slice expected" {
    var r = try compileTo(.semantic,
        \\sum :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    x: bool = true
        \\    return sum(x)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.argument_type_mismatch);
}
