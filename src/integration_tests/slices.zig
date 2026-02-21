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
        \\    return sum(arr[..])
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
        \\    return first(arr[..])
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
        \\length :: func(data: []i32) usize {
        \\    return data.len
        \\}
        \\main :: func() usize {
        \\    arr: [5]i32 = [1, 2, 3, 4, 5]
        \\    return length(arr[..])
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
        \\    return get(arr[..])
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
        \\    return get_at(arr[..], idx)
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
        \\    return sum(arr[..])
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
        \\    return first(arr[..])
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("byval(%slice)");
}

// ============================================================
// codegen: explicit arr[..] creates fat pointer
// ============================================================

test "codegen arr[..] creates fat pointer" {
    var r = try compileTo(.codegen,
        \\sum :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    return sum(arr[..])
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
        \\    return get(arr[..])
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
        \\length :: func(data: []i32) usize {
        \\    return data.len
        \\}
        \\main :: func() usize {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    return length(arr[..])
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
// correct programs: array slicing syntax
// ============================================================

test "array slicing produces slice type" {
    var r = try compileTo(.semantic,
        \\first :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    arr: [5]i32 = [10, 20, 30, 40, 50]
        \\    return first(arr[1..4])
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "slice of slice" {
    var r = try compileTo(.semantic,
        \\inner :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\outer :: func(data: []i32) i32 {
        \\    return inner(data[1..2])
        \\}
        \\main :: func() i32 {
        \\    arr: [5]i32 = [10, 20, 30, 40, 50]
        \\    return outer(arr[..])
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "slice with zero-length range" {
    var r = try compileTo(.semantic,
        \\get_len :: func(data: []i32) usize {
        \\    return data.len
        \\}
        \\main :: func() usize {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    return get_len(arr[2..2])
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// semantic errors: array slicing
// ============================================================

test "slice with non-integer start" {
    var r = try compileTo(.semantic,
        \\first :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    flag: bool = true
        \\    return first(arr[flag..2])
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.index_not_integer);
}

test "slice on non-array type" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    x: i32 = 42
        \\    return x[0..1]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.index_non_array);
}

test "bare array where slice expected is type mismatch" {
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
    try r.expectSemanticError(.argument_type_mismatch);
}

// ============================================================
// codegen: array slicing
// ============================================================

test "codegen array slice emits bounds checks and make_slice" {
    var r = try compileTo(.codegen,
        \\first :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    arr: [5]i32 = [10, 20, 30, 40, 50]
        \\    return first(arr[1..4])
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // bounds checks: end > length and start > end
    try r.expectLLVMContains("icmp ugt i64");
    // ptr_offset for slice start
    try r.expectLLVMContains("getelementptr i8");
    // sub for slice length
    try r.expectLLVMContains("sub i64");
    // fat pointer construction
    try r.expectLLVMContains("store ptr");
    try r.expectLLVMContains("store i64");
}

test "codegen slice-of-slice emits slice_get_ptr and bounds checks" {
    var r = try compileTo(.codegen,
        \\inner :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\outer :: func(data: []i32) i32 {
        \\    return inner(data[1..2])
        \\}
        \\main :: func() i32 {
        \\    arr: [5]i32 = [10, 20, 30, 40, 50]
        \\    return outer(arr[..])
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // slice_get_len for bounds checking
    try r.expectLLVMContains("load i64");
    // bounds check
    try r.expectLLVMContains("icmp ugt i64");
    // slice_get_ptr to extract data pointer
    try r.expectLLVMContains("load ptr");
}

// ============================================================
// correct programs: local slice variables
// ============================================================

test "local slice variable from array slice" {
    var r = try compileTo(.semantic,
        \\first :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    arr: [5]i32 = [10, 20, 30, 40, 50]
        \\    s: []i32 = arr[1..4]
        \\    return first(s)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "local slice variable from full array" {
    var r = try compileTo(.semantic,
        \\first :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    arr: [3]i32 = [10, 20, 30]
        \\    s: []i32 = arr[..]
        \\    return first(s)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "codegen local slice variable stores pointer to fat pointer" {
    var r = try compileTo(.codegen,
        \\first :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    arr: [5]i32 = [10, 20, 30, 40, 50]
        \\    s: []i32 = arr[1..4]
        \\    return first(s)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // slice variable alloca
    try r.expectLLVMContains("alloca { ptr, i64 }");
    // fat pointer construction
    try r.expectLLVMContains("store ptr");
    try r.expectLLVMContains("store i64");
    // passed to function via byval
    try r.expectLLVMContains("byval(%slice)");
}

// ============================================================
// correct programs: global slice variables
// ============================================================

test "global slice variable with runtime init" {
    var r = try compileTo(.codegen,
        \\arr: [3]i32 = [10, 20, 30]
        \\mut s: []i32 = arr[..]
        \\first :: func(data: []i32) i32 {
        \\    return data[0]
        \\}
        \\main :: func() i32 {
        \\    return first(s)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // global slice has zeroinitializer
    try r.expectLLVMContains("{ ptr, i64 } zeroinitializer");
}

// ============================================================
// semantic errors: slices
// ============================================================

test "no implicit array-to-slice coercion in function call" {
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
    try r.expectSemanticError(.argument_type_mismatch);
}

test "no implicit array-to-slice coercion in variable declaration" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    s: []i32 = arr
        \\    return s[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.type_mismatch);
}

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
