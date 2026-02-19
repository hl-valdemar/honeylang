const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

// ============================================================
// correct programs: array declarations
// ============================================================

test "array type annotation" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "array literal infers element type" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [4]i32 = [10, 20, 30, 40]
        \\    return arr[1]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "array with single element" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [1]i32 = [42]
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "array of f32" {
    var r = try compileTo(.semantic,
        \\main :: func() f32 {
        \\    arr: [2]f32 = [1, 2]
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "array of bool" {
    var r = try compileTo(.semantic,
        \\main :: func() bool {
        \\    arr: [2]bool = [true, false]
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "array index with variable" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [3]i32 = [10, 20, 30]
        \\    i: i32 = 2
        \\    return arr[i]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: mutable array elements
// ============================================================

test "mutable array element assignment" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    mut arr: [3]mut i32 = [1, 2, 3]
        \\    arr[0] = 10
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "mutable elements with immutable binding" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [3]mut i32 = [1, 2, 3]
        \\    arr[0] = 10
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "mutable array compound assignment" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [3]mut i32 = [1, 2, 3]
        \\    arr[0] += 10
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// semantic errors: arrays
// ============================================================

test "array length mismatch" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2]
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.array_length_mismatch);
}

test "index on non-array type" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    x: i32 = 42
        \\    return x[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.index_non_array);
}

test "index with non-integer" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    flag: bool = true
        \\    return arr[flag]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.index_not_integer);
}

test "assign to immutable array element" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    arr[0] = 10
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.assign_to_immutable_element);
}

test "compound assign to immutable array element" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    arr[0] += 10
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.assign_to_immutable_element);
}

// ============================================================
// codegen: array allocation and element access
// ============================================================

test "codegen array literal emits alloca and stores" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    arr: [3]i32 = [10, 20, 30]
        \\    return arr[1]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("alloca [3 x i32]");
    try r.expectLLVMContains("getelementptr inbounds [3 x i32]");
    try r.expectLLVMContains("store i32");
    try r.expectLLVMContains("load i32");
}

test "codegen array of f32" {
    var r = try compileTo(.codegen,
        \\main :: func() f32 {
        \\    arr: [2]f32 = [1, 2]
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("alloca [2 x float]");
    try r.expectLLVMContains("getelementptr inbounds [2 x float]");
    try r.expectLLVMContains("load float");
}

test "codegen array index emits GEP and load" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    arr: [4]i32 = [1, 2, 3, 4]
        \\    return arr[2]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("alloca [4 x i32]");
    try r.expectLLVMContains("getelementptr inbounds [4 x i32]");
}

test "codegen mutable array element store" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    arr: [3]mut i32 = [1, 2, 3]
        \\    arr[0] = 10
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("alloca [3 x i32]");
    try r.expectLLVMContains("store i32");
    try r.expectLLVMContains("getelementptr inbounds [3 x i32]");
}

// ============================================================
// codegen: error recovery traps for arrays
// ============================================================

test "assign to immutable element emits trap" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    arr[0] = 10
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.assign_to_immutable_element);
    try r.expectLLVMContains("call void @llvm.trap()");
    try r.expectLLVMContains("unreachable");
}

test "compound assign to immutable element emits trap" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    arr: [3]i32 = [1, 2, 3]
        \\    arr[0] += 10
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.assign_to_immutable_element);
    try r.expectLLVMContains("call void @llvm.trap()");
    try r.expectLLVMContains("unreachable");
}
