const std = @import("std");
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
// correct programs: inferred array size [_]T
// ============================================================

test "infer array size from literal" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [_]i32 = [1, 2, 3]
        \\    return arr[2]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "infer array size with mutable elements" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [_]mut i32 = [10, 20]
        \\    arr[0] = 5
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "infer array size single element" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    arr: [_]i32 = [42]
        \\    return arr[0]
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

// ============================================================
// codegen: inferred array size [_]T
// ============================================================

test "codegen inferred array emits correct alloca size" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    arr: [_]i32 = [1, 2, 3, 4]
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("alloca [4 x i32]");
}

test "codegen inferred mutable array emits store" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    arr: [_]mut i32 = [10, 20, 30]
        \\    arr[0] = 5
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("alloca [3 x i32]");
    try r.expectLLVMContains("getelementptr inbounds [3 x i32]");
}

// ============================================================
// codegen: local arrays use direct [N x T] storage (no ptr wrapper)
// ============================================================

test "local array has no alloca ptr indirection" {
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
    // Should NOT have an `alloca ptr` — the local IS the array
    const cg = r.codegen orelse return error.TestExpectedEqual;
    try std.testing.expect(std.mem.indexOf(u8, cg.output, "alloca ptr") == null);
}

// ============================================================
// correct programs: global arrays
// ============================================================

test "global array with static initializer" {
    var r = try compileTo(.codegen,
        \\arr: [3]i32 = [10, 20, 30]
        \\main :: func() i32 {
        \\    return arr[1]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("@arr = global [3 x i32] [i32 10, i32 20, i32 30]");
}

test "global mutable array" {
    var r = try compileTo(.codegen,
        \\mut arr: [3]mut i32 = [1, 2, 3]
        \\main :: func() i32 {
        \\    arr[0] = 42
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("@arr = global [3 x i32] [i32 1, i32 2, i32 3]");
    try r.expectLLVMContains("getelementptr inbounds [3 x i32]");
}

test "global array with inferred size" {
    var r = try compileTo(.codegen,
        \\arr: [_]i32 = [5, 10, 15, 20]
        \\main :: func() i32 {
        \\    return arr[3]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("@arr = global [4 x i32] [i32 5, i32 10, i32 15, i32 20]");
}

// ============================================================
// correct programs: comptime array constants
// ============================================================

test "comptime array constant" {
    var r = try compileTo(.codegen,
        \\ARR: [3]i32 :: [10, 20, 30]
        \\main :: func() i32 {
        \\    return ARR[2]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("@ARR = constant [3 x i32] [i32 10, i32 20, i32 30]");
}

test "comptime array constant with inferred size" {
    var r = try compileTo(.codegen,
        \\VALS: [_]i32 :: [1, 2, 3, 4, 5]
        \\main :: func() i32 {
        \\    return VALS[4]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("@VALS = constant [5 x i32] [i32 1, i32 2, i32 3, i32 4, i32 5]");
}

test "comptime f32 array constant" {
    var r = try compileTo(.codegen,
        \\VALS: [2]f32 :: [1, 2]
        \\main :: func() f32 {
        \\    return VALS[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("@VALS = constant [2 x float]");
}
