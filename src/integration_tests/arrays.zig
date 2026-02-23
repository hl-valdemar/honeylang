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
// correct programs: arrays of structs
// ============================================================

test "array of structs with explicit size" {
    var r = try compileTo(.semantic,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\main :: func() i32 {
        \\    points: [2]Point = [Point{ .x = 1, .y = 2 }, Point{ .x = 3, .y = 4 }]
        \\    return points[0].x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "array of structs with inferred size" {
    var r = try compileTo(.semantic,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\main :: func() i32 {
        \\    points: [_]Point = [Point{ .x = 1, .y = 2 }, Point{ .x = 3, .y = 4 }, Point{ .x = 5, .y = 6 }]
        \\    return points[1].y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "mutable array of structs element reassignment" {
    var r = try compileTo(.semantic,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\main :: func() i32 {
        \\    arr: [2]mut Point = [Point{ .x = 1, .y = 2 }, Point{ .x = 3, .y = 4 }]
        \\    arr[0] = Point{ .x = 10, .y = 20 }
        \\    return arr[0].x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// codegen: arrays of structs
// ============================================================

test "codegen array of structs emits alloca and GEP" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\main :: func() i32 {
        \\    points: [2]Point = [Point{ .x = 1, .y = 2 }, Point{ .x = 3, .y = 4 }]
        \\    return points[0].x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Point = type { i32, i32 }");
    try r.expectLLVMContains("alloca [2 x %Point]");
    try r.expectLLVMContains("getelementptr inbounds [2 x %Point]");
}

test "codegen inferred array of structs emits correct alloca size" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\main :: func() i32 {
        \\    points: [_]Point = [Point{ .x = 1, .y = 2 }, Point{ .x = 3, .y = 4 }, Point{ .x = 5, .y = 6 }]
        \\    return points[2].y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("alloca [3 x %Point]");
}
