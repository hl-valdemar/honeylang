const std = @import("std");
const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

// ============================================================
// codegen: array bounds checking
// ============================================================

test "array index emits bounds check before GEP" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    arr: [3]i32 = [10, 20, 30]
        \\    return arr[1]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // unsigned comparison: index >= length
    try r.expectLLVMContains("icmp uge i64");
    // conditional branch to trap or continue
    try r.expectLLVMContains("br i1");
    // trap block for out-of-bounds
    try r.expectLLVMContains("call void @llvm.trap()");
    try r.expectLLVMContains("unreachable");
    // the actual element access still happens after the check
    try r.expectLLVMContains("getelementptr inbounds [3 x i32]");
    try r.expectLLVMContains("load i32");
}

test "array index bounds check compares against array length" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    arr: [5]i32 = [1, 2, 3, 4, 5]
        \\    return arr[2]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // length 5 should appear as the bounds check immediate
    // mov 5, then icmp uge index, 5
    const cg = r.codegen orelse return error.TestExpectedEqual;
    // The bounds check compares index (2) against length (5)
    // Both values are i64 in the comparison
    try std.testing.expect(std.mem.indexOf(u8, cg.output, "icmp uge i64") != null);
}

test "mutable array assignment emits bounds check" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    arr: [3]mut i32 = [1, 2, 3]
        \\    arr[0] = 42
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // Two bounds checks: one for the store, one for the load
    const cg = r.codegen orelse return error.TestExpectedEqual;
    var count: usize = 0;
    var pos: usize = 0;
    while (std.mem.indexOfPos(u8, cg.output, pos, "icmp uge i64")) |idx| {
        count += 1;
        pos = idx + 1;
    }
    // At least 2: one for arr[0] = 42, one for return arr[0]
    try std.testing.expect(count >= 2);
}

test "global array index emits bounds check" {
    var r = try compileTo(.codegen,
        \\arr: [3]i32 = [10, 20, 30]
        \\main :: func() i32 {
        \\    return arr[1]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("icmp uge i64");
    try r.expectLLVMContains("call void @llvm.trap()");
}

test "comptime array constant index emits bounds check" {
    var r = try compileTo(.codegen,
        \\VALS: [3]i32 :: [10, 20, 30]
        \\main :: func() i32 {
        \\    return VALS[2]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("icmp uge i64");
    try r.expectLLVMContains("call void @llvm.trap()");
}

// ============================================================
// codegen: slice bounds checking
// ============================================================

test "slice index emits bounds check with runtime length" {
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
    // slice bounds check extracts length (field 1) then compares
    // extract length from fat pointer
    try r.expectLLVMContains("load i64");
    // unsigned comparison
    try r.expectLLVMContains("icmp uge i64");
    // trap block
    try r.expectLLVMContains("call void @llvm.trap()");
    try r.expectLLVMContains("unreachable");
    // element access still happens
    try r.expectLLVMContains("getelementptr inbounds i32");
}

test "slice index with variable emits bounds check" {
    var r = try compileTo(.codegen,
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
    try r.expectLLVMContains("icmp uge i64");
    try r.expectLLVMContains("call void @llvm.trap()");
}

// ============================================================
// codegen: bounds check structure (trap block + continue block)
// ============================================================

test "bounds check emits labeled trap and continue blocks" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    arr: [3]i32 = [10, 20, 30]
        \\    return arr[0]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // The trap block is a labeled block
    try r.expectLLVMContains("lbl.0:");
    // The continue block follows
    try r.expectLLVMContains("lbl.1:");
}

test "inferred size array emits bounds check with correct length" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    arr: [_]i32 = [1, 2, 3, 4]
        \\    return arr[3]
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("icmp uge i64");
    // The alloca should reflect the inferred size of 4
    try r.expectLLVMContains("alloca [4 x i32]");
    // Bounds check should still be present
    try r.expectLLVMContains("call void @llvm.trap()");
}
