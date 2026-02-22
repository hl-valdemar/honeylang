const std = @import("std");
const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

// ============================================================
// correct programs: basic while loops
// ============================================================

test "basic while loop" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut i := 0
        \\    while i < 10 {
        \\        i += 1
        \\    }
        \\    return i
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // condition check with branch
    try r.expectLLVMContains("br i1");
    // back-edge to loop header
    try r.expectLLVMContains("br label %lbl.0");
}

test "while loop with break" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut i := 0
        \\    while i < 100 {
        \\        if i == 5 {
        \\            break
        \\        }
        \\        i += 1
        \\    }
        \\    return i
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "while loop with continue" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut sum := 0
        \\    mut i: i32 = 0
        \\    while i < 10 {
        \\        i += 1
        \\        if i == 5 {
        \\            continue
        \\        }
        \\        sum += i
        \\    }
        \\    return sum
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: while with continue expression
// ============================================================

test "while loop with continue expression" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut sum := 0
        \\    mut i: i32 = 0
        \\    while i < 5 : i += 1 {
        \\        sum += i
        \\    }
        \\    return sum
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "continue with continue expression executes cont expr" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut sum := 0
        \\    mut i: i32 = 0
        \\    while i < 10 : i += 1 {
        \\        mut half := i / 2
        \\        if half * 2 == i {
        \\            continue
        \\        }
        \\        sum += i
        \\    }
        \\    return sum
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// correct programs: edge cases
// ============================================================

test "while with not condition" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut done := false
        \\    mut i := 0
        \\    while not done {
        \\        i += 1
        \\        if i == 5 {
        \\            done = true
        \\        }
        \\    }
        \\    return i
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "while with empty body" {
    var r = try compileTo(.codegen,
        \\main :: func() void {
        \\    while false {
        \\    }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "while with function call condition" {
    var r = try compileTo(.codegen,
        \\check :: func() bool {
        \\    return false
        \\}
        \\main :: func() void {
        \\    while check() {
        \\    }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "nested while loops with break" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    mut total := 0
        \\    mut i: i32 = 0
        \\    while i < 3 : i += 1 {
        \\        mut j: i32 = 0
        \\        while j < 10 {
        \\            if j == 2 {
        \\                break
        \\            }
        \\            j += 1
        \\        }
        \\        total += j
        \\    }
        \\    return total
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// semantic errors: break/continue outside loop
// ============================================================

test "break outside loop is error" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    break
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.break_outside_loop);
}

test "continue outside loop is error" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\    continue
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.continue_outside_loop);
}

// ============================================================
// semantic: condition must be bool
// ============================================================

test "while condition must be boolean" {
    var r = try compileTo(.semantic,
        \\main :: func() void {
        \\    while 1 == 1 {
        \\        break
        \\    }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}
