const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

// ============================================================
// codegen: float types
// ============================================================

test "f32 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: f32, b: f32) f32 {
        \\    return a + b
        \\}
        \\
        \\main :: func() f32 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc float @add(float %arg0, float %arg1)");
    try r.expectLLVMContains("fadd float");
    try r.expectLLVMContains("ret float");
}

test "f64 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: f64, b: f64) f64 {
        \\    return a + b
        \\}
        \\
        \\main :: func() f64 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc double @add(double %arg0, double %arg1)");
    try r.expectLLVMContains("fadd double");
    try r.expectLLVMContains("ret double");
}

test "f32 local variable and literal" {
    var r = try compileTo(.codegen,
        \\identity :: func(x: f32) f32 { return x }
        \\
        \\main :: func() f32 {
        \\    x: f32 = 42
        \\    return identity(x)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("fadd float 0.0, 0x");
    try r.expectLLVMContains("alloca float");
    try r.expectLLVMContains("store float %");
}

test "f32 arithmetic operations" {
    var r = try compileTo(.codegen,
        \\main :: func() f32 {
        \\    a: f32 = 10
        \\    b: f32 = 3
        \\    return a - b
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("load float, ptr %local.");
    try r.expectLLVMContains("fsub float");
}

test "f32 multiply and divide" {
    var r = try compileTo(.codegen,
        \\mul :: func(a: f32, b: f32) f32 {
        \\    return a * b
        \\}
        \\
        \\div :: func(a: f32, b: f32) f32 {
        \\    return a / b
        \\}
        \\
        \\main :: func() f32 {
        \\    return mul(div(6, 2), 3)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("fmul float");
    try r.expectLLVMContains("fdiv float");
}

test "f32 constant via comptime" {
    var r = try compileTo(.codegen,
        \\PI: f32 :: 3
        \\
        \\main :: func() f32 {
        \\    return PI
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("fadd float 0.0, 0x");
    try r.expectLLVMContains("ret float");
}

test "global f32 variable" {
    var r = try compileTo(.codegen,
        \\x: f32 = 10
        \\
        \\main :: func() f32 {
        \\    return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("global float 0x");
    try r.expectLLVMContains("load float, ptr @x");
}

// ============================================================
// codegen: primitive type widths
// ============================================================

test "i8 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: i8, b: i8) i8 {
        \\    return a + b
        \\}
        \\
        \\main :: func() i8 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i8 @add(i8 %arg0, i8 %arg1)");
    try r.expectLLVMContains("add i8");
    try r.expectLLVMContains("ret i8");
}

test "u8 function parameter and return" {
    var r = try compileTo(.codegen,
        \\identity :: func(x: u8) u8 { return x }
        \\
        \\main :: func() u8 {
        \\    return identity(42)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i8 @identity(i8 %arg0)");
    try r.expectLLVMContains("ret i8");
}

test "i16 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: i16, b: i16) i16 {
        \\    return a + b
        \\}
        \\
        \\main :: func() i16 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i16 @add(i16 %arg0, i16 %arg1)");
    try r.expectLLVMContains("add i16");
    try r.expectLLVMContains("ret i16");
}

test "u16 function parameter and return" {
    var r = try compileTo(.codegen,
        \\identity :: func(x: u16) u16 { return x }
        \\
        \\main :: func() u16 {
        \\    return identity(42)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i16 @identity(i16 %arg0)");
    try r.expectLLVMContains("ret i16");
}

test "u32 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: u32, b: u32) u32 {
        \\    return a + b
        \\}
        \\
        \\main :: func() u32 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i32 @add(i32 %arg0, i32 %arg1)");
    try r.expectLLVMContains("add i32");
}

test "i64 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: i64, b: i64) i64 {
        \\    return a + b
        \\}
        \\
        \\main :: func() i64 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i64 @add(i64 %arg0, i64 %arg1)");
    try r.expectLLVMContains("add i64");
    try r.expectLLVMContains("ret i64");
}

test "u64 function parameter and return" {
    var r = try compileTo(.codegen,
        \\identity :: func(x: u64) u64 { return x }
        \\
        \\main :: func() u64 {
        \\    return identity(42)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i64 @identity(i64 %arg0)");
    try r.expectLLVMContains("ret i64");
}

test "f16 function parameter and return" {
    var r = try compileTo(.codegen,
        \\add :: func(a: f16, b: f16) f16 {
        \\    return a + b
        \\}
        \\
        \\main :: func() f16 {
        \\    return add(1, 2)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc half @add(half %arg0, half %arg1)");
    try r.expectLLVMContains("fadd half");
    try r.expectLLVMContains("ret half");
}

test "bool local variable and branch" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    flag: bool = true
        \\    if flag {
        \\        return 1
        \\    }
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("alloca i8");
    try r.expectLLVMContains("store i8");
    try r.expectLLVMContains("load i8");
    try r.expectLLVMContains("icmp ne i8");
}

test "comparison produces i8 bool" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\    x: i32 = 5
        \\    y: i32 = 3
        \\    if x > y {
        \\        return 1
        \\    }
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("icmp sgt i32");
    try r.expectLLVMContains("zext i1 %cmp");
    try r.expectLLVMContains("icmp ne i8");
}

test "unsigned comparison uses correct predicate" {
    var r = try compileTo(.codegen,
        \\main :: func() u32 {
        \\    x: u32 = 5
        \\    y: u32 = 3
        \\    if x > y {
        \\        return 1
        \\    }
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("icmp ugt i32");
}

test "i8 local variable" {
    var r = try compileTo(.codegen,
        \\main :: func() i8 {
        \\    x: i8 = 42
        \\    return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("alloca i8");
    try r.expectLLVMContains("store i8");
    try r.expectLLVMContains("load i8");
}

test "i16 local variable" {
    var r = try compileTo(.codegen,
        \\main :: func() i16 {
        \\    x: i16 = 1000
        \\    return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("alloca i16");
    try r.expectLLVMContains("store i16");
    try r.expectLLVMContains("load i16");
}
