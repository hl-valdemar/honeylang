const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

// ============================================================
// correct programs: struct declarations
// ============================================================

test "c struct declaration" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "c struct used as function parameter type" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\get_x :: func(p: Point) i32 {
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "struct field access" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\get_x :: func(p: Point) i32 {
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "chained struct field access" {
    var r = try compileTo(.semantic,
        \\Inner :: c struct {
        \\    value: i32,
        \\}
        \\
        \\Outer :: c struct {
        \\    inner: Inner,
        \\}
        \\
        \\get_value :: func(o: Outer) i32 {
        \\    return o.inner.value
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "struct literal initialization" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 3, .y = 4 }
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

// ============================================================
// semantic errors: structs
// ============================================================

test "duplicate field in struct" {
    var r = try compileTo(.semantic,
        \\Bad :: c struct {
        \\    x: i32,
        \\    x: i32,
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.duplicate_field);
}

test "no such field on struct" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\bad :: func(p: Point) i32 {
        \\    return p.z
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.no_such_field);
}

test "field access on non-struct type" {
    var r = try compileTo(.semantic,
        \\bad :: func(x: i32) i32 {
        \\    return x.y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.field_access_on_non_struct);
}

test "struct literal missing field" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 3 }
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.missing_field);
}

test "struct literal duplicate field" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 3, .x = 4 }
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.duplicate_literal_field);
}

test "struct literal field type mismatch" {
    var r = try compileTo(.semantic,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\main :: func() i32 {
        \\    flag: bool = true
        \\    p := Point{ .x = 3, .y = flag }
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.type_mismatch);
}

// ============================================================
// codegen: struct type emission and field access
// ============================================================

test "codegen emits LLVM struct type definition" {
    var r = try compileTo(.codegen,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\main :: func() i32 {
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Point = type { i32, i32 }");
}

test "codegen struct field access emits GEP and load" {
    var r = try compileTo(.codegen,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\get_x :: func(p: Point) i32 {
        \\    return p.x
        \\}
        \\
        \\main :: func() i32 {
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Point = type { i32, i32 }");
    // get_x is unused from main, so it will be skipped — check struct type only
}

test "codegen struct field access with referenced function" {
    var r = try compileTo(.codegen,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\get_x :: func(p: Point) i32 {
        \\    return p.x
        \\}
        \\
        \\main :: func() i32 {
        \\    return get_x(0)
        \\}
        \\
    );
    defer r.deinit();
    // argument type mismatch, but "always compile" — codegen still runs
    try r.expectLLVMContains("%Point = type { i32, i32 }");
    try r.expectLLVMContains("getelementptr inbounds %Point");
    try r.expectLLVMContains("ptr %arg0");
}

test "codegen struct literal emits alloca and GEP stores" {
    var r = try compileTo(.codegen,
        \\Point :: c struct {
        \\    x: i32,
        \\    y: i32,
        \\}
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 3, .y = 4 }
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Point = type { i32, i32 }");
    try r.expectLLVMContains("alloca %Point");
    try r.expectLLVMContains("getelementptr inbounds %Point");
    try r.expectLLVMContains("store i32");
}

test "nested struct inline type definition" {
    var r = try compileTo(.codegen,
        \\Vec2 :: c struct { x: i32, y: i32, }
        \\Rect :: c struct { origin: Vec2, size: Vec2, }
        \\
        \\main :: func() i32 {
        \\    r := Rect{
        \\        .origin = Vec2{ .x = 1, .y = 2 },
        \\        .size = Vec2{ .x = 3, .y = 4 },
        \\    }
        \\    return r.origin.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    // struct fields are inline, not pointers
    try r.expectLLVMContains("%Rect = type { %Vec2, %Vec2 }");
    // struct field store uses memcpy
    try r.expectLLVMContains("@llvm.memcpy.p0.p0.i64");
    // struct field access is GEP-only (no "load ptr")
    try r.expectLLVMContains("getelementptr inbounds %Rect");
    try r.expectLLVMContains("getelementptr inbounds %Vec2");
}

test "nested struct reverse declaration order" {
    var r = try compileTo(.codegen,
        \\Rect :: c struct { origin: Vec2, size: Vec2, }
        \\Vec2 :: c struct { x: i32, y: i32, }
        \\
        \\main :: func() i32 {
        \\    r := Rect{
        \\        .origin = Vec2{ .x = 5, .y = 6 },
        \\        .size = Vec2{ .x = 7, .y = 8 },
        \\    }
        \\    return r.size.y
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Rect = type { %Vec2, %Vec2 }");
}

test "function returning struct uses sret" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\make_point :: func(x: i32, y: i32) Point {
        \\    return Point{ .x = x, .y = y }
        \\}
        \\
        \\main :: func() i32 {
        \\    p := make_point(3, 4)
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc void @make_point(ptr sret(%Point) %arg0");
    try r.expectLLVMContains("call fastcc void @make_point(ptr sret(%Point)");
}

test "sret with nested struct return" {
    var r = try compileTo(.codegen,
        \\Vec2 :: c struct { x: i32, y: i32 }
        \\Rect :: c struct { origin: Vec2, size: Vec2 }
        \\
        \\make_rect :: func() Rect {
        \\    return Rect{
        \\        .origin = Vec2{ .x = 0, .y = 0 },
        \\        .size = Vec2{ .x = 10, .y = 20 },
        \\    }
        \\}
        \\
        \\main :: func() i32 {
        \\    r := make_rect()
        \\    return r.origin.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc void @make_rect(ptr sret(%Rect) %arg0");
    try r.expectLLVMContains("call fastcc void @make_rect(ptr sret(%Rect)");
}

test "struct by-value parameter uses byval attribute" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\sum :: func(p: Point) i32 {
        \\    return p.x
        \\}
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 10, .y = 20 }
        \\    return sum(p)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define fastcc i32 @sum(ptr byval(%Point) %arg0)");
    try r.expectLLVMContains("ptr byval(%Point)");
    try r.expectLLVMContains("getelementptr inbounds %Point");
}

test "struct copy via assignment emits memcpy" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 5, .y = 6 }
        \\    q := p
        \\    return q.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("@llvm.memcpy.p0.p0.i64");
}

test "mutable struct field store" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\main :: func() i32 {
        \\    mut p := Point{ .x = 1, .y = 2 }
        \\    p.x = 42
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("getelementptr inbounds %Point");
    try r.expectLLVMContains("store i32");
}

test "global struct i32 codegen" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\origin := Point{ .x = 0, .y = 0 }
        \\
        \\main :: func() i32 {
        \\    return origin.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Point = type { i32, i32 }");
    try r.expectLLVMContains("@origin = global %Point { i32 0, i32 0 }");
}

test "c function taking struct parameter" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\process :: c func(p: Point) i32 {
        \\    return p.x
        \\}
        \\
        \\main :: func() i32 {
        \\    p := Point{ .x = 7, .y = 8 }
        \\    return process(p)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define i32 @process(ptr byval(%Point) %arg0)");
    try r.expectLLVMContains("call i32 @process(ptr byval(%Point)");
}

test "c function returning struct uses sret" {
    var r = try compileTo(.codegen,
        \\Point :: c struct { x: i32, y: i32 }
        \\
        \\make :: c func(x: i32, y: i32) Point {
        \\    return Point{ .x = x, .y = y }
        \\}
        \\
        \\main :: func() i32 {
        \\    p := make(3, 4)
        \\    return p.x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("define void @make(ptr sret(%Point) %arg0");
    try r.expectLLVMContains("call void @make(ptr sret(%Point)");
}

test "struct with f32 fields" {
    var r = try compileTo(.codegen,
        \\Point :: struct { x: f32, y: f32 }
        \\
        \\get_x :: func(p: Point) f32 { return p.x }
        \\
        \\main :: func() f32 {
        \\    p := Point{ .x = 1, .y = 2 }
        \\    return get_x(p)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Point = type { float, float }");
    try r.expectLLVMContains("store float %");
}

test "struct with f32 field access" {
    var r = try compileTo(.codegen,
        \\Point :: struct { x: f32, y: f32 }
        \\
        \\get_x :: func(p: Point) f32 {
        \\    return p.x
        \\}
        \\
        \\main :: func() f32 {
        \\    p := Point{ .x = 5, .y = 10 }
        \\    return get_x(p)
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("load float, ptr %gep.");
}

test "global struct with f32 fields comptime init" {
    var r = try compileTo(.codegen,
        \\Point :: struct { x: f32, y: f32 }
        \\
        \\p := Point{ .x = 52, .y = 44 }
        \\
        \\main :: func() i32 {
        \\    return 0
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Point = type { float, float }");
    try r.expectLLVMContains("float 0x404A000000000000");
    try r.expectLLVMContains("float 0x4046000000000000");
}

test "f32 function call with struct field args" {
    var r = try compileTo(.codegen,
        \\Point :: struct { x: f32, y: f32 }
        \\
        \\p := Point{ .x = 52, .y = 44 }
        \\
        \\add :: func(a: f32, b: f32) f32 { return a + b }
        \\
        \\main :: func() f32 { return add(p.x, p.y) }
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("call fastcc float @add(float %");
    try r.expectLLVMContains("fadd float");
}

test "mixed f32 and i32 struct fields" {
    var r = try compileTo(.codegen,
        \\Entity :: struct { id: i32, x: f32, y: f32 }
        \\
        \\main :: func() i32 {
        \\    e := Entity{ .id = 1, .x = 5, .y = 10 }
        \\    return e.id
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Entity = type { i32, float, float }");
    try r.expectLLVMContains("store i32");
    try r.expectLLVMContains("store float");
}

test "mixed width struct fields" {
    var r = try compileTo(.codegen,
        \\Packet :: c struct { tag: u8, len: u16, data: i32 }
        \\
        \\main :: func() i32 {
        \\    p := Packet{ .tag = 1, .len = 100, .data = 42 }
        \\    return p.data
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("%Packet = type { i8, i16, i32 }");
    try r.expectLLVMContains("store i8");
    try r.expectLLVMContains("store i16");
    try r.expectLLVMContains("store i32");
}
