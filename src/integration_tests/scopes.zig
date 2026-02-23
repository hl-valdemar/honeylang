const helpers = @import("helpers.zig");
const compileTo = helpers.compileTo;

// ── bare blocks ──

test "bare block creates a new scope" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  a := 1
        \\  {
        \\    b := 2
        \\    return a + b
        \\  }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "variable declared in bare block does not leak to outer scope" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  {
        \\    b := 2
        \\  }
        \\  return b
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.undefined_symbol);
}

test "same name reusable after bare block exits" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  {
        \\    b := 1
        \\  }
        \\  b := 2
        \\  return b
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "nested bare blocks" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  a := 1
        \\  {
        \\    b := 2
        \\    {
        \\      c := 3
        \\      return a + b + c
        \\    }
        \\  }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "bare block codegen" {
    var r = try compileTo(.codegen,
        \\main :: func() i32 {
        \\  a := 1
        \\  {
        \\    b := 2
        \\    return a + b
        \\  }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
    try r.expectLLVMContains("add i32");
}

// ── shadowing ──

test "shadowing in bare block is an error" {
    var r = try compileTo(.semantic,
        \\main :: func() void {
        \\  a := 1
        \\  {
        \\    a := 2
        \\  }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.variable_shadowing);
}

test "shadowing in if block is an error" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  a := 1
        \\  if true {
        \\    a := 2
        \\    return a
        \\  }
        \\  return a
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.variable_shadowing);
}

test "shadowing in deeply nested block is an error" {
    var r = try compileTo(.semantic,
        \\main :: func() void {
        \\  a := 1
        \\  {
        \\    {
        \\      a := 2
        \\    }
        \\  }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.variable_shadowing);
}

test "no shadowing across sibling blocks" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  {
        \\    a := 1
        \\  }
        \\  {
        \\    a := 2
        \\    return a
        \\  }
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}

test "function parameter shadowed by local is an error" {
    var r = try compileTo(.semantic,
        \\main :: func(x: i32) i32 {
        \\  x := 5
        \\  return x
        \\}
        \\
    );
    defer r.deinit();
    try r.expectSemanticError(.variable_shadowing);
}

// ── defer in bare blocks ──

test "defer in bare block" {
    var r = try compileTo(.semantic,
        \\main :: func() i32 {
        \\  {
        \\    defer x := 10
        \\  }
        \\  return 42
        \\}
        \\
    );
    defer r.deinit();
    try r.expectNoErrors();
}
