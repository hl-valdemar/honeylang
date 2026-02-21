# Honey Language — Current Compiler Support

This document describes what the Honey compiler can currently parse, analyze, and generate code for. Every feature listed here is backed by an integration test in `src/integration_tests/`. Features described in the full language specification (`docs/honeylang-specification.md`) but absent from this document are not yet implemented.

## Compiler Pipeline

```
Source (.hon) → Lexer → Parser → Semantic Analysis → Comptime Evaluation → Code Generation (LLVM IR) → Executable
```

Each phase collects errors and continues processing. Even when errors are present, the compiler produces an executable — error sites are replaced with trap instructions that crash at runtime with a clear message. This is the **always-compile philosophy**.

## Comments

Single-line comments use `#`:

```honey
# This is a comment
X :: 42  # inline comment
```

## Primitive Types

| Type | Description |
| ---- | ----------- |
| `i8`, `i16`, `i32`, `i64` | Signed integers |
| `u8`, `u16`, `u32`, `u64` | Unsigned integers |
| `f16`, `f32`, `f64` | Floating-point numbers |
| `bool` | Boolean (`true` / `false`) |
| `void` | No value |

All widths are fully supported through declaration, function parameters, return types, and code generation.

## Declarations

### Compile-Time Constants

Declared with `::`, evaluated at compile time:

```honey
X :: 42
DEBUG :: true
RELEASE :: false
Y: f32 :: 0.4
```

Constants can reference other constants:

```honey
X :: -42
Y: f32 :: 0.4
Z :: X * Y
```

### Runtime Variables

Immutable by default:

```honey
x: i32 = 42
```

Mutable with `mut`:

```honey
mut x := 10
```

### Globals

Both constants and runtime variables can be declared at file scope:

```honey
FACTOR :: 2
mut counter := 0
x: f32 = 10
```

Constants declared at file scope are accessible from within functions:

```honey
FACTOR :: 2

double :: func(x: i32) i32 {
    return x * FACTOR
}
```

### Type Annotations

Types can be explicit or inferred:

```honey
a: i8 :: 1       # explicit type on constant
b: i16 :: 2
x: i32 = 42      # explicit type on variable
mut y := 10      # type inferred
```

## Functions

### Declaration

```honey
name :: func(param1: Type1, param2: Type2) ReturnType {
    body
}
```

### Simple Functions

```honey
main :: func() i32 {
    return 42
}
```

### Void Functions

```honey
noop :: func() void {
    return
}
```

### Parameters

```honey
add :: func(a: i32, b: i32) i32 {
    return a + b
}

add3 :: func(a: i32, b: i32, c: i32) i32 {
    return a + b + c
}
```

### Function Calls

```honey
add :: func(a: i32, b: i32) i32 {
    return a + b
}

main :: func() i32 {
    return add(40, 2)
}
```

Functions can call each other, including nested calls:

```honey
double :: func(x: i32) i32 {
    return x + x
}

quadruple :: func(x: i32) i32 {
    return double(double(x))
}
```

### C Calling Convention

External C function (no body — defined elsewhere):

```honey
exit :: c func(code: i32) void
```

C function with Honey implementation:

```honey
process :: c func(p: Point) i32 {
    return p.x
}
```

## Operators

### Arithmetic

Works on integer and floating-point types:

```honey
x + y    # addition
x - y    # subtraction
x * y    # multiplication
x / y    # division
```

### Unary

```honey
X :: -42       # negation
not flag       # logical not (bool only)
```

### Comparison

Returns `bool`. Uses signed, unsigned, or float comparison as appropriate:

```honey
x == y    # equal
x != y    # not equal
x < y     # less than
x > y     # greater than
x <= y    # less or equal
x >= y    # greater or equal
```

### Logical

Operands must be `bool` — no implicit truthiness:

```honey
a and b
a or b
not a
```

### Compound Assignment

Requires `mut` variable:

```honey
mut x := 3
x += 4
x -= 3
x *= 2
x /= 2
```

## Control Flow

### If Statements

Condition must be `bool`:

```honey
if SKIP {
    return 1
}
```

### If / Else

```honey
if x {
    return 1
} else {
    return 0
}
```

### If / Else If / Else

```honey
if x == 1 {
    return 10
} else if x == 2 {
    return 20
} else {
    return 30
}
```

The compiler detects missing returns — if a non-void function has branches that don't all return, a `missing_return` error is emitted:

```honey
# ERROR: missing return (no else branch)
main :: func() i32 {
    if true { return 0 }
}

# OK: all branches return
main :: func() i32 {
    if true { return 0 }
    else { return 1 }
}
```

### Defer

Deferred statements execute before the function returns:

```honey
main :: func() i32 {
    defer y := 10
    return 42
}
```

## Structs

### Declaration

Honey layout (compiler may reorder fields):

```honey
Point :: struct { x: f32, y: f32 }
```

C-compatible layout (guaranteed field order, C ABI padding):

```honey
Point :: c struct {
    x: i32,
    y: i32,
}
```

### Struct Literals

All fields must be provided with named initialization:

```honey
p := Point{ .x = 3, .y = 4 }
```

### Field Access

```honey
get_x :: func(p: Point) i32 {
    return p.x
}
```

### Chained / Nested Access

```honey
Inner :: c struct { value: i32 }
Outer :: c struct { inner: Inner }

get_value :: func(o: Outer) i32 {
    return o.inner.value
}
```

### Nested Struct Initialization

```honey
Vec2 :: c struct { x: i32, y: i32 }
Rect :: c struct { origin: Vec2, size: Vec2 }

main :: func() i32 {
    r := Rect{
        .origin = Vec2{ .x = 1, .y = 2 },
        .size = Vec2{ .x = 3, .y = 4 },
    }
    return r.origin.x
}
```

Forward references are supported — structs can be used before they're declared:

```honey
Rect :: c struct { origin: Vec2, size: Vec2 }
Vec2 :: c struct { x: i32, y: i32 }
```

### Struct Copy

Assigning one struct to another creates a copy:

```honey
p := Point{ .x = 5, .y = 6 }
q := p       # q is a copy of p
```

### Mutable Struct Fields

```honey
mut p := Point{ .x = 1, .y = 2 }
p.x = 42
```

### Functions Returning Structs

```honey
make_point :: func(x: i32, y: i32) Point {
    return Point{ .x = x, .y = y }
}

main :: func() i32 {
    p := make_point(3, 4)
    return p.x
}
```

### Struct By-Value Parameters

```honey
sum :: func(p: Point) i32 {
    return p.x
}
```

### Global Struct Instances

```honey
Point :: c struct { x: i32, y: i32 }

origin := Point{ .x = 0, .y = 0 }

main :: func() i32 {
    return origin.x
}
```

### Mixed-Width and Mixed-Type Fields

```honey
Packet :: c struct { tag: u8, len: u16, data: i32 }
Entity :: struct { id: i32, x: f32, y: f32 }
```

## Arrays

Fixed-size arrays with a length known at compile time.

### Declaration and Initialization

Array type syntax is `[N]T` where `N` is the length and `T` is the element type. Array literals use `[elem1, elem2, ...]`:

```honey
arr: [3]i32 :: [10, 20, 30]  # comptime
arr: [3]i32 = [10, 20, 30]   # runtime
```

Works with any element type:

```honey
floats: [2]f32 = [1, 2]
flags: [2]bool = [true, false]
```

### Size Inference

Use `[_]T` to infer the array length from the literal:

```honey
arr: [_]i32 = [1, 2, 3]       # inferred as [3]i32
big: [_]f32 = [1, 2, 3, 4, 5] # inferred as [5]f32
```

Works with mutable elements:

```honey
arr: [_]mut i32 = [10, 20]    # inferred as [2]mut i32
arr[0] = 5
```

Size inference only applies to declarations with an immediate array literal assignment.

### Indexing

Access elements with `arr[index]`. The index must be an integer type:

```honey
main :: func() i32 {
    arr: [4]i32 = [1, 2, 3, 4]
    return arr[1]
}
```

Indexing with a variable:

```honey
main :: func() i32 {
    arr: [3]i32 = [10, 20, 30]
    i: i32 = 2
    return arr[i]
}
```

### Element Mutability

By default, array elements are immutable — `[N]T` does not allow element assignment. Use `[N]mut T` to declare an array with mutable elements:

| Type | Element write | Variable reassign |
| ---- | ------------- | ----------------- |
| `arr: [N]T` or `arr: [_]T` | Not allowed | Not allowed |
| `arr: [N]mut T` or `arr: [_]mut T` | Allowed | Not allowed |
| `mut arr: [N]T` or `mut arr: [_]T` | Not allowed | Allowed |
| `mut arr: [N]mut T` or `mut arr: [_]mut T` | Allowed | Allowed |

Element mutability is controlled by the type (`[N]mut T`), independent of the variable binding, which is consistent with pointer dereference — `p: @mut i32` allows `p^ = 10` without requiring `mut p`.

```honey
# Mutable elements, immutable binding
arr: [3]mut i32 = [1, 2, 3]
arr[0] = 10     # OK — elements are mutable
arr[1] = 20     # OK
# arr = [4, 5, 6]  # ERROR — variable is not mut
```

```honey
# Both mutable elements and mutable binding
mut arr: [3]mut i32 = [1, 2, 3]
arr[0] = 10     # OK
arr = [4, 5, 6] # OK
```

### Compound Assignment on Elements

Works with `[N]mut T`:

```honey
arr: [3]mut i32 = [1, 2, 3]
arr[0] += 10
arr[1] *= 2
```

### Errors

The compiler checks:
- **Length mismatch:** `arr: [3]i32 = [1, 2]` — literal has fewer/more elements than the type requires
- **Index on non-array:** `x[0]` where `x` is `i32` — only arrays can be indexed
- **Non-integer index:** `arr[flag]` where `flag` is `bool` — index must be an integer
- **Immutable element assignment:** `arr[0] = 10` on `[N]T` — use `[N]mut T` for mutable elements

## Pointers

### Single-Item Pointers

Point to exactly one value. No arithmetic allowed.

```honey
mut x := 42
p := &x          # p is @mut i32
p^ = 100         # dereference and write
return x          # x is now 100
```

### Type Annotations

```honey
mut x := 10
p: @mut i32 = &x   # mutable pointer
q: @i32 = &x       # immutable pointer
return q^           # dereference
```

### Pointer as Function Parameter

```honey
set_value :: func(p: @mut i32, val: i32) void {
    p^ = val
}

main :: func() i32 {
    mut x := 0
    set_value(&x, 42)
    return x
}
```

### Compound Assignment Through Pointer

```honey
mut x := 42
p := &x
p^ += 100
```

### Pointer to Struct

```honey
get_x :: func(p: @Point) i32 { return p^.x }

main :: func() i32 {
    pt := Point{ .x = 7, .y = 3 }
    return get_x(&pt)
}
```

### Address-of Struct Field

```honey
mut buf := Buf{ .a = 10, .b = 20 }
p: @mut i32 = &buf.a
return p^
```

### Returning Pointers

```honey
main :: func() @i32 {
    mut x := 42
    return &x
}
```

### Many-Item Pointers

Point to multiple items. Support arithmetic with element-size scaling.

```honey
mut x: i32 = 10
p: *mut i32 = &x
q: *i32 = p + 1      # advances by sizeof(i32)
```

Subtraction:

```honey
r: *i32 = q - 1
```

Commutative addition:

```honey
q: *i32 = 2 + p      # same as p + 2
```

### Many-Item Pointer as Function Parameter

```honey
read_at :: func(base: *i32, offset: i32) i32 {
    p: *i32 = base + offset
    return p^
}
```

### Pointer Coercion Rules

**Tightening** (`*T` → `@T`) is allowed — a many-item pointer can be used where a single-item pointer is expected:

```honey
p: *i32 = &x
q: @i32 = p      # OK
```

**Loosening** (`@T` → `*T`) is rejected — a single-item pointer cannot widen to many-item:

```honey
p: @i32 = &x
q: *i32 = p      # ERROR: type mismatch
```

**Single-item pointer arithmetic** is rejected:

```honey
p: @mut i32 = &x
bad: i32 = p + 1   # ERROR: pointer_arithmetic
```

## Imports

### Honey File Imports

```honey
import "utils.hon"               # bare import
utils :: import "utils.hon"      # named import
```

### C Header Imports

```honey
import c "math.h"               # bare C import
math :: import c "math.h"       # named C import
```

### C Import Blocks

For multiple includes and preprocessor defines (name required):

```honey
math :: import c {
    include "math.h"
    include "util.h"
    define "DEBUG"
    define "PI 3.14"
}
```

Bare C import blocks (without a name) produce a parse error.

## Error Detection

The compiler detects errors at each phase and continues processing.

### Lexer Errors

| Error | Example |
| ----- | ------- |
| `unexpected_character` | `a :: 10 $ 20` |
| `multiple_decimal_points` | `pi :: 3.14.159` |

### Parse Errors

| Error | Example |
| ----- | ------- |
| `unclosed_brace` | `main :: func() i32 { return 42` (missing `}`) |
| `expected_expression` | `x ::` (nothing after `::`) |
| `expected_right_paren` | `add(1, 2` (missing `)`) |
| `expected_left_paren` | `main :: func i32 { ... }` (missing `(`) |
| `expected_declaration` | `x 42` (missing `::`) |
| `expected_identifier` | `main :: func(` (unexpected EOF) |

### Semantic Errors

**Type System:**

| Error | Example |
| ----- | ------- |
| `type_mismatch` | `flag: bool = 1`, `y: u32 = x` (where x is i32) |
| `unknown_type` | `x: foo :: 42` |

**Symbols:**

| Error | Example |
| ----- | ------- |
| `duplicate_symbol` | `X :: 1` then `X :: 2` |
| `undefined_symbol` | `return y` (y not declared) |

**Functions:**

| Error | Example |
| ----- | ------- |
| `argument_count_mismatch` | `add(1)` (expects 2 args) |
| `argument_type_mismatch` | `add(true, 1)` (expects i32) |
| `return_type_mismatch` | Returning `bool` from `func() i32` |
| `not_callable` | `X(1)` where `X :: 42` |
| `missing_return` | Non-void function without return on all paths |

**Operators:**

| Error | Example |
| ----- | ------- |
| `arithmetic_op_requires_numeric` | `true + false` |
| `logical_op_requires_bool` | `1 and 2`, `not 1` |

**Mutability & Control Flow:**

| Error | Example |
| ----- | ------- |
| `assignment_to_immutable` | `x := 10; x += 1` |
| `condition_not_bool` | `if x { ... }` where x is i32 |

**Structs:**

| Error | Example |
| ----- | ------- |
| `duplicate_field` | Struct with two fields named `x` |
| `no_such_field` | `p.z` on a struct with only `x` and `y` |
| `field_access_on_non_struct` | `x.y` where x is `i32` |
| `missing_field` | `Point{ .x = 3 }` (missing `.y`) |
| `duplicate_literal_field` | `Point{ .x = 3, .x = 4 }` |

**Pointers:**

| Error | Example |
| ----- | ------- |
| `pointer_arithmetic` | `@mut i32 + 1` (single-item pointer) |

**Arrays:**

| Error | Example |
| ----- | ------- |
| `array_length_mismatch` | `arr: [3]i32 = [1, 2]` (wrong number of elements) |
| `index_non_array` | `x[0]` where x is `i32` |
| `index_not_integer` | `arr[flag]` where flag is `bool` |

## Always-Compile: Error Recovery

When the compiler encounters an error, it inserts a trap instruction at the error site and continues generating code. If execution reaches the error site at runtime, the program crashes immediately rather than producing silent corruption.

```honey
# This has a return type mismatch (f32 vs i32), but still compiles.
# The return site gets a trap instruction.
Point :: struct { x: f32, y: f32 }

main :: func() i32 {
    p := Point{ .x = 1, .y = 2 }
    return p.x    # ERROR: type mismatch → trap inserted
}
```

## Code Generation

- **Backend:** LLVM IR (text format)
- **Target:** aarch64-darwin (ARM 64-bit, macOS)
- **Honey calling convention:** `fastcc` with honey-specific name mangling
- **C calling convention:** Standard C ABI
- **Struct passing:** by-value uses `byval`, returns use `sret`
- **Bool representation:** `i8` (0 or 1)
- **Float operations:** Native LLVM float instructions (`fadd`, `fsub`, `fmul`, `fdiv`)
- **Comparison:** Correct signed (`sgt`, `slt`, ...) vs unsigned (`ugt`, `ult`, ...) predicates
