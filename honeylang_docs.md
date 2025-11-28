# Honey Language Documentation

## Comments

Comments are strings of characters prefixed with the `#` token. These are, of
course, not evaluated or compiled into the binary, but are simply ignored by
the compiler.

```honey
# Comments are prefixed with a pound, so yes, this is a comment.
```

## Logical Operators and Comparators

Logical operators in the Honey language include `not`, `and`, and `or`, while
`<`, `>`, `<=`, `>=`, `==`, and `!=` comprise the comparison operators.

## Comptime vs Runtime

Honey draws a clear line between compile-time and runtime evaluation:

```
┌─────────────────────────────────────────────────────────────┐
│  COMPTIME WORLD                                             │
│                                                             │
│  - Constants (NAME :: expr)                                 │
│  - Types                                                    │
│  - Comptime function calls                                  │
│  - No side effects, no mutable state                        │
└─────────────────────────────────────────────────────────────┘
                           │
                           │ comptime values flow down
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  RUNTIME WORLD                                              │
│                                                             │
│  - Mutable globals (mut name := expr)                       │
│  - Immutable runtime globals (name := expr)                 │
│  - Function calls with runtime inputs                       │
│  - I/O, system calls, mutable state                         │
└─────────────────────────────────────────────────────────────┘
```

**Constraint:** Comptime cannot observe or mutate runtime state. Information
flows downward only.

## Type Inference

Types are inferred from the context in which they're first used. Once a type is
determined for any given name, it will be locked in place and must be cast to
the appropriate type explicitly if used in operations with different types.

If a type cannot be inferred from context, the compiler will emit an error
requesting an explicit type annotation. The compiler does not guess or apply
default types.

```honey
# type is inferred from the immediate value, when not used in any context
# (results in a warning about unused declarations - type doesn't matter)
DEBUG :: true
BAD_INDEX :: -1

# type is explicitly declared
PI: f64 :: 3.14

# type is inferred from the context in which it's first used
ANSWER :: 42  # type is pending
PI_SQUARED :: PI * PI  # PI has type f64, so PI_SQUARED resolves to f64
PI_CUBED :: PI * PI * PI  # same for PI_CUBED

# ANSWER is finally used, so its type can be resolved.
# the type of the context is f64 due to PI, so ANSWER's type resolves to f64
SHOULD_WORK :: ANSWER * PI

# floats can be declared without decimals when type is explicit or inferred
X: f32 :: 10
```

## Declarations

### Compile-time Constants

Compile-time constants are declared with `::` and are evaluated during
compilation:

```honey
PI :: 3.14159
MAX_SIZE :: 1024
DEBUG :: true
```

### Runtime Globals

Runtime globals are initialized at program start, before `main` executes.

**Immutable runtime globals:**

```honey
startup_time := get_system_time()  # computed once at startup, never changes
```

**Mutable runtime globals:**

```honey
mut counter := 0  # can be modified during program execution
```

Example usage:

```honey
DEBUG :: true  # comptime constant for comparison

mut did_something := false

do_something :: func() void {
    did_something = true
}

main :: func() void {
    do_something()
    
    if did_something {
        print("something was done!")
    }
}
```

**Restriction:** Comptime constants cannot reference runtime state:

```honey
mut counter := 0
# BAD :: counter  # ERROR: comptime cannot reference runtime mutable
```

## Discarding Values

The special name `_` acts as a sink for values you want to explicitly discard.
Use assignment (`=`), not declaration (`:=`), since `_` is not a variable being
declared:

```honey
_ = add(1, 2)        # explicitly discard the result
result := add(1, 2)  # actually use it
```

**Unused value warning:** If a function returns a value and you neither use it
nor assign it to `_`, the compiler emits a warning:

```honey
add(1, 2)  # WARNING: unused value
```

If you're calling a function purely for side effects, it should return `void`.
If it returns something, you should either use it or explicitly discard it.

## Uninitialized Variables

Honey provides `undefined` for explicit uninitialized variables:

```honey
x: i32 = undefined   # explicitly uninitialized
y: i32 = 0           # explicitly zero
z := get_value()     # initialized from expression
```

**Why `undefined` instead of zero-initialization?**

Zero-initialization is *safe* in the sense of "no garbage memory," but it's
*deceptive* - your program runs without crashing but may have a logic bug
because you forgot to set something and zero happened to be wrong.

`undefined` is honest. The compiler performs flow analysis to catch uses of
potentially undefined variables:

```honey
main :: func() void {
    x: i32 = undefined
    
    if some_condition {
        x = 10
    }
    
    print(x)  # ERROR: x may be undefined here
}
```

In debug builds, undefined memory is filled with poison values (e.g., 0xAA)
so that if a use slips past the compiler, you get an obvious crash or clearly
wrong data rather than a subtle bug.

## Pointers

Honey has two kinds of pointers, both **non-nullable by default**:

| Type | Name | Arithmetic | Use Case |
|------|------|------------|----------|
| `@T` | Single-item pointer | No | Point to one value |
| `*T` | Many-item pointer | Yes | Point into arrays/buffers, low-level work |

Unlike C/C++ pointers, Honey pointers cannot be null unless explicitly marked
optional (`?@T` or `?*T`). See Optional Types.

**Pointer operations:**

| Symbol | Context | Meaning |
|--------|---------|---------|
| `@T` | Type | Single-item pointer (read-only) |
| `@mut T` | Type | Single-item pointer (mutable) |
| `*T` | Type | Many-item pointer (read-only) |
| `*mut T` | Type | Many-item pointer (mutable) |
| `&` | Expression | Address of |
| `^` | Expression (postfix) | Dereference |

### Single-Item vs Many-Item Pointers

Single-item pointers (`@T`) point to exactly one value. The compiler prevents
arithmetic on them, catching bugs where you accidentally index a single value:

```honey
main :: func() void {
    x: i32 = 42
    ptr: @i32 = &x
    
    val := ptr^          # OK: dereference
    # bad := ptr + 1     # ERROR: no arithmetic on single-item pointer
}
```

Many-item pointers (`*T`) point to an unknown number of items and support
pointer arithmetic. Use them for low-level memory work:

```honey
main :: func() void {
    arr: [4]i32 = {1, 2, 3, 4}
    ptr: *i32 = &arr[0]     # many-item pointer into array
    
    first := ptr^           # OK: dereference
    second := (ptr + 1)^    # OK: arithmetic, then dereference
}
```

### Pointer Mutability

Both pointer types respect the mutability of the data they point to. By default,
pointers are read-only (`@T`, `*T`). To mutate through a pointer, you need a
mutable pointer (`@mut T`, `*mut T`) pointing to mutable data.

```honey
main :: func() void {
    x: i32 = 3              # immutable variable
    mut y: i32 = 4          # mutable variable
    
    a: @i32 = &x            # OK: read-only pointer to immutable
    b: @i32 = &y            # OK: read-only pointer to mutable (downgrade is fine)
    c: @mut i32 = &x        # ERROR: cannot take mutable pointer to immutable
    d: @mut i32 = &y        # OK: mutable pointer to mutable
    
    val := a^               # OK: reading through any pointer
    a^ = 10                 # ERROR: cannot write through @i32
    d^ = 10                 # OK: y is now 10
}
```

The same rules apply to many-item pointers (`*T`, `*mut T`).

### Pointer Variable Mutability

The pointer *variable* can also be mutable or immutable, independently of what
it points to. This controls whether the pointer itself can be reassigned:

```honey
main :: func() void {
    mut x: i32 = 1
    mut y: i32 = 2
    
    ptr: @mut i32 = &x      # immutable pointer to mutable data
    ptr^ = 10               # OK: can write through pointer
    ptr = &y                # ERROR: ptr itself is immutable
    
    mut ptr2: @mut i32 = &x # mutable pointer to mutable data
    ptr2^ = 10              # OK: can write through pointer
    ptr2 = &y               # OK: can reassign pointer
}
```

### All Four Combinations

These combinations apply to both single-item (`@`) and many-item (`*`) pointers:

```honey
# Single-item pointers
ptr1: @i32              # immutable pointer to immutable data
                        # - cannot reassign ptr1
                        # - cannot write through ptr1

ptr2: @mut i32          # immutable pointer to mutable data
                        # - cannot reassign ptr2
                        # - can write through ptr2

mut ptr3: @i32          # mutable pointer to immutable data
                        # - can reassign ptr3
                        # - cannot write through ptr3

mut ptr4: @mut i32      # mutable pointer to mutable data
                        # - can reassign ptr4
                        # - can write through ptr4

# Many-item pointers (same combinations, plus arithmetic)
buf1: *u8               # immutable many-item pointer to immutable data
buf2: *mut u8           # immutable many-item pointer to mutable data
mut buf3: *u8           # mutable many-item pointer to immutable data
mut buf4: *mut u8       # mutable many-item pointer to mutable data
```

### Pointer Chains

Multi-level pointers work naturally:

```honey
main :: func() void {
    mut x: i32 = 42
    mut y: @mut i32 = &x   # pointer to x
    z: @@mut i32 = &y      # pointer to pointer to x
    
    a: i32 = y^            # dereference once
    b: i32 = z^^           # dereference twice
}
```

### Struct Field Access Through Pointers

```honey
Buffer :: struct {
    data: *mut u8,      # many-item pointer to buffer data
    len: u64,
}

main :: func() void {
    mut buf: Buffer = ...
    ptr: @mut Buffer = &buf   # single-item pointer to one Buffer
    data := ptr^.data         # dereference then access field
    ptr^.len = 100            # modify field through pointer
}
```

### Pointer Arithmetic

Only many-item pointers (`*T`) support arithmetic. This prevents accidental
arithmetic on pointers that are meant to reference a single value:

```honey
import "std/mem"

main :: func() void {
    buffer: []mut u8 = mem.alloc(u8, 100)
    defer mem.free(buffer)
    
    ptr: *mut u8 = &buffer[0]   # many-item pointer into buffer
    
    # arithmetic operations
    second := ptr + 1           # pointer to second element
    tenth := ptr + 9            # pointer to tenth element
    
    # difference between pointers (returns offset)
    offset := tenth - ptr       # offset is 9
    
    # comparison
    if ptr < tenth {
        print("ptr comes before tenth")
    }
    
    # increment/decrement (requires mutable binding)
    mut cursor: *mut u8 = ptr
    cursor += 1                 # advance one element
    cursor -= 1                 # back one element
}
```

Single-item pointers (`@T`) do **not** support arithmetic:

```honey
main :: func() void {
    x: i32 = 42
    ptr: @i32 = &x
    
    # bad := ptr + 1    # ERROR: cannot do arithmetic on @i32
}
```

**Safety note:** Pointer arithmetic can create invalid pointers. The compiler
does not bounds-check pointer arithmetic—it's the programmer's responsibility
to ensure pointers remain valid. Out-of-bounds access is undefined behavior.

### Nullable Pointers

To represent a pointer that may be absent, use an optional pointer type:

```honey
ptr: @Buffer = get_buffer()    # must return valid pointer, cannot be null
opt: ?@Buffer = find_buffer()  # may be none, must unwrap before use

# same for many-item pointers
data: *u8 = get_data()         # must be valid
maybe: ?*u8 = find_data()      # may be none

# use orelse or conditional unwrap to handle nullable pointers
if opt |p| {
    use(p)
}
```

## Arrays and Slices

Honey distinguishes between fixed-size arrays and dynamically-sized slices.

### Arrays

Arrays have a fixed size known at compile time. The size is part of the type:

```honey
arr: [4]u8 = {1, 2, 3, 4}      # fixed array of 4 bytes
zeros: [16]i32 = {0} ** 16    # array initialized to all zeros
```

### Slices

Slices are a pointer-length pair that reference a contiguous sequence of
elements. They don't own the data they point to:

```honey
arr: [4]u8 = {1, 2, 3, 4}
slice: []u8 = &arr            # slice referencing the array
```

### Element Mutability

Following Honey's "immutable by default" principle, slice and array element
mutability works the same as pointer mutability:

| Type | Meaning |
|------|---------|
| `[]T` | Slice of immutable elements (cannot modify through slice) |
| `[]mut T` | Slice of mutable elements (can modify through slice) |
| `[N]T` | Array of immutable elements |
| `[N]mut T` | Array of mutable elements |

```honey
# immutable elements (default)
data: []u8 = &buffer
data[0] = 65              # ERROR: cannot modify through []u8

# mutable elements
mut_data: []mut u8 = &mut_buffer
mut_data[0] = 65          # OK: can modify through []mut u8
```

### Slice Variable Mutability

Just like pointers, the slice *variable* itself can be mutable or immutable,
independently of element mutability:

```honey
slice: []u8             # cannot reassign slice, cannot modify elements
slice: []mut u8         # cannot reassign slice, CAN modify elements
mut slice: []u8         # CAN reassign slice, cannot modify elements
mut slice: []mut u8     # CAN reassign slice, CAN modify elements
```

### String Literals

String literals have type `[]u8`—a slice of immutable bytes:

```honey
greeting: []u8 = "hello"      # string literal, immutable
name := "world"               # type inferred as []u8
```

Since `[]u8` has immutable elements by default, you cannot modify a string
literal (which is correct—string literals are stored in read-only memory).

To work with mutable text, allocate a buffer:

```honey
import "std/mem/heap"

main :: func() void {
    buffer: []mut u8 = heap.alloc(u8, 1024)
    defer heap.free(buffer)
    
    buffer[0] = 'H'   # OK: can modify elements
}
```

### Mutability Comparison

The mutability model is consistent across pointers, arrays, and slices:

| Pointer | Array | Slice | Meaning |
|---------|-------|-------|---------|
| `@T` | `[N]T` | `[]T` | Immutable target/elements |
| `@mut T` | `[N]mut T` | `[]mut T` | Mutable target/elements |

## Optional Types

To represent a value that may or may not be present, use optional types with
the `?` prefix.

### Basic Syntax

```honey
x: ?i32 = none      # optional with no value
y: ?u8 = 255        # optional with a value
z: ?@Buffer = none  # optional pointer (nullable pointer)
```

### Unwrapping Optionals

**Force unwrap with `?` postfix** - Extracts the value, traps/panics if `none`:

```honey
y: ?u8 = 255
val := y?  # val is u8, equals 255

x: ?i32 = none
bad := x?  # runtime trap/panic - x is none
```

**Unwrap with default using `orelse`**:

```honey
x: ?i32 = none
y := x orelse 0      # y is i32, equals 0

name: ?[]u8 = get_name()
display := name orelse "anonymous"
```

### Conditional Unwrapping

Use `if` with capture syntax to safely unwrap and bind the inner value:

```honey
name: ?[]u8 = get_name()

if name |n| {
    # n is []u8 here, block only runs if name != none
    print(n)
}
```

**With else branch:**

```honey
if config |c| {
    use(c)
} else {
    use_defaults()
}
```

**With guard clause** - Add a condition on the unwrapped value after `:`:

```honey
age: ?u8 = get_age()

if age |a : a >= 18| {
    print("adult")
}
```

The guard is evaluated only if the optional contains a value. If the guard
evaluates to `false`, the `else` branch (if present) is taken.

### Multi-Unwrap

Unwrap multiple optionals with `and`. This **short-circuits**: if the first
optional is `none`, subsequent expressions are not evaluated.

```honey
name: ?[]u8 = get_name()
age: ?u8 = get_age()

if name and age |n, a| {
    # both n and a are guaranteed non-none here
    print("{s} is {d} years old", {n, a})
}
```

**With guard clause on multiple values:**

```honey
if name and hat |n, h : n == "Huginn" and h.brand == .gucci| {
    print("{s}'s got that drip\n", {n})
}
```

Parentheses around the expression are optional, but can aid readability when
combined with guards:

```honey
# without parentheses
if name and hat |n, h : guard| { ... }

# with parentheses for clarity
if (name and hat) |n, h : guard| { ... }
```

### Short-Circuit Evaluation

The `and` in multi-unwrap short-circuits left-to-right:

```honey
if get_name() and get_hat() |n, h| {
    # get_hat() is only called if get_name() returned non-none
}
```

This is important for avoiding unnecessary computation or side effects.

### Optional Type Summary

| Syntax | Meaning |
|--------|---------|
| `?T` | Optional type (may be `none`) |
| `none` | Absent value |
| `x orelse default` | Unwrap with fallback value |
| `x?` | Force unwrap (traps if `none`) |
| `if x \|v\| { }` | Conditional unwrap |
| `if x \|v : guard\| { }` | Conditional unwrap with guard |
| `if x and y \|a, b\| { }` | Multi-unwrap (short-circuits) |
| `if (x and y) \|a, b : guard\| { }` | Multi-unwrap with guard |

## Functions

### Basic Functions

```honey
add :: func(a: i32, b: i32) i32 {
    return a + b
}

compute_result :: func(a: u32, b: u32) i32 {
    return add(a as i32, b as i32)
}

main :: func() void {
    _ = compute_result(1, 2)
}
```

### Comptime Functions

Functions declared with `comptime` exist only at compile time and cannot be
called at runtime. All parameters are implicitly comptime.

**Comptime function calls must use the `!` postfix** to make compile-time
evaluation explicit in the code:

```honey
# all parameters implicitly comptime - no need to annotate them
make_array_type :: comptime func(T: type, N: u32) type {
    return [N]T
}

# the `!` postfix is required when calling comptime functions
MyArray :: make_array_type!(i32, 16)
```

Built-in comptime functions also use this syntax:

```honey
main :: func() void {
    size := size_of!(Buffer)
    alignment := align_of!(Buffer)
}
```

### Comptime Parameters

Runtime functions can have specific parameters marked as `comptime`. These must
be known at compile time, enabling specialization.

**Note:** The `!` postfix is only required for comptime-only functions (declared
with `comptime func`). Runtime functions with comptime parameters are called
normally:

```honey
# comptime-only function - requires `!` to call
make_array_type :: comptime func(T: type, N: u32) type {
    return [N]T
}

# runtime function with comptime parameter - called normally
print_n_times :: func(comptime N: u32, msg: []u8) void {
    # compiler can unroll this because N is comptime-known
    ...
}

# wrapper around builtin - still a runtime function
sizeof :: func(comptime T: type) u64 {
    return size_of!(T)  # builtin comptime call uses `!`
}

main :: func() void {
    MyArray :: make_array_type!(i32, 16)  # comptime func, needs `!`
    
    print_n_times(5, "hello")  # runtime func, no `!`
    x := sizeof(i32)           # runtime func, no `!`
}
```

### Comptime Polymorphism

Comptime parameters enable generic programming:

```honey
make_pair :: comptime func(T: type) type {
    return struct {
        first: T,
        second: T,
    }
}

IntPair :: make_pair!(i32)
FloatPair :: make_pair!(f32)
```

### Function Inlining

Inlining is a runtime concern distinct from comptime. When a function is
inlined, its body is copied to each call site instead of performing a
jump-and-return.

**`inline`** - Force inline. Compiler errors if inlining is impossible:

```honey
add :: inline func(a: i32, b: i32) i32 {
    return a + b
}
```

**`noinline`** - Prevent inlining. Useful for debugging or controlling code size:

```honey
complex_operation :: noinline func(...) {
    ...
}
```

**Unannotated functions** let the compiler decide based on optimization level.

Annotations are instructions, not hints. If you annotate it, the compiler
respects it.

## Imports

```honey
import "std/mem"
import "std/mem/heap"

main :: func() void {
    buffer := heap.alloc(u8, 10)
    defer heap.free(buffer)

    result := mem.eql(u8, buffer, "hello?")
    return result
}
```

Or with local aliasing:

```honey
import "std/mem"

heap :: mem.heap

main :: func() void {
    buffer := heap.alloc(u8, 10)
    defer heap.free(buffer)

    result := mem.eql(u8, buffer, "hello?")
    return result
}
```

Or using fully qualified paths:

```honey
import "std/mem"

some_func :: func() bool {
    buffer := mem.heap.alloc(u8, 10)
    defer mem.heap.free(buffer)

    result := mem.eql(u8, buffer, "hello?")
    return result
}

main :: func() void {
    _ = some_func()
}
```

## Structs

```honey
import "std/mem/heap"

PersonInfo :: struct {
    name: []u8,
    age: u8,
}

main :: func() void {
    # instantiate dynamically
    person_ptr := heap.create(PersonInfo)
    defer heap.destroy(person_ptr)

    # or statically
    person_static := PersonInfo{
        .name = "Carol",
        .age = 42,
    }
}
```

## Type Aliasing

```honey
EntityId :: u32
EntityId: type :: u32  # equivalent, explicit form
```

## Summary of Syntax

### Declarations

| Syntax                             | Meaning                                    |
|------------------------------------|--------------------------------------------|
| `NAME :: expr`                     | Compile-time constant                      |
| `name := expr`                     | Runtime immutable (initialized at startup) |
| `mut name := expr`                 | Runtime mutable                            |
| `name: T = undefined`              | Explicitly uninitialized variable          |
| `_ = expr`                         | Discard a value                            |
| `name :: func(...) T { }`          | Runtime function                           |
| `name :: comptime func(...) T { }` | Compile-time only function                 |
| `name :: inline func(...) T { }`   | Force-inlined runtime function             |
| `name :: noinline func(...) T { }` | Never-inlined runtime function             |

### Pointers

| Syntax   | Meaning                                        |
|----------|------------------------------------------------|
| `@T`     | Single-item pointer (read-only, no arithmetic) |
| `@mut T` | Single-item pointer (mutable, no arithmetic)   |
| `*T`     | Many-item pointer (read-only, with arithmetic) |
| `*mut T` | Many-item pointer (mutable, with arithmetic)   |
| `@@T`    | Pointer to pointer (single-item)               |
| `**T`    | Pointer to pointer (many-item)                 |
| `&x`     | Address of x                                   |
| `p^`     | Dereference p                                  |
| `p^^`    | Dereference twice                              |

### Pointer Mutability Combinations

Applies to both `@T` (single-item) and `*T` (many-item) pointers:

| Declaration                           | Reassign pointer? | Write through pointer? |
|---------------------------------------|-------------------|------------------------|
| `ptr: @T` / `ptr: *T`                 | No                | No                     |
| `ptr: @mut T` / `ptr: *mut T`         | No                | Yes                    |
| `mut ptr: @T` / `mut ptr: *T`         | Yes               | No                     |
| `mut ptr: @mut T` / `mut ptr: *mut T` | Yes               | Yes                    |

### Arrays and Slices

| Syntax     | Meaning                                  |
|------------|------------------------------------------|
| `[N]T`     | Fixed-size array of N immutable elements |
| `[N]mut T` | Fixed-size array of N mutable elements   |
| `[]T`      | Slice of immutable elements              |
| `[]mut T`  | Slice of mutable elements                |
| `"hello"`  | String literal (type `[]u8`)             |

### Slice Mutability Combinations

| Declaration      | Reassign slice? | Modify elements? |
|------------------|-----------------|------------------|
| `s: []T`         | No              | No               |
| `s: []mut T`     | No              | Yes              |
| `mut s: []T`     | Yes             | No               |
| `mut s: []mut T` | Yes             | Yes              |

### Optional Types

| Syntax                              | Meaning                        |
|-------------------------------------|--------------------------------|
| `?T`                                | Optional type (may be `none`)  |
| `none`                              | Absent value                   |
| `x orelse default`                  | Unwrap with fallback value     |
| `x?`                                | Force unwrap (traps if `none`) |
| `if x \|v\| { }`                    | Conditional unwrap             |
| `if x \|v : guard\| { }`            | Conditional unwrap with guard  |
| `if x and y \|a, b\| { }`           | Multi-unwrap (short-circuits)  |
| `if (x and y) \|a, b : guard\| { }` | Multi-unwrap with guard        |

### Function Calls

| Syntax             | Meaning                                               |
|--------------------|-------------------------------------------------------|
| `func_name(args)`  | Runtime function call                                 |
| `func_name!(args)` | Comptime function call (required for `comptime func`) |
