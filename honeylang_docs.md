# Honey Language Documentation

## Compilation Philosophy

Honey follows an "always compile" philosophy. The compiler will always produce
an executable, even in the presence of errors. This enables incremental
development and experiential learning.

```
┌─────────────────────────────────────────────────────────────┐
│  COMPILATION                                                │
│                                                             │
│  Source → Always produces executable                        │
│           ├─ Clean code → normal instructions               │
│           ├─ Warnings → normal instructions + console msg   │
│           └─ Errors → trap/poison + console msg             │
│                                                             │
│  Exit code: 0 only if zero warnings AND zero errors         │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│  RUNTIME                                                    │
│                                                             │
│  Execute → hits trap? → crash with clear message            │
│          → hits poison value? → obvious garbage / crash     │
│          → avoids error paths? → runs fine                  │
└─────────────────────────────────────────────────────────────┘
```

**Why this approach?**

- **Incremental development:** Work on one part of your program while another
  is incomplete. If your execution path avoids the broken code, you can still
  test what you're working on.

- **Experiential learning:** Want to know what happens when you return stack
  memory? The compiler warns you, but lets you run it anyway. You learn by
  seeing the trap fire, not by being blocked from experimenting.

- **CI/CD compatibility:** The compiler returns a non-zero exit code when
  warnings or errors are present, so automated pipelines still catch issues
  before shipping.

### Warnings vs Errors

```honey
# Warning: you probably don't want this, but it's technically legal
dangerous :: func() []u8 {
    buffer: [10]u8 = undefined
    return buffer[0..]  # ⚠️ WARNING: returning slice to stack memory
}

# Error: this fundamentally cannot work, trap inserted
broken :: func() i32 {
    return "hello"  # ❌ ERROR: type mismatch → trap inserted
}

# Error: incomplete code
todo :: func() i32 {
    # no return statement → trap inserted at function exit
}
```

### TODO: Define the complete categorization of warnings vs errors
     
Questions to resolve:

1. Type errors: If someone writes `x: i32 = "hello"`, what goes in the 
binary?
    - Trap instruction at that site?
    - Zero-initialize with poison pattern (e.g., 0xAAAAAAAA) and trap on use?
    - Skip the entire function?

2. Comptime errors: How does this interact with compile-time evaluation?
    - `X :: 1 + "oops"` is a comptime error
    - Does comptime error → runtime trap at usage sites?
    - Or does comptime need stricter rules since there's no "runtime" to 
      defer to?

3. Specific categorizations needed:
    - Returning stack memory: warning or error?
    - Use of undefined variable: warning or error?
    - Unreachable code: warning or error?
    - Unused declarations: warning or error?
    - Integer overflow: warning or error? (probably depends on build mode)

## Comments

Comments are strings of characters prefixed with the `#` token. These are not
evaluated or compiled into the binary, they are simply ignored.

```honey
# Comments are prefixed with a pound, like this.
```

### Doc Comments

Doc comments are comments meant for documenting structures and behaviors and
are just like comments but prefixed with the `#!` token. They can only appear
in relation to any such constructs and will produce an error if left without
context.

Doc comments will be included in generated documentation.

```honey
#! This is a doc comment describing the purpose of the struct below.
PersonInfo :: struct {
    name: []u8,
    age: u8,
}

#! This doc comment stands alone and is thus invalid
<missing construct>
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
                           │
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

## Control Flow

### If Statements

Basic conditional execution:

```honey
if some_condition {
    do_something()
}

if x > 0 {
    print("positive")
} else {
    print("non-positive")
}
```

Parentheses around the condition are optional but allowed for visual clarity:

```honey
if (complex_expr and another_expr) {
    do_something()
}
```

For optional unwrapping with `if`, see Optional Types.

### Match Statements

Pattern matching on values:

```honey
match status {
    .ok: print("success"),
    .error: print("failure"),
    .pending: {
        log("still waiting")
        retry()
    },
    else: print("unknown status"),
}
```

Single expressions use a comma. Multiple statements require braces. The `else`
arm handles any unmatched values.

For enum types, the compiler checks exhaustiveness—if you handle all cases,
`else` is not required - in fact, including the `else` arm when handling all
cases, the compiler will error about unreachable code. If you add a new enum
variant later, the compiler will error at every non-exhaustive match, helping
you handle the new case everywhere.

### Ranges

Ranges represent a sequence of values, commonly used in for loops:

```honey
0..10      # exclusive: 0, 1, 2, ..., 9
0..=10     # inclusive: 0, 1, 2, ..., 10
```

**Parenthesization rule:** Each side of `..` must be either a simple term
(literal or identifier) or a parenthesized expression. This eliminates
precedence ambiguity:

```honey
0..10           # OK: both sides are literals
0..n            # OK: both sides are simple
0..(n + 1)      # OK: complex expression is parenthesized
(a + 1)..(b - 1)  # OK: both sides parenthesized

# 0..n + 1      # ERROR: must parenthesize complex expressions
```

This rule keeps the grammar simple and forces clarity at the call site—no
precedence rules to remember.

### For Loops

Iterate over arrays, slices, or ranges:

```honey
# iterate over elements
for val in some_array {
    print(val)
}

# iterate with index (value first, then index)
for val, idx in some_array {
    print("element {d} is {v}", {idx, val})
}

# iterate over a range
for i in 0..10 {
    print(i)  # prints 0 through 9
}

# inclusive range
for i in 0..=10 {
    print(i)  # prints 0 through 10
}

# with computed bounds
for i in 0..(len - 1) {
    process(data[i])
}
```

### While Loops

Basic while loop:

```honey
mut i := 0
while i < 10 {
    print(i)
    i += 1
}
```

**With continue expression:** The expression after `:` runs after each
iteration, including after `continue` statements. This prevents the common bug
of forgetting to increment:

```honey
mut i := 0
while i < 10 : i += 1 {
    if skip_condition {
        continue  # i += 1 still runs!
    }
    process(i)
}
```

The continue expression is particularly useful for pointer iteration:

```honey
mut ptr := start
while ptr < end : ptr += 1 {
    if ptr^ == 0 {
        continue  # ptr still advances
    }
    process(ptr^)
}
```

Note that pointer arithmetic requires a many-item pointer, and is generally
considered unsafe. That is, it's the programmers job to ensure that the pointer
always points to valid memory, and failing to uphold this invariant results in
undefined behavior. See Pointers.

### Break and Continue

`break` exits the innermost loop immediately:

```honey
for item in items {
    if item == target {
        break
    }
}
```

`continue` skips to the next iteration:

```honey
for item in items {
    if should_skip(item) {
        continue
    }
    process(item)
}
```

In while loops with a continue expression, `continue` executes that expression
before the next iteration.

## Pointers

Honey has two kinds of pointers, both **non-nullable by default**:

| Type | Name                | Arithmetic | Use Case                                  |
|------|---------------------|------------|-------------------------------------------|
| `@T` | Single-item pointer | No         | Point to one value                        |
| `*T` | Many-item pointer   | Yes        | Point into arrays/buffers, low-level work |

Unlike C/C++ pointers, Honey pointers cannot be null unless explicitly marked
optional (`?@T` or `?*T`). See Optional Types.

**Pointer operations:**

| Symbol   | Context              | Meaning                         |
|----------|----------------------|---------------------------------|
| `@T`     | Type                 | Single-item pointer (read-only) |
| `@mut T` | Type                 | Single-item pointer (mutable)   |
| `*T`     | Type                 | Many-item pointer (read-only)   |
| `*mut T` | Type                 | Many-item pointer (mutable)     |
| `&`      | Expression (prefix)  | Address of                      |
| `^`      | Expression (postfix) | Dereference                     |

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
import "std/mem/heap"

main :: func() void {
    buffer: []mut u8 = heap.alloc(u8, size: 100)
    defer heap.free(buffer)
    
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

| Type       | Meaning                                                   |
|------------|-----------------------------------------------------------|
| `[]T`      | Slice of immutable elements (cannot modify through slice) |
| `[]mut T`  | Slice of mutable elements (can modify through slice)      |
| `[N]T`     | Array of immutable elements                               |
| `[N]mut T` | Array of mutable elements                                 |

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
literal (FYI, string literals are stored in read-only memory).

To work with mutable text, instantiate it statically:

```honey
main :: func() void {
    buffer: [1024]mut u8 = {0} ** 1024
    buffer[0] = 'H'   # OK: can modify elements
}
```

Or allocate a buffer:

```honey
import "std/mem/heap"

main :: func() void {
    buffer: []mut u8 = heap.alloc(u8, size: 1024)
    defer heap.free(buffer)
    
    buffer[0] = 'H'   # OK: can modify elements
}
```

### Mutability Comparison

The mutability model is consistent across pointers, arrays, and slices:

| Pointer  | Array      | Slice     | Meaning                   |
|----------|------------|-----------|---------------------------|
| `@T`     | `[N]T`     | `[]T`     | Immutable target/elements |
| `@mut T` | `[N]mut T` | `[]mut T` | Mutable target/elements   |

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
complex_operation :: noinline func(...) void {
    ...
}
```

**Unannotated functions** let the compiler decide based on optimization level.

Annotations are instructions, not hints. If you annotate it, the compiler
respects it.

### Default Arguments

Functions can have parameters with default values. When calling such functions,
you can omit arguments that have defaults, or provide them using named argument
syntax.

**Declaration:**

```honey
greet :: func(name: []u8, greeting: []u8 = "Hello", times: u32 = 1) void {
    for _ in 0..times {
        print("{s}, {s}!", {greeting, name})
    }
}
```

**Calling with defaults:**

```honey
greet("Alice")                          # uses defaults: "Hello", 1
greet("Bob", greeting: "Hi")            # override greeting, use default times
greet("Carol", times: 3)                # use default greeting, override times
greet("Dave", greeting: "Hey", times: 2)  # override both
```

**Named argument syntax is required** when skipping earlier defaulted parameters
to set later ones. This avoids ambiguity about argument order:

```honey
greet("Eve", "Howdy", 5)    # positional: all arguments in order
greet("Eve", times: 5)      # named: skip greeting, set times
# greet("Eve", 5)           # ERROR: ambiguous - is 5 the greeting or times?
```

**Comptime parameters cannot have defaults.**

### Default Arguments vs Descriptor Structs

For functions with many optional parameters, consider using a descriptor struct
instead of many default arguments.

**Use default arguments when:**
- 1-3 optional parameters
- Parameters are independent of each other
- The common case uses mostly defaults

```honey
# Good use of default arguments
alloc :: func(comptime T: type, size: usize = 1) []T { ... }

read_file :: func(path: []u8, buffer_size: usize = 4096) ![]u8 { ... }
```

**Use descriptor structs when:**
- Many optional parameters (4+)
- Parameters form a logical group (configuration)
- You want to name the configuration for clarity

```honey
# Good use of descriptor struct
WindowDesc :: struct {
    width: u32 = 800,
    height: u32 = 600,
    title: []u8 = "Untitled",
    resizable: bool = true,
    vsync: bool = true,
    fullscreen: bool = false,
    monitor: ?@Monitor = none,
}

create_window :: func(desc: WindowDesc) !Window { ... }

# Clean to use, easy to extend without breaking callers
window := create_window({ .title = "My Game", .fullscreen = true })
```

The descriptor struct approach has an advantage: adding new fields with
defaults doesn't break any existing call sites. With default arguments, adding
a new parameter (even with a default) changes the function signature, and
likely the parameter order.

## Imports

```honey
import "std/mem"
import "std/mem/heap"

main :: func() void {
    buffer := heap.alloc(u8, size: 10)
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
    buffer := heap.alloc(u8, size: 10)
    defer heap.free(buffer)

    result := mem.eql(u8, buffer, "hello?")
    return result
}
```

Or using fully qualified paths:

```honey
import "std/mem"

some_func :: func() bool {
    buffer := mem.heap.alloc(u8, size: 10)
    defer mem.heap.free(buffer)

    result := mem.eql(u8, buffer, "hello?")
    return result
}

main :: func() void {
    _ = some_func()
}
```

## Memory Allocation

Memory allocation in Honey is designed to be **explicit but not verbose**. We
reject the dogma that global state is inherently evil—allocators are a
cross-cutting concern that nearly every function needs, making them a perfect
candidate for sensible defaults.

### Philosophy

```
┌─────────────────────────────────────────────────────────────┐
│  DESIGN PRINCIPLES                                          │
│                                                             │
│  1. No hidden magic: allocation calls are visible           │
│  2. Sensible defaults: thread-local heap for common cases   │
│  3. Explicit override: custom allocators when needed        │
│  4. Build-mode aware: different behavior for debug/release  │
│  5. Immutable defaults: no "action at a distance" bugs      │
└─────────────────────────────────────────────────────────────┘
```

### The Default Heap Allocator

Honey provides a **thread-local global heap allocator** that is:

- Determined at compile time by build mode
- **Immutable at runtime**—cannot be reconfigured

```honey
import "std/mem/heap"

process :: func(input: []u8) []u8 {
    # heap.alloc() is clear and explicit
    result := heap.alloc(u8, size: input.len * 2)
    
    # ... work with result ...
    
    return result  # caller is responsible for heap.free()
}

main :: func() void {
    data := process("hello")
    defer heap.free(data)
    
    # use data...
}
```

The behavior of `heap` depends on build mode:

| Build Mode    | Allocator Behavior                              |
|---------------|-------------------------------------------------|
| Debug         | Tracking allocator with leak detection          |
| Release       | Fast allocator, zero overhead                   |
| ReleaseSafe   | Bounds-checking allocator                       |

This is configured at compile time. You cannot change which allocator `heap`
uses at runtime. This is intentional—it prevents bugs where memory allocated
with one allocator is freed with another.

### Why Immutable Defaults?

Consider what would happen if you could reconfigure the default allocator:

```honey
# ❌ THIS IS NOT ALLOWED (and doesn't exist in Honey)

mem.heap_set(my_custom_heap)

# Somewhere else in the codebase...
data := heap.alloc(u8, size: 100)

# Later, someone changes it again...
mem.heap_set(different_heap)

# Now who frees `data`? With which allocator?
heap.free(data)  # 💥 Wrong allocator - undefined behavior!
```

This is "action at a distance"—the behavior of `heap.free()` depends on what
some unrelated code did earlier. By making `heap` immutable, Honey guarantees:

**Whatever you allocate with, you free with.**

### Custom Allocators

For specialized needs, you create explicit allocator instances. These are not
global—you manage their lifetime and pass them where needed.

**Arena Allocator** - Fast bump allocation, bulk deallocation:

```honey
import "std/mem"
import "std/mem/heap"

process_file :: func(path: []u8) !Data {
    # create arena backed by heap memory
    backing := heap.alloc(u8, size: mem.megabytes(1))
    defer heap.free(backing)
    
    arena := mem.Arena.init(backing)
    
    # all temporary allocations from arena (fast bump allocation)
    file_contents := arena.alloc(u8, size: file_size)
    parsed := arena.alloc(ParsedData)         # size defaults to 1
    tokens := arena.alloc(Token, size: 1000)
    
    # ... process ...
    
    # copy result to heap for return
    result := heap.create(Data)
    mem.copy(result, parsed)
    
    return result
    # arena goes out of scope - no individual frees needed
}
```

**Pool Allocator** - O(1) fixed-size allocation, no fragmentation:

```honey
import "std/mem"

EntitySystem :: struct {
    pool: mem.Pool(Entity),
}

init_entities :: func() EntitySystem {
    return EntitySystem{
        pool = mem.Pool(Entity).init(10_000),
    }
}

spawn :: func(sys: @mut EntitySystem) @Entity {
    return sys.pool.alloc()  # O(1), no fragmentation
}

despawn :: func(sys: @mut EntitySystem, entity: @Entity) void {
    sys.pool.free(entity)  # returned to pool for reuse
}
```

### Passing Allocators to Functions

When a function needs to allocate memory with a caller-provided allocator,
accept it as a parameter:

```honey
import "std/mem"

# Function that uses caller's allocator
parse :: func(input: []u8, allocator: @mem.Allocator) !ParseResult {
    buffer := allocator.alloc(u8, size: input.len)
    defer allocator.free(buffer)
    
    # ... parse into buffer ...
    
    result := allocator.alloc(ParseResult)  # size defaults to 1
    return result
}

# Caller decides which allocator to use
main :: func() void {
    # use an arena for this parsing work
    arena := mem.Arena.init(backing)
    result := parse(input, &arena) catch |err| {
        # handle error
    }
    
    # or use a pool
    pool := mem.Pool(ParseResult).init(100)
    result := parse(input, &pool) catch |err| {
        # handle error
    }
}
```

### Memory Allocation Summary

| What                        | How                 | When to Use                      |
|-----------------------------|---------------------|----------------------------------|
| `heap.alloc(T, size: n)`    | Thread-local global | General purpose, 90% of cases    |
| `heap.create(T)`            | Thread-local global | Allocate single item             |
| `arena.alloc(T, size: n)`   | Explicit instance   | Temporary/scoped work, bulk free |
| `pool.alloc()`              | Explicit instance   | Many same-sized objects, O(1)    |

**The golden rule:** Allocate and free with the same allocator. The type system
helps enforce this—memory from `heap` can only be freed with `heap`, memory
from your arena can only be freed with that arena.

### Compared to Other Languages

| Language | Approach | Honey's Advantage |
|----------|----------|-------------------|
| C | Hidden malloc, easy to mismatch | Explicit allocator at call site |
| C++ | Allocator templates, complex | Simple, no template complexity |
| Rust | Explicit everywhere, verbose | Sensible defaults reduce noise |
| Zig | Allocator parameter threading | Less verbose for common cases |
| Odin | Hidden context parameter | Fully transparent, nothing hidden |
| Go | Hidden GC | Explicit control, no GC pauses |

Honey sits in a sweet spot: explicit enough to always know what's happening,
convenient enough that you don't drown in boilerplate.

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

### Control Flow

| Syntax                           | Meaning                                     |
|----------------------------------|---------------------------------------------|
| `if cond { }`                    | Conditional execution                       |
| `if cond { } else { }`           | Conditional with else branch                |
| `match val { pat: expr, ... }`   | Pattern matching                            |
| `for x in collection { }`        | Iterate over elements                       |
| `for x, i in collection { }`     | Iterate with index                          |
| `for i in 0..n { }`              | Iterate over exclusive range                |
| `for i in 0..=n { }`             | Iterate over inclusive range                |
| `while cond { }`                 | While loop                                  |
| `while cond : cont_expr { }`     | While loop with continue expression         |
| `break`                          | Exit innermost loop                         |
| `continue`                       | Skip to next iteration                      |

### Ranges

| Syntax       | Meaning                                              |
|--------------|------------------------------------------------------|
| `a..b`       | Exclusive range (a to b-1)                           |
| `a..=b`      | Inclusive range (a to b)                             |
| `0..(n + 1)` | Complex expressions must be parenthesized            |

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
