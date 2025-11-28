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

## Pointers and References

Honey uses distinct symbols for pointer operations to avoid ambiguity:

| Symbol | Context | Meaning |
|--------|---------|---------|
| `@` | Type | Pointer to (read-only) |
| `@mut` | Type | Pointer to (mutable) |
| `&` | Expression | Address of (reference) |
| `^` | Expression (postfix) | Dereference |

This keeps `*` unambiguous as multiplication.

### Pointer Mutability

Pointers respect the mutability of the data they point to. By default, pointers
are read-only (`@T`). To mutate through a pointer, you need a mutable pointer
(`@mut T`) pointing to mutable data.

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

```honey
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
    data: @mut u8,
    len: u64,
}

main :: func() void {
    mut buf: Buffer = ...
    ptr: @mut Buffer = &buf
    data := ptr^.data      # dereference then access field
    ptr^.len = 100         # modify field through pointer
}
```

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
print_n_times :: func(comptime N: u32, msg: []const u8) void {
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
    name: []const u8,
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

| Syntax | Meaning |
|--------|---------|
| `@T` | Pointer to immutable T (read-only) |
| `@mut T` | Pointer to mutable T (can write through) |
| `@@T` | Pointer to pointer to immutable T |
| `@@mut T` | Pointer to pointer to mutable T |
| `&x` | Address of x |
| `p^` | Dereference p |
| `p^^` | Dereference twice |

### Pointer Mutability Combinations

| Declaration | Reassign pointer? | Write through pointer? |
|-------------|-------------------|------------------------|
| `ptr: @T` | No | No |
| `ptr: @mut T` | No | Yes |
| `mut ptr: @T` | Yes | No |
| `mut ptr: @mut T` | Yes | Yes |

### Function Calls

| Syntax | Meaning |
|--------|---------|
| `func_name(args)` | Runtime function call |
| `func_name!(args)` | Comptime function call (required for `comptime func`) |
