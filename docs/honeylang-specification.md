# Honey Language Documentation

Specification v0.1.4

## Compilation Philosophy

Honey follows an "always compile" philosophy. The compiler will always produce an executable, even in the presence of errors. This enables incremental development and experiential learning.

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

* **Incremental development:** Work on one part of your program while another is incomplete. If your execution path avoids the broken code, you can still test what you're working on.
* **Experiential learning:** Want to know what happens when you return stack memory? The compiler warns you, but lets you run it anyway. You learn by seeing the trap fire, not by being blocked from experimenting.
* **CI/CD compatibility:** The compiler returns a non-zero exit code when warnings or errors are present, so automated pipelines still catch issues before shipping.

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

1. Type errors: If someone writes `x: i32 = "hello"`, what goes in the binary?

```
- Trap instruction at that site?
- Zero-initialize with poison pattern (e.g., 0xAAAAAAAA) and trap on use?
- Skip the entire function?
```

2. Compile-time errors: How does this interact with compile-time evaluation?
   * `X :: 1 + "oops"` is a comptime error
   * Does comptime error → runtime trap at usage sites?
   * Or does comptime need stricter rules since there's no "runtime" to defer to?
3. Specific categorizations needed:
   * Returning stack memory: warning or error?
   * Use of undefined variable: warning or error?
   * Unreachable code: warning or error?
   * Unused declarations: warning or error?
   * Integer overflow: warning or error? (probably depends on build mode)

## Comments

Comments are strings of characters prefixed with the `#` token. These are not evaluated or compiled into the binary, they are simply ignored.

```honey
# Comments are prefixed with a pound, like this.
```

### Doc Comments

Doc comments are comments meant for documenting structures and behaviors and are just like comments but prefixed with the `#!` token. They can only appear in relation to any such constructs and will produce an error if left without context.

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

Logical operators in the Honey language include `!`, `and`, and `or`, while `<`, `>`, `<=`, `>=`, `==`, and `!=` comprise the comparison operators.

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

**Constraint:** Compile-time cannot observe or mutate runtime state. Information flows downward only.

## Type Inference

Types are inferred from the context in which they're first used. Once a type is determined for any given name, it will be locked in place and must be cast to the appropriate type explicitly if used in operations with different types. If a type cannot be inferred from context, the compiler will emit an error requesting an explicit type annotation. The compiler does not guess or apply default types.

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

## Type Casting

Honeylang does not support implicit type casting. All casts must be explicit.

### `as` — Checked Cast

The `as` keyword is an infix operator that casts a value to a target type, preserving its meaning. It traps at runtime if the value doesn't fit in the target type.

```honey
x: i64 = 1000
y := x as i32          # OK: 1000 fits in i32
z := x as u8           # TRAP: 1000 > 255

key := 'q' as i32      # OK: char to int
ratio := count as f64  # int to float
```

`as` binds tighter than arithmetic but looser than unary operators: `x as i32 + 1` is `(x as i32) + 1`.

### Builtins — Unchecked Casts

For casts that bypass safety checks, Honey provides builtin functions:

| Builtin | Purpose | Traps when... |
| -- | -- | -- |
| `truncate(x, T)` | Keep low bits, discard rest | Never |
| `bitcast(x, T)` | Reinterpret bits, no cast | Sizes don't match (compile error) |
| `ptrcast(p, T)` | Change pointer type | Gaining mutability (compile error) |

```honey
# truncation
a: u32 = 0xDEADBEEF
b := truncate(a, u8)  # b == 0xEF (low byte)

# bit reinterpretation
n: i32 = -1
m := bitcast(n, u32)  # m == 0xFFFFFFFF (same bits)
f: f32 = 3.14
bits := bitcast(f, u32)   # IEEE 754 representation

# pointer casts (element type, many ↔ single, pointer ↔ usize)
buf: *u8 = get_buffer()
ints := ptrcast(buf, *u32)    # element type change
single := ptrcast(buf, @u8)   # many → single (restricting)
addr := ptrcast(buf, usize)   # pointer to integer
ptr := ptrcast(addr, @u8)     # integer to pointer
```

## Declarations

### Compile-time Constants

Compile-time constants are declared with `::` and are evaluated during compilation:

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

**Restriction:** Compile-time constants cannot reference runtime state:

```honey
mut counter := 0
# BAD :: counter  # ERROR: comptime cannot reference runtime mutable
```

## Discarding Values

The special name `_` acts as a sink for values you want to explicitly discard. Use assignment (`=`), not declaration (`:=`), since `_` is not a variable being declared:

```honey
_ = add(1, 2)        # explicitly discard the result
result := add(1, 2)  # actually use it
```

**Unused value warning:** If a function returns a value and you neither use it nor assign it to `_`, the compiler emits a warning:

```honey
add(1, 2)  # WARNING: unused value
```

If you're calling a function purely for side effects, it should return `void`. If it returns something, you should either use it or explicitly discard it.

## Uninitialized Variables

Honey provides `undefined` for explicit uninitialized variables:

```honey
x: i32 = undefined   # explicitly uninitialized
y: i32 = 0           # explicitly zero
z := get_value()     # initialized from expression
```

**Why** `undefined` instead of zero-initialization?

Zero-initialization is *safe* in the sense of "no garbage memory," but it's *deceptive* — your program runs without crashing but may have a logic bug because you forgot to set something and zero happened to be wrong. `undefined` is honest. It tells the compiler "I haven't initialized this yet — track it for me."

### How `undefined` is checked

Honey uses a layered approach to catch uses of uninitialized memory:

**Layer 1 — Compile-time flow analysis:** For scalar locals and whole-variable assignments, the compiler tracks initialization across branches. If it cannot prove a variable is initialized at a use site, it emits an error and inserts a trap (per the always-compile philosophy):

```honey
main :: func() void {
    x: i32 = undefined

    if some_condition {
        x = 10
    }

    print(x)  # ERROR: x may be undefined here (no else branch)
              # trap inserted — will crash if reached at runtime
}
```

```honey
main :: func() void {
    x: i32 = undefined

    if some_condition {
        x = 10
    } else {
        x = 20
    }

    print(x)  # OK: x is proven initialized on all paths
}
```

**Layer 2 — Debug poison values:** For cases the compiler cannot reason about — per-element buffer initialization, memory written through pointers by external functions — undefined memory is filled with poison values (e.g., 0xAA) in debug builds. This is a best-effort runtime safety net:

```honey
main :: func() void {
    buffer: [4096]u8 = undefined   # filled with 0xAA in debug
    bytes_read := read_into(&buffer)
    process(buffer[0..bytes_read])  # compiler can't verify read_into filled the buffer
                                    # if it didn't, debug poison makes bugs obvious
}
```

**Layer 3 — Programmer responsibility:** In release builds, there are no poison values and no runtime checks. If undefined memory is read in a case the compiler couldn't catch, it is undefined behavior. This is the cost of maximum performance — the programmer is responsible for ensuring correctness in cases beyond the compiler's analysis.

| Layer | Catches | Cost |
| -- | -- | -- |
| Flow analysis (compile time) | Scalars, whole-variable assignments, branching | Zero — compile-time only |
| Poison values (debug runtime) | Buffer elements, memory behind pointers | Debug builds only |
| Programmer (release runtime) | Everything else | None — UB if wrong |

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

### Blocks and Scoping

A bare block `{ ... }` can appear anywhere a statement is expected. It introduces a new scope: variables declared inside are local to the block and are dropped when the block exits.

```honey
main :: func() void {
    a := 1
    {
        b := 2          # b is only visible inside this block
        use(a + b)      # a is visible from the outer scope
    }
    # b is no longer in scope here
    b := 3              # this is fine — it's a new declaration, not a redeclaration
}
```

**Shadowing is not allowed.** Redeclaring a variable that already exists in an enclosing scope (or as a function parameter) is an error:

```honey
main :: func(x: i32) void {
    x := 5              # ERROR: variable shadows outer declaration (parameter)
}

main :: func() void {
    a := 1
    {
        a := 2          # ERROR: variable shadows outer declaration
    }
}
```

Sibling blocks do not conflict — once a block exits, its names are available for reuse:

```honey
main :: func() void {
    {
        temp := compute_a()
    }
    {
        temp := compute_b()     # fine — previous temp was dropped
    }
}
```

### Defer

The `defer` keyword schedules a statement to execute when the enclosing block exits, regardless of how the block is exited (fall-through or return). Multiple defers in the same block execute in reverse order (LIFO):

```honey
main :: func() void {
    {
        defer printf("second\n")
        defer printf("first\n")
        printf("inside\n")
    }
    # output: inside, first, second
    printf("after\n")
}
```

Defers execute before `return` when a return appears inside the block:

```honey
open_and_read :: func() i32 {
    handle := open("file.txt")
    defer close(handle)             # runs before the function returns

    data := read(handle)
    return process(data)            # close(handle) runs here, before returning
}
```

### Match Statements and Expressions

Pattern matching on values:

```honey
match status {
    .ok: print("success"),
    .error: print("failure"),
    .pending: {
        log("still waiting")
        retry()
    },
}
```

Single expressions use a comma. Multiple statements require braces.

**Matching on integers and other values:**

```honey
match code {
    0: print("zero"),
    1: print("one"),
    2: print("two"),
    else: print("other"),
}
```

**The** `else` arm handles any unmatched values. For non-enum types (integers, strings, etc.), `else` is required since exhaustive matching isn't possible.

**Exhaustiveness checking:** For enum types, the compiler checks that all variants are handled. If you handle all cases, `else` is not required — in fact, including `else` when all cases are covered produces a compiler error about unreachable code. This means adding a new enum variant later will cause compiler errors at every non-exhaustive match, helping you handle the new case everywhere.

```honey
Status :: enum { ok, error, pending }

match status {
    .ok: handle_ok(),
    .error: handle_error(),
    .pending: handle_pending(),
    # no else needed - all variants covered
}
```

**Union variant matching with payload capture:** When matching on tagged unions, use `|name|` to capture the variant's payload:

```honey
Result :: union(enum) {
    success: Data,
    failure: struct {
        msg: []u8,
        code: i32,
    },
    pending: void,
}
match result {
    .success |data|: {
        process(data)
    },
    .failure |info|: {
        print("error {d}: {s}", {info.code, info.msg})
    },
    .pending: {
        # void payload - no capture needed
        wait()
    },
}
```

The capture syntax is consistent with optional unwrapping (`if opt |val| { }`)
and error handling (`catch |e| { }`).

**Void payloads:** When a union variant has a `void` payload, simply omit the capture:

```honey
Event :: union(enum) {
    click: struct { x: i32, y: i32 },
    keypress: KeyCode,
    quit: void,
}

match event {
    .click |pos|: handle_click(pos.x, pos.y),
    .keypress |key|: handle_key(key),
    .quit: should_exit = true,
}
```

**Match expressions:** When matching on an expression (rather than a variable), the result is evaluated once:

```honey
match get_status() {
    .ok: print("success"),
    .error: print("failure"),
}

# get_status() is only called once
```

For union payload capture on expressions, the capture binds the payload directly:

```honey
match fetch_result() {
    .success |data|: use(data),
    .failure |err|: log(err.msg),
}
```

For error handling patterns using match, see the Error Handling section.

**Match as expression:** Match can produce a value when used in expression position. Single-expression arms yield their value implicitly. Multi-statement arms use explicit `yield`:

```honey
label := match priority {
    .critical: "CRIT",
    .high:     "HIGH",
    .normal:   "NORM",
    .low:      " LOW",
}

message := match code {
    0: "success",
    1: {
        log("warning encountered")
        yield "warning"
    },
    else: "unknown",
}
```

This follows the same rule as `catch` and scoped blocks: single expressions yield implicitly, blocks require explicit `yield`.

### Ranges

Ranges represent a sequence of values, commonly used in for loops:

```honey
0..10      # exclusive: 0, 1, 2, ..., 9
0..=10     # inclusive: 0, 1, 2, ..., 10
```

**Parenthesization rule:** Each side of `..` must be either a simple term (literal or identifier) or a parenthesized expression. This eliminates precedence ambiguity:

```honey
0..10           # OK: both sides are literals
0..n            # OK: both sides are simple
0..(n + 1)      # OK: complex expression is parenthesized
(a + 1)..(b - 1)  # OK: both sides parenthesized
# 0..n + 1      # ERROR: must parenthesize complex expressions
```

This rule keeps the grammar simple and forces clarity at the call site — no precedence rules to remember.

### For Loops

Iterate over arrays, slices, or ranges:

```honey
# iterate over elements
for some_array |val| {
    print(val)
}

# iterate with index (value first, then index)
for some_array |val, idx| {
    print("element {d} is {v}", {idx, val})
}

# iterate over a range
for 0..10 |i| {
    print(i)  # prints 0 through 9
}

# inclusive range
for 0..=10 |i| {
    print(i)  # prints 0 through 10
}

# with computed bounds
for 0..(len - 1) |i| {
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

**With continue expression:** The expression after `:` runs after each iteration, including after continue statements. This prevents the common bug of forgetting to increment:

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

Note that pointer arithmetic requires a many-item pointer, and is generally considered unsafe. That is, it's the programmers job to ensure that the pointer always points to valid memory, and failing to uphold this invariant results in undefined behavior. See Pointers.

### Break and Continue

`break` exits the innermost loop immediately:

```honey
for items |item| {
    if item == target {
        break
    }
}
```

`continue` skips to the next iteration:

```honey
for items |item| {
    if should_skip(item) {
        continue
    }
    process(item)
}
```

In while loops with a continue expression, `continue` executes that expression before the next iteration.

### Yield

The `yield` keyword provides a value from a block to its enclosing expression and **exits the block immediately** — just as `return` exits a function, `yield` exits the enclosing scope. Code after a `yield` is unreachable, and the compiler flags it. This makes `yield` part of a consistent set of scope-exiting control flow: `return` exits a function, `yield` exits a block, `break` exits a loop, and `continue` skips to the next iteration.

It is used in scoped blocks, match arms, and catch handlers.

**General rule:** When a block needs to produce a value, single expressions yield implicitly while multi-statement blocks require explicit `yield`. This rule applies uniformly across the language:

```honey
# scoped block
data := {
    result := compute()
    yield result
}

# match arms
label := match p {
    .high: "HIGH",          # single expression: implicit yield
    .low: {
        log("low priority")
        yield "LOW"         # block: explicit yield
    },
}

# catch handlers
data := read(path) catch default_data         # single expression: implicit
data := read(path) catch |e| {
    log(e)
    yield fallback_data                       # block: explicit yield
}
```

### Labeled Blocks

Blocks can be labeled with `name:` to enable yielding from a specific scope in nested contexts. An unlabeled `yield` always exits the innermost block. A labeled `yield :name value` exits the block with that label, even from within nested blocks or conditions:

```honey
result := outer: {
    mut status := 500

    if some_condition {
        yield :outer 404        # exits the outer block with 404
    }

    {
        yield :outer status     # can target outer from a nested block too
    }
}
```

Unlabeled `yield` without a label exits the immediately enclosing block — this is the common case and keeps simple code concise:

```honey
value := {
    yield 42                    # no label needed for the simple case
}
```

Labels are useful when:
- An `if` or `match` inside a block needs to produce the block's value
- Nested blocks need to exit an outer block
- You want to make the target scope explicit for clarity

```honey
# labeled yield from within a match arm
config := setup: {
    mut cfg := default_config()

    match mode {
        .fast: yield :setup fast_config(),
        .safe: {
            validate()
            yield :setup safe_config()
        },
    }

    yield cfg
}
```

A labeled `yield` that targets the immediately enclosing block is equivalent to an unlabeled one — the label is optional in that case:

```honey
x := blk: {
    yield :blk 42       # labeled — explicit
    # yield 42          # unlabeled — equivalent, less verbose
}
```

## Pointers

Honey has two kinds of pointers, both **non-nullable by default**:

| Type | Name | Arithmetic | Use Case |
| -- | -- | -- | -- |
| `@T` | Single-item pointer | No | Point to one value |
| `*T` | Many-item pointer | Yes | Point into arrays/buffers, low-level work |

Unlike C/C++ pointers, Honey pointers cannot be null unless explicitly marked optional (?@T or ?\*T). See Optional Types.

**Pointer operations:**

| Symbol | Context | Meaning |
| -- | -- | -- |
| `@T` | Type | Single-item pointer (read-only) |
| `@mut T` | Type | Single-item pointer (mutable) |
| `*T` | Type | Many-item pointer (read-only) |
| `*mut T` | Type | Many-item pointer (mutable) |
| `&` | Expression (prefix) | Address of |
| `^` | Expression (postfix) | Dereference |

### Single-Item vs Many-Item Pointers

Single-item pointers (`@T`) point to exactly one value. The compiler prevents arithmetic on them, catching bugs where you accidentally index a single value:

```honey
main :: func() void {
    x: i32 = 42
    ptr: @i32 = &x
    
    val := ptr^          # OK: dereference
    # bad := ptr + 1     # ERROR: no arithmetic on single-item pointer
}
```

Many-item pointers (`*T`) point to an unknown number of items and support pointer arithmetic. Use them for low-level memory work:

```honey
main :: func() void {
    arr: [4]i32 = {1, 2, 3, 4}
    ptr: *i32 = &arr[0]     # many-item pointer into array
    
    first := ptr^           # OK: dereference
    second := (ptr + 1)^    # OK: arithmetic, then dereference
}
```

### Pointer Mutability

Both pointer types respect the mutability of the data they point to. By default, pointers are read-only (`@T`, `*T`). To mutate through a pointer, you need a mutable pointer (@mut T, \*mut T) pointing to mutable data.

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

The pointer *variable* can also be mutable or immutable, independently of what it points to. This controls whether the pointer itself can be reassigned:

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

Pointers to structs are automatically dereferenced on field access — no explicit `^` is needed:

```honey
Buffer :: struct {
    data: *mut u8,
    len: u64,
}

main :: func() void {
    mut buf: Buffer = ...
    ptr: @mut Buffer = &buf

    data := ptr.data          # auto-deref: same as ptr^.data
    ptr.len = 100             # auto-deref: same as ptr^.len = 100
}
```

This works through multiple layers of pointers and nested structs:

```honey
Inner :: struct { value: i32 }
Outer :: struct { inner: Inner }

get_value :: func(p: @Outer) i32 {
    return p.inner.value      # auto-derefs p, then accesses inner.value
}
```

Explicit dereference (`ptr^.field`) still works and is equivalent. Mutability is checked against the pointer type — writing through an immutable pointer (`@T`) is an error regardless of whether you write `p.x = 1` or `p^.x = 1`.

### Pointer Arithmetic

Only many-item pointers (`*T`) support arithmetic. This prevents accidental arithmetic on pointers that are meant to reference a single value:

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

**Safety note:** Pointer arithmetic can create invalid pointers. The compiler does not bounds-check pointer arithmetic — it's the programmer's responsibility to ensure pointers remain valid. Out-of-bounds access is undefined behavior.

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
zeros: [16]i32 = {0} ** 16     # array initialized to all zeros
```

### Slices

Slices are a pointer-length pair that reference a contiguous sequence of elements. They don't own the data they point to:

```honey
arr: [4]u8 = {1, 2, 3, 4}
slice: []u8 = &arr            # slice referencing the array
```

**Subranging:** Slices and arrays can be narrowed using range syntax:

```honey
arr: [4]u8 = {1, 2, 3, 4}
first_two: []u8 = arr[0..2]      # elements 0, 1
last_two: []u8 = arr[2..4]       # elements 2, 3

data := allocator.alloc(u8, size: 100)
filled := data[0..bytes_written]  # narrow to the filled portion
```

### Sentinel-Terminated Types

For C interoperability, Honey supports sentinel-terminated slices and arrays. These guarantee a sentinel value (typically null) follows the data:

```honey
# Sentinel-terminated slice
c_string: [:0]u8 = "hello"    # null-terminated, length 5

# Sentinel-terminated array (fixed size, stack allocated)
buffer: [256:0]u8 = undefined # 256 bytes + null terminator
```

A `[:0]u8` has:

* A pointer to the data
* A length (excluding the sentinel)
* A guarantee that `data[len] == 0`

**Sentinel values other than zero:**

```honey
# Terminated by 0xFF instead of 0x00
packet: [:0xFF]u8 = ...
buffer: [64:0xFF]u8 = ...
```

**Sentinels are a C interop detail.** In normal Honey code, always use `.len` for bounds. The sentinel exists to satisfy C APIs that expect null-terminated strings — Honey code should treat slices uniformly regardless of whether they have a sentinel.

### Coercion

Sentinel-terminated types coerce to their non-sentinel equivalents:

```honey
process :: func(data: []u8) void { ... }

main :: func() void {
    c_str: [:0]u8 = "hello"
    process(c_str)  # [:0]u8 coerces to []u8
}
```

This is safe because `[]u8` makes no assumptions about what follows the data. The sentinel guarantee is simply dropped. The reverse (converting \[\]u8 to \[:0\]u8) requires allocation or copying, since there's no guarantee a null terminator exists:

```honey
main :: func() void {
    data: []u8 = get_some_data()  # not null-terminated
    
    # TODO: exact API TBD, but conversion requires allocation
    # because we must copy the data and append a null terminator
    c_str: [:0]u8 = ... # allocate, copy data, add sentinel
}
```

### Element Mutability

Following Honey's "immutable by default" principle, slice and array element mutability works the same as pointer mutability:

| Type | Meaning |
| -- | -- |
| `[]T` | Slice of immutable elements (cannot modify through slice) |
| `[]mut T` | Slice of mutable elements (can modify through slice) |
| `[:0]T` | Sentinel-terminated slice of immutable elements |
| `[:0]mut T` | Sentinel-terminated slice of mutable elements |
| `[N]T` | Array of immutable elements |
| `[N]mut T` | Array of mutable elements |
| `[N:0]T` | Sentinel-terminated array of immutable elements |
| `[N:0]mut T` | Sentinel-terminated array of mutable elements |

```honey
# immutable elements (default)
data: []u8 = &buffer
data[0] = 65              # ERROR: cannot modify through []u8

# mutable elements
mut_data: []mut u8 = &mut_buffer
mut_data[0] = 65          # OK: can modify through []mut u8

# sentinel-terminated, mutable
c_buffer: [:0]mut u8 = get_c_buffer()
c_buffer[0] = 65          # OK: can modify
```

### Slice Variable Mutability

Just like pointers, the slice *variable* itself can be mutable or immutable, independently of element mutability:

```honey
slice: []u8               # cannot reassign slice, cannot modify elements
slice: []mut u8           # cannot reassign slice, CAN modify elements
mut slice: []u8           # CAN reassign slice, cannot modify elements
mut slice: []mut u8       # CAN reassign slice, CAN modify elements
```

### String Literals

String literals have type `[:0]u8`—a sentinel-terminated slice of immutable bytes. This means they are always null-terminated and can be passed directly to C functions:

```honey
greeting: [:0]u8 = "hello"    # null-terminated string literal
name := "world"               # type inferred as [:0]u8
```

String literals coerce to `[]u8` when sentinel-termination is not required:

```honey
process :: func(data: []u8) void { ... }
main :: func() void {
    process("hello")  # [:0]u8 coerces to []u8
}
```

Since `[:0]u8` has immutable elements by default, you cannot modify a string literal (string literals are stored in read-only memory anyway).

### Multi-Line Strings

Multi-line strings use the `` ` `` character to mark each line. Content starts immediately after the pipe. Newlines between lines are implicit.

```honey
config: [:0]u8 =
    `# Database configuration
    `host = localhost
    `port = 5432
    `
    `[server]
    `address = 0.0.0.0
```

Key properties:

* Content begins immediately after `` ` `` 
* Newlines are automatically inserted between lines
* Empty `` ` `` produces a blank line
* No escape sequence processing (raw content)
* No trailing newline after the last line

If your content contains `` ` `` characters, only the leading pipe is special:

```honey
table: [:0]u8 =
    `Name    | Age | City
    `--------|-----|--------
    `Alice   | 30  | Paris
```

If you need a trailing newline, add an empty line at the end:

```honey
# No trailing newline
msg: [:0]u8 =
    `hello
    `world

# With trailing newline
msg: [:0]u8 =
    `hello
    `world
    `
```

To mix raw multi-line content with escape sequences, use concatenation:

```honey
message: [:0]u8 = 
    "Header:\t" ++
    `raw content here
    `more content
    `
    ++ "Footer"
```

### Mutability Comparison

The mutability model is consistent across pointers, arrays, and slices:

| Pointer | Array | Slice | Meaning |
| -- | -- | -- | -- |
| `@T` | `[N]T` | `[]T` | Immutable target/elements |
| `@mut T` | `[N]mut T` | `[]mut T` | Mutable target/elements |
| \-- | `[N:0]T` | `[:0]T` | Sentinel, immutable |
| \-- | `[N:0]mut T` | `[:0]mut T` | Sentinel, mutable |

## Optional Types

To represent a value that may or may not be present, use optional types with the `?` prefix.

### Basic Syntax

```honey
x: ?i32 = none      # optional with no value
y: ?u8 = 255        # optional with a value
z: ?@Buffer = none  # optional pointer (nullable pointer)
```

### Unwrapping Optionals

**Force unwrap with** `?` postfix - Extracts the value, traps/panics if `none`:

```honey
y: ?u8 = 255
val := y?  # val is u8, equals 255

x: ?i32 = none
bad := x?  # runtime trap/panic - x is none
```

**Unwrap with default using** `orelse`:

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

The guard is evaluated only if the optional contains a value. If the guard evaluates to false, the else branch (if present) is taken.

### Multi-Unwrap

Unwrap multiple optionals with `and`. This **short-circuits**: if the first optional is none, subsequent expressions are not evaluated.

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

Parentheses around the expression are optional, but can aid readability when combined with guards:

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
| -- | -- |
| `?T` | Optional type (may be `none`) |
| `none` | Absent value |
| `x orelse default` | Unwrap with fallback value |
| `x?` | Force unwrap (traps if `none`) |
| `if x |v| { }` | Conditional unwrap |
| `if x |v : guard| { }` | Conditional unwrap with guard |
| `if x and y |a, b| { }` | Multi-unwrap (short-circuits) |
| `if (x and y) |a, b : guard| { }` | Multi-unwrap with guard |

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

### Compile-time Functions

Functions declared with `comptime` exist only at compile time and cannot be called at runtime. All parameters are implicitly comptime.

**Comptime function calls must use the** `!` postfix to make compile-time

evaluation explicit in the code:

```honey
# all parameters implicitly comptime - no need to annotate them
make_array_type :: comptime func(T: type, N: u32) type {
    return [N]T
}

MyArray :: make_array_type(i32, 16)
```

Built-in comptime functions also use this syntax:

```honey
main :: func() void {
    size := size_of(Buffer)
    alignment := align_of(Buffer)
}
```

### Comptime Parameters

Runtime functions can have specific parameters marked as `comptime`. These must be known at compile time, enabling specialization.

```honey
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
    return size_of(T)
}

main :: func() void {
    MyArray :: make_array_type(i32, 16)

    print_n_times(5, "hello")
    x := sizeof(i32)
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
IntPair :: make_pair(i32)
FloatPair :: make_pair(f32)
```

### Function Inlining

Inlining is a runtime concern distinct from comptime. When a function is inlined, its body is copied to each call site instead of performing a jump-and-return.

`inline`: Force inline. Compiler errors if inlining is impossible:

```honey
add :: inline func(a: i32, b: i32) i32 {
    return a + b
}
```

`noinline`: Prevent inlining. Useful for debugging or controlling code size:

```honey
complex_operation :: noinline func(...) void {
    ...
}
```

**Unannotated functions** let the compiler decide based on optimization level. Annotations are instructions, not hints. If you annotate it, the compiler respects it.

### Default Arguments

Functions can have parameters with default values. When calling such functions, you can omit arguments that have defaults, or provide them using named argument syntax.

**Declaration:**

```honey
greet :: func(name: []u8, greeting: []u8 = "Hello", times: u32 = 1) void {
    for 0..times |_| {
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

**Named argument syntax is required** when skipping earlier defaulted parameters to set later ones. This avoids ambiguity about argument order:

```honey
greet("Eve", "Howdy", 5)    # positional: all arguments in order
greet("Eve", times: 5)      # named: skip greeting, set times
# greet("Eve", 5)           # ERROR: ambiguous - is 5 the greeting or times?
```

**Comptime parameters cannot have default values.**

### Default Arguments vs Descriptor Structs

For functions with many optional parameters, consider using a descriptor struct instead of many default arguments.

**Use default arguments when:**

* 1-3 optional parameters
* Parameters are independent of each other
* The common case uses mostly defaults

```honey
# Good use of default arguments
alloc :: func(comptime T: type, size: usize = 1) []T { ... }
read_file :: func(path: []u8, buffer_size: usize = 4096) ![]u8 { ... }
```

**Use descriptor structs when:**

* Many optional parameters (4+)
* Parameters form a logical group (configuration)
* You want to name the configuration for clarity

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

The descriptor struct approach has an advantage: adding new fields with defaults doesn't break any existing call sites. With default arguments, adding a new parameter (even with a default) changes the function signature, and likely the parameter order.

## First-Class Functions

Functions in Honey are first-class values. They can be passed as arguments, returned from other functions, and stored in structs. This enables powerful patterns like callbacks, higher-order functions, and configurable behavior. Honey does not have closures — functions cannot capture variables from their enclosing scope. If a function needs data, it takes it as a parameter. This keeps data flow explicit and avoids hidden state or lifetime complexity.

### Function Types

A function declaration uses the `func` keyword followed by its signature:

```honey
is_even :: func(x: i32) bool {
    return x % 2 == 0
}
```

To store or pass a function, you need a **pointer** to it—just like any other value. The type `@func(i32) bool` is a pointer to a function taking `i32` and returning bool:

```honey
Predicate :: @func(i32) bool
callback: Predicate = &is_even  # & takes pointer to function
```

This follows the same pattern as all other pointers in Honey:

```honey
x: i32 = 42
ptr: @i32 = &x           # pointer to i32
is_even :: func(x: i32) bool { ... }
fp: @func(i32) bool = &is_even  # pointer to function
```

### Named Parameters in Function Types

Parameter names can be added to function pointer types for documentation. Names are purely labels — they don't affect type compatibility:

```honey
# These are all the same type
@func(i32, i32) i32
@func(a: i32, b: i32) i32
@func(left: i32, right: i32) i32
@func(width: i32, height: i32) i32

# Named parameters make intent clear
AreaFunc :: @func(width: i32, height: i32) i32
DistanceFunc :: @func(x1: f32, y1: f32, x2: f32, y2: f32) f32
EventHandler :: @func(event: @Event, ctx: @mut Context) void

# Mixing named and unnamed is allowed
Comparator :: @func(a: i32, i32) i32  # second param unnamed
```

When assigning a function to a typed variable, only the types must match — the function's actual parameter names are irrelevant:

```honey
AreaFunc :: @func(width: i32, height: i32) i32

# Different parameter names - still compatible
compute :: func(w: i32, h: i32) i32 {
    return w * h
}

main :: func() void {
    calc: AreaFunc = &compute  # OK - signatures match structurally
}
```

### Passing Functions as Arguments

Functions can be passed to other functions, enabling callbacks and higher-order programming:

```honey
apply_twice :: func(x: i32, f: @func(i32) i32) i32 {
    return f(f(x))
}

double :: func(x: i32) i32 {
    return x * 2
}

square :: func(x: i32) i32 {
    return x * x
}

main :: func() void {
    a := apply_twice(5, &double)  # 20
    b := apply_twice(3, &square)  # 81
}
```

Common patterns like `map` and `filter`:

```honey
filter :: func(
    data: []i32,
    keep: @func(value: i32) bool,
    allocator: @Allocator,
) []i32 {
    # ... return new slice with elements where keep() returned true
}

map :: func(
    data: []i32,
    transform: @func(value: i32) i32,
    allocator: @Allocator,
) []i32 {
    # ... return new slice with transformed elements
}

is_positive :: func(x: i32) bool { return x > 0 }
negate :: func(x: i32) i32 { return -x }

main :: func() void {
    data: []i32 = &{-2, -1, 0, 1, 2}
    positives := filter(data, &is_positive, heap)
    defer heap.free(positives)
    negated := map(data, &negate, heap)
    defer heap.free(negated)
}
```

### Returning Functions

Functions can return pointers to other functions:

```honey
Operation :: enum { add, sub, mul, div }

add :: func(a: i32, b: i32) i32 { return a + b }
sub :: func(a: i32, b: i32) i32 { return a - b }
mul :: func(a: i32, b: i32) i32 { return a * b }
div :: func(a: i32, b: i32) i32 { return a / b }

get_operation :: func(op: Operation) @func(a: i32, b: i32) i32 {
    return match op {
        .add: &add,
        .sub: &sub,
        .mul: &mul,
        .div: &div,
    }
}

main :: func() void {
    f := get_operation(.mul)
    result := f(6, 7)  # 42
}
```

### Storing Functions in Structs

Functions can be stored as struct fields, enabling configurable behavior:

```honey
EventHandlers :: struct {
    on_click: @func(x: i32, y: i32, button: MouseButton) void,
    on_key: @func(key: KeyCode, modifiers: Modifiers) void,
    on_resize: @func(width: u32, height: u32) void,
}

Widget :: struct {
    bounds: Rect,
    handlers: EventHandlers,
}

# Handler implementations
handle_click :: func(x: i32, y: i32, button: MouseButton) void {
    print("click at ({d}, {d})", {x, y})
}

handle_key :: func(key: KeyCode, modifiers: Modifiers) void {
    print("key pressed: {}", {key})
}

handle_resize :: func(width: u32, height: u32) void {
    print("resized to {d}x{d}", {width, height})
}

main :: func() void {
    widget := Widget{
        .bounds = Rect{ .x = 0, .y = 0, .w = 100, .h = 50 },
        .handlers = EventHandlers{
            .on_click = &handle_click,
            .on_key = &handle_key,
            .on_resize = &handle_resize,
        },
    }

    # Invoke stored function
    widget.handlers.on_click(10, 20, .left)
}
```

### Optional Function Fields

Function fields can be optional for handlers that aren't always needed:

```honey
EventHandlers :: struct {
    on_click: ?@func(x: i32, y: i32) void,
    on_hover: ?@func(x: i32, y: i32) void,
    on_leave: ?@func() void,
}

dispatch_click :: func(handlers: @EventHandlers, x: i32, y: i32) void {
    if handlers.on_click |handler| {
        handler(x, y)
    }
}
```

### Functions with State

Since Honey has no closures, functions that need state take it as a parameter:

```honey
# State is explicit - passed in, not captured
process_with_count :: func(
    data: []i32,
    state: @mut ProcessState,
    handler: @func(value: i32, state: @mut ProcessState) void,
) void {
    for data |value| {
        handler(value, state)
    }
}

ProcessState :: struct {
    count: i32,
    sum: i64,
}

accumulate :: func(value: i32, state: @mut ProcessState) void {
    state.count += 1
    state.sum += value as i64
}

main :: func() void {
    data: []i32 = &{1, 2, 3, 4, 5}
    mut state := ProcessState{ .count = 0, .sum = 0 }
    process_with_count(data, &state, &accumulate)
    print("processed {d} items, sum = {d}", {state.count, state.sum})
}
```

### Anonymous Functions

For simple cases where naming a function adds noise, you can define a function inline as an expression. Anonymous functions cannot capture variables from their enclosing scope — they follow the same rules as named functions:

```honey
filtered := filter(data, &func(x: i32) bool { return x > 10 }, heap)
```

The `&func(...)` syntax creates a function and takes its address in one step — the same pattern as named functions, just without an intermediate name. For anything beyond a trivial expression, prefer a named function for readability.

### Idiomatic Patterns

While storing functions in structs enables method-like patterns, the idiomatic Honey approach uses namespaces to group related functions:

```honey
# Idiomatic: namespace groups type and operations
counter :: namespace {
    State :: struct { value: i32 }
    make :: func(initial: i32) State {
        return State{ .value = initial }
    }
    increment :: func(c: @mut State) void {
        c.value += 1
    }
    get :: func(c: @State) i32 {
        return c.value
    }
}

main :: func() void {
    mut c := counter.make(0)
    counter.increment(&c)
    counter.increment(&c)
    print("count: {d}", {counter.get(&c)})  # 2
}
```

Reserve function-in-struct patterns for cases where the behavior truly needs to vary per instance, such as event handlers, plugins, or strategy patterns.

## Imports

### Importing Modules

To import standard Honey namespaces, the `import` keyword can be used.

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

Namespace aliasing can be used to achieve the same result.

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

Alternatively, the fully qualified paths can also be used.

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

### Importing Source Code

To import source code directly, the relative path to the file can be used.

```hon
import "path/to/file.hon"
```

To import external source code (be it C, Fortran, Cobol, etc.), an external import can be used. Taking C as example:

```hon
c import include "path/to/file.h"
```

To define a macro for the namespace generated from the C header file, the `define` keyword can be used.

```hon
c import {
  include "path/to/file.h"
  define "WHATEVER"
  define "SOME_CONST 2.72"
}
```

## Memory Allocation

Memory allocation in Honey is designed to be **explicit but not verbose**. We reject the dogma that global state is inherently evil — allocators are a cross-cutting concern that nearly every function needs, making them a perfect candidate for sensible defaults.

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
│  6. Escaping allocations: caller provides allocator         │
└─────────────────────────────────────────────────────────────┘
```

### The Default Heap Allocator

Honey provides a **thread-local global heap allocator** that is:

* Determined at compile time by build mode
* **Immutable at runtime** — cannot be reconfigured

```honey
import "std/mem/heap"

process :: func(input: []u8) u64 {
    # heap used for internal temporary work — does not escape
    temp := heap.alloc(u8, size: input.len * 2)
    defer heap.free(temp)

    # ... work with temp ...

    return compute_hash(temp)  # only the result escapes, not the allocation
}
```

The behavior of `heap` depends on build mode:

| Build Mode | Allocator Behavior |
| -- | -- |
| Debug | Tracking allocator with leak detection |
| Release | Fast allocator, zero overhead |
| ReleaseSafe | Bounds-checking allocator |

This is configured at compile time. You cannot change which allocator `heap` uses at runtime. This is intentional — it prevents bugs where memory allocated with one allocator is freed with another.

### The Escaping Allocation Rule

**If a function heap-allocates memory that escapes its scope — whether via the return value or via writes through mutable parameters — the function must accept an allocator parameter.** The presence of an allocator parameter is the contract that says "heap memory escapes here, and you're responsible for it."

This rule makes ownership transfer visible at the function signature level. The caller never needs to read the function's implementation to know whether heap cleanup is involved:

```honey
import "std/mem"
import "std/mem/heap"

# Allocation escapes via return value — requires allocator
duplicate :: func(input: []u8, allocator: @mem.Allocator) []u8 {
    result := allocator.alloc(u8, size: input.len)
    mem.copy(result, input)
    return result  # caller manages this memory
}

# Allocation escapes via mutable parameter — requires allocator
init :: func(obj: @mut MyStruct, allocator: @mem.Allocator) void {
    obj.buffer = allocator.alloc(u8, size: 100)
    # caller now knows heap memory was written into obj
}

# No allocation escapes — no allocator needed
process :: func(input: []u8) u64 {
    temp := heap.alloc(u8, size: input.len)
    defer heap.free(temp)
    # ... work with temp ...
    return compute_hash(temp)
}

# No heap allocation at all — no allocator needed
reset :: func(obj: @mut MyStruct) void {
    obj.count = 0
}

main :: func() void {
    data := duplicate("hello", heap)
    defer heap.free(data)

    mut obj := MyStruct{ ... }
    init(&obj, heap)
    defer heap.free(obj.buffer)
}
```

**Why this matters:**

* Without the rule, a function like `init(obj: @mut MyStruct) void` is ambiguous — did it heap-allocate into `obj`, or just set some fields to stack/static data? The caller has no way to know without reading the implementation.
* With the rule, the allocator parameter is a clear signal: "this function produces heap memory that outlives its scope, and you are responsible for cleaning it up."
* Internal allocations (temporary buffers, scratch space) use `heap` directly and are freed before the function returns. No allocator parameter needed, no burden on the caller.

**The compiler enforces this rule.** If a function heap-allocates memory that escapes without accepting an allocator parameter, the compiler emits an error.

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

This is "action at a distance" — the behavior of `heap.free()` depends on what some unrelated code did earlier. By making `heap` immutable, Honey guarantees:

**Whatever you allocate with, you free with.**

### Custom Allocators

For specialized needs, you create explicit allocator instances. These are not global — you manage their lifetime and pass them where needed.

**Arena Allocator**: Fast bump allocation, bulk deallocation:

```honey
import "std/mem"
import "std/mem/heap"

process_file :: func(path: []u8, allocator: @mem.Allocator) !Data {
    # arena manages its own backing memory via heap
    arena := mem.Arena.init(heap, capacity: mem.megabytes(1))
    defer arena.deinit()

    # all temporary allocations from arena (fast bump allocation)
    file_contents := arena.alloc(u8, size: file_size)
    parsed := arena.alloc(ParsedData)         # size defaults to 1
    tokens := arena.alloc(Token, size: 1000)

    # ... process ...

    # escaping allocation uses the caller's allocator
    result := allocator.create(Data)
    mem.copy(result, parsed)

    return result
    # arena.deinit() frees all arena memory — no individual frees needed
}
```

**Pool Allocator**: O(1) fixed-size allocation, no fragmentation:

```honey
import "std/mem"
import "std/mem/heap"

EntitySystem :: struct {
    pool: mem.Pool(Entity),
}

init_entities :: func(allocator: @mem.Allocator) EntitySystem {
    return EntitySystem{
        pool = mem.Pool(Entity).init(allocator, capacity: 10_000),
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

As described in the escaping allocation rule, when a function heap-allocates memory that escapes its scope, it must accept an allocator parameter. The caller decides which allocator to use:

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
    arena := mem.Arena.init(heap, capacity: mem.kilobytes(64))
    defer arena.deinit()
    result := parse(input, &arena) catch |err| {
        # handle error
    }

    # or use a pool
    pool := mem.Pool(ParseResult).init(capacity: 100)
    defer pool.deinit()
    result := parse(input, &pool) catch |err| {
        # handle error
    }
}
```

### Memory Allocation Summary

| What | How | When to Use |
| -- | -- | -- |
| `heap.alloc(T, size: n)` | Thread-local global | General purpose, 90% of cases |
| `heap.create(T)` | Thread-local global | Allocate single item |
| `allocator.alloc(T, size: n)` | Caller-provided | Escaping allocations (returned or written to caller's data) |
| `arena.alloc(T, size: n)` | Explicit instance | Temporary/scoped work, bulk free |
| `pool.alloc()` | Explicit instance | Many same-sized objects, O(1) |

Note that `heap` satisfies the `Allocator` interface, so callers can pass `heap` as the allocator argument when they don't need a specialized allocator — which is most of the time.

**The golden rule:** Allocate and free with the same allocator. The type system helps enforce this — memory from `heap` can only be freed with `heap`, memory from your arena can only be freed with that arena. The escaping allocation rule ensures the caller always knows which allocator was used.

### Compared to Other Languages

| Language | Approach | Honey's Advantage |
| -- | -- | -- |
| C | Hidden malloc, easy to mismatch | Explicit allocator at call site |
| C++ | Allocator templates, complex | Simple, no template complexity |
| Rust | Explicit everywhere, verbose | Sensible defaults reduce noise |
| Zig | Allocator parameter threading | Only required for escaping allocations, not internal work |
| Odin | Hidden context parameter | Fully transparent, nothing hidden |
| Go | Hidden GC | Explicit control, no GC pauses |

Honey sits in a sweet spot: explicit enough to always know what's happening, convenient enough that you don't drown in boilerplate.

## C Interoperability

Honey is designed for seamless interoperability with C libraries. The key mechanisms are sentinel-terminated types, ABI-specific declarations, and opaque types.

### C Functions

Declare C functions using the `c` modifier:

```honey
# External C function (no body = defined elsewhere)
puts :: c func(s: [:0]u8) i32
strlen :: c func(s: [:0]u8) usize
malloc :: c func(size: usize) ?*mut u8
free :: c func(ptr: *mut u8) void
```

The `c` modifier indicates the function uses the C calling convention. The absence of a body indicates the function is defined externally (in a C library or object file).

**Honey functions callable from C:**

```honey
# Honey implementation with C calling convention
my_callback :: c func(data: @u8, len: usize) i32 {
    # Honey code, but callable from C
    return 0
}
```

This is useful for callbacks, plugins, or exposing a C API from Honey code.

### Other ABIs

The same pattern extends to other calling conventions:

```honey
# Future support for other ABIs
fortran_sub :: fortran func(...)
cobol_proc :: cobol func(...)
```

### String Interop

Honey string literals are `[:0]u8`, making C interop natural:

```honey
puts :: c func(s: [:0]u8) i32
getenv :: c func(name: [:0]u8) ?[:0]u8

main :: func() void {
    # String literals work directly
    puts("Hello from Honey!")
    
    # Receiving C strings
    if getenv("PATH") |path| {
        puts(path)
    }
}
```

**Performance note:** When receiving a `[:0]u8` from C and only passing it to other C functions (never accessing .len), the compiler may skip length computation entirely.

### Opaque Types

For C types you only interact with through pointers, use `opaque`:

```honey
# Opaque type - can only be used through pointers
FILE :: opaque

fopen :: c func(path: [:0]u8, mode: [:0]u8) ?@mut FILE
fclose :: c func(file: @mut FILE) i32
fread :: c func(buf: *mut u8, size: usize, count: usize, file: @mut FILE) usize

main :: func() void {
    if fopen("data.txt", "r") |file| {
        defer fclose(file)
        
        buffer: [1024]mut u8 = undefined
        bytes_read := fread(&buffer, 1, 1024, file)
        # ...
    }
}
```

Opaque types cannot be:

* Instantiated directly (no `FILE{}` or stack allocation)
* Measured (`size_of(FILE)` is an error)
* Copied or inspected

They can only be used as pointer targets (`@FILE`, `?@FILE`, etc.), which matches how C APIs expose them.

### Struct Layout

By default, Honey structs may have fields reordered or padded for optimization:

```honey
# Honey struct - compiler may optimize layout
Point :: struct {
    x: f64,
    y: f64,
}
```

For C interop, use `c struct` to guarantee C-compatible layout:

```honey
# C-layout struct - guaranteed to match C
Point :: c struct {
    x: f64,
    y: f64,
}

distance :: c func(a: @Point, b: @Point) f64
```

Use `c struct` when:

* Passing structs to/from C functions
* Memory-mapping C data structures
* Interfacing with hardware or file formats with specific layouts

**Other ABI layouts:**

```honey
# Fortran-compatible layout (future)
Matrix :: fortran struct {
    data: *f64,
    rows: i32,
    cols: i32,
}
```

### Pointer Correspondence

| Honey | C Equivalent | Notes |
| -- | -- | -- |
| `@T` | `const T*` | Single item, non-null |
| `@mut T` | `T*` | Single item, non-null, mutable |
| `?@T` | `const T*` | Single item, nullable |
| `?@mut T` | `T*` | Single item, nullable, mutable |
| `*T` | `const T*` | Many items, non-null |
| `*mut T` | `T*` | Many items, non-null, mutable |
| `?*T` | `const T*` | Many items, nullable |
| `?*mut T` | `T*` | Many items, nullable, mutable |
| `[:0]u8` | `const char*` | Null-terminated string |
| `[:0]mut u8` | `char*` | Mutable null-terminated string |

**Note:** C makes no distinction between single-item and many-item pointers, or between nullable and non-nullable. Honey's type system is stricter—choose the appropriate type based on how the C API actually uses the pointer.

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

## Tuples

Tuples are anonymous structs with positional (unnamed) fields. They use `{}` syntax — the same braces as named structs — because conceptually they are the same thing, just with fields accessed by position instead of name.

### Tuple Types

A tuple type is a struct with positional field types:

```honey
Pair :: struct {i32, [:0]u8}
Point :: struct {f32, f32}
```

This is equivalent to a struct with fields named `0`, `1`, etc. internally.

### Literals and Access

```honey
p := Point{1.0, 2.0}
x := p.0                  # f32: 1.0
y := p.1                  # f32: 2.0
```

```honey
info: Pair = {42, "hello"}
id := info.0               # i32: 42
name := info.1             # [:0]u8: "hello"
```

### Anonymous Tuples

Tuples can be used without a named type declaration:

```honey
t := {1, "hello", 3.14}
first := t.0
```

The type is inferred from context, the same way numeric literals resolve through type anchoring.

### In Named Structs

Tuple types can be used as struct field types:

```honey
Line :: struct {
    start: struct {f32, f32},
    end: struct {f32, f32},
}
```

### Tuple vs Named Struct

The distinction is purely syntactic — whether fields have names or not:

```honey
# Named struct: fields accessed by name
Point :: struct { x: f32, y: f32 }
p := Point{ .x = 1.0, .y = 2.0 }
p.x  # by name

# Tuple struct: fields accessed by position
Point2 :: struct { f32, f32 }
q := Point2{1.0, 2.0}
q.0  # by position
```

Both are structs under the hood. There is no separate `tuple` keyword.

## Variadic Arguments

Honey supports type-safe variadic arguments through comptime tuple collection. Variadic calls are fully type-checked at compile time — no runtime type erasure, no `va_list`.

### Syntax

A function declares a variadic parameter with `...` as the type of its last parameter:

```honey
print :: func(comptime fmt: [:0]u8, args: ...) void {
    # 'args' is a comptime tuple — access via args.0, args.1, etc.
    # fmt is validated against the argument types at compile time
}
```

At each call site, the compiler knows the exact types and count of the variadic arguments:

```honey
print("x={} y={}", x, y)          # args = {x, y} — two args
print("name: {}", name)           # args = {name} — one arg
print("no args")                   # args = {} — zero args
```

The `...` parameter must be the last parameter in the function signature. The compiler packages the extra arguments into a comptime tuple internally and generates specialized code for each call site.

### C Variadic Interop

For calling C variadic functions (`printf`, `snprintf`, etc.), the `...` parameter is unnamed — there's no function body to access them from:

```honey
printf :: c func(fmt: [:0]u8, ...) i32

main :: func() void {
    printf("Hello %s, you are %d\n", "world", 42)
}
```

The compiler passes each extra argument according to the C calling convention.

**Note:** C variadic calls are inherently type-unsafe (the compiler trusts the format string). Honey's own `print` function would use comptime format string validation to catch mismatches at compile time.

### Design Rationale

- **No macros:** Unlike Rust's `println!`, Honey's print is a regular function
- **No runtime overhead:** Unlike Go's `...any`, there's no boxing or runtime type dispatch — the compiler monomorphizes each call
- **Compile-time safety:** Format string mismatches in Honey functions are compile errors, not runtime crashes
- **C compatible:** Variadic arguments unpack naturally to C variadic arguments
- **Separation of concerns:** Tuples are a data structure (positional structs); variadics are a calling convention (`...`). They are independent features that compose naturally

## Type Aliasing

```honey
EntityId :: u32
EntityId: type :: u32  # equivalent, explicit form
```

## Composition Over Inheritance

Honey does not have class-based inheritance. This is a deliberate design choice, not an omission. Inheritance hierarchies tend to create rigid, tightly-coupled code that becomes difficult to modify as requirements evolve. Instead, Honey encourages composition: building complex types by combining simpler ones, and **interfaces**: contracts about what functions must exist.

### The Problem with Inheritance

In languages with inheritance, you often see patterns like:

```
Animal
├── Mammal
│   ├── Dog
│   └── Cat
└── Bird
    ├── Eagle
    └── Penguin  # Can't fly... awkward
```

This creates problems:

* **Rigid hierarchies:** Adding "flying mammal" (bat) breaks the model
* **God classes:** Base classes accumulate methods "just in case"
* **Hidden coupling:** Changing a base class ripples through all descendants
* **Diamond problem:** Multiple inheritance creates ambiguity

### Honey's Model

Honey enforces a clean separation:

```
┌─────────────────────────────────────────────────────────────┐
│  NAMESPACES                                                 │
│  - Organization units                                       │
│  - Contain: functions, constants, types, nested namespaces  │
│  - Can have mutable state (module-level globals)            │
│  - Can implement interfaces                                 │
└─────────────────────────────────────────────────────────────┘
┌─────────────────────────────────────────────────────────────┐
│  STRUCTS                                                    │
│  - Pure data layout                                         │
│  - Contain: fields only                                     │
│  - No methods, no behavior                                  │
└─────────────────────────────────────────────────────────────┘
┌─────────────────────────────────────────────────────────────┐
│  INTERFACES                                                 │
│  - Contracts about what a namespace must provide            │
│  - Contain: associated types, function signatures           │
│  - Checked at compile time via `impl` declarations          │
└─────────────────────────────────────────────────────────────┘
```

## Namespaces

Namespaces are organizational units that contain functions, types, constants, and can even hold mutable state. They are the primary way to group related functionality.

### Basic Namespace Syntax

```honey
geometry :: namespace {
    # Constants
    PI :: 3.14159265359
    
    # Types (pure data)
    Point :: struct {
        x: f32,
        y: f32,
    }
    
    Circle :: struct {
        center: Point,
        radius: f32,
    }
    
    # Functions
    distance :: func(a: @Point, b: @Point) f32 {
        dx := b.x - a.x
        dy := b.y - a.y
        return sqrt(dx * dx + dy * dy)
    }
    
    area :: func(c: @Circle) f32 {
        return PI * c.radius * c.radius
    }
}

main :: func() void {
    p1 := geometry.Point{ .x = 0, .y = 0 }
    p2 := geometry.Point{ .x = 3, .y = 4 }
    
    d := geometry.distance(&p1, &p2)  # 5.0
}
```

### Nested Namespaces

Namespaces can be nested to create hierarchies:

```honey
# std/mem.hon

mem :: namespace {
    # Direct contents of mem
    copy :: func(dst: []mut u8, src: []u8) void { ... }
    
    # Nested namespace
    heap :: namespace {
        alloc :: func(comptime T: type, size: usize = 1) []T { ... }
        free :: func(ptr: *mut u8) void { ... }
    }
    
    # Another nested namespace
    arena :: namespace {
        State :: struct { ... }
        
        init :: func(backing: []mut u8) State { ... }
        alloc :: func(state: @mut State, comptime T: type, size: usize = 1) []T { ... }
    }
}

# Usage
main :: func() void {
    buffer := mem.heap.alloc(u8, size: 1024)
    defer mem.heap.free(buffer.ptr)
    
    mem.copy(buffer, "hello")
}
```

### Namespaces with Mutable State

Namespaces can contain mutable globals, enabling singleton patterns:

```honey
heap :: namespace {
    # Private mutable state
    mut state: HeapState = HeapState.init()
    
    # Public functions operate on the state
    alloc :: func(comptime T: type, size: usize = 1) []T {
        # operates directly on state
        ...
    }
    
    free :: func(ptr: *mut u8) void {
        # operates directly on state
        ...
    }
}

# Clean usage - no state passing needed
main :: func() void {
    buffer := heap.alloc(u8, size: 1024)
    defer heap.free(buffer.ptr)
}
```

### Functions Over Methods

Rather than methods attached to types, write functions that operate on the data they need:

```honey
# Define data
Position :: struct { x: f32, y: f32 }
Velocity :: struct { dx: f32, dy: f32 }
Health :: struct { current: u32, max: u32 }

# Functions work on specific data, not class hierarchies
physics :: namespace {
    move :: func(pos: @mut Position, vel: @Velocity, dt: f32) void {
        pos.x += vel.dx * dt
        pos.y += vel.dy * dt
    }
}

combat :: namespace {
    take_damage :: func(health: @mut Health, amount: u32) void {
        if amount >= health.current {
            health.current = 0
        } else {
            health.current -= amount
        }
    }
    
    is_alive :: func(health: @Health) bool {
        return health.current > 0
    }
}

# Compose entities from the parts they need
Player :: struct {
    pos: Position,
    vel: Velocity,
    health: Health,
}

Projectile :: struct {
    pos: Position,
    vel: Velocity,
    # No health - projectiles don't need it
}

# Update functions use the appropriate operations
update_player :: func(player: @mut Player, dt: f32) void {
    physics.move(&player.pos, &player.vel, dt)
}

update_projectile :: func(proj: @mut Projectile, dt: f32) void {
    physics.move(&proj.pos, &proj.vel, dt)
}
```

## Interfaces

Interfaces define contracts about what functions and types a namespace must provide. They enable generic programming without inheritance or virtual dispatch.

### Basic Interface Syntax

An interface declares associated types and function signatures:

```honey
Serializable :: interface {
    # Associated types
    Data: type,
    
    # Required functions
    serialize: func(value: @Data, writer: @Writer) void!,
    deserialize: func(reader: @Reader) Data!,
}
```

A namespace satisfies an interface by providing all required types and functions.

Use `impl` to declare this (idiomatically placed at the top of the namespace):

```honey
json :: namespace {
    impl Serializable  # compile-time assertion
    
    Data :: Point
    
    serialize :: func(value: @Data, writer: @Writer) void! {
        try writer.write_str("{\"x\":")
        try writer.write_f32(value.x)
        try writer.write_str(",\"y\":")
        try writer.write_f32(value.y)
        try writer.write_str("}")
    }
    
    deserialize :: func(reader: @Reader) Data! {
        # parse JSON...
    }
}
```

### Associated Types

All types in an interface are associated types. The implementing namespace provides concrete types for each:

```honey
Collection :: interface {
    Container: type,
    Element: type,
    
    len: func(c: @Container) usize,
    get: func(c: @Container, idx: usize) ?@Element,
    set: func(c: @mut Container, idx: usize, value: Element) void,
}

int_array :: namespace {
    impl Collection
    
    Container :: struct { items: [256]i32, count: usize }
    Element :: i32
    
    len :: func(c: @Container) usize { 
        return c.count 
    }
    
    get :: func(c: @Container, idx: usize) ?@Element {
        if idx >= c.count { return none }
        return &c.items[idx]
    }
    
    set :: func(c: @mut Container, idx: usize, value: Element) void {
        if idx >= 256 { return }
        c.items[idx] = value
        if idx >= c.count { c.count = idx + 1 }
    }
}
```

### Using Interfaces in Generic Code

Generic functions take implementation namespaces as comptime parameters:

```honey
# I is a namespace that satisfies Collection
sum_all :: func(comptime I: Collection, container: @I.Container) i64 {
    mut total: i64 = 0
    for 0..I.len(container) |i| {
        if I.get(container, i) |val| {
            total += val^ as i64
        }
    }
    return total
}

main :: func() void {
    mut arr: int_array.Container = ...
    
    # Pass the implementing namespace explicitly
    total := sum_all(int_array, &arr)
}
```

The implementation is always explicit at the call site. No hidden dispatch, no "which implementation wins?" questions.

### Associated Namespaces

Interfaces can require associated namespaces that satisfy other interfaces. This lets you compose behavior from existing implementations:

```honey
Comparable :: interface {
    Element: type,
    compare: func(a: @Element, b: @Element) Ordering,
}

SortedCollection :: interface {
    Container: type,
    Element: type,
    Ops: Comparable,  # must be a namespace satisfying Comparable

    insert: func(c: @mut Container, value: Element) void,
    find: func(c: @Container, value: @Element) ?@Element,
}

HashMap :: interface {
    Map: type,
    Key: type,
    KeyOps: Comparable,  # associated namespace
    Value: type,

    get: func(m: @Map, key: @Key) ?@Value,
    put: func(m: @mut Map, key: Key, value: Value) void,
}
```

The implementing namespace provides a concrete namespace for each associated namespace. The compiler verifies that the provided namespace satisfies the required interface:

```honey
# Standard library provides canonical implementations
# std/ops.hon

ops :: namespace {
    i32 :: namespace {
        impl Comparable
        
        Element :: i32
        
        compare :: func(a: @Element, b: @Element) Ordering {
            if a^ < b^ { return .less }
            if a^ > b^ { return .greater }
            return .equal
        }
    }
    
    f32 :: namespace {
        impl Comparable
        
        Element :: f32
        
        compare :: func(a: @Element, b: @Element) Ordering { ... }
    }
    
    # ... etc for other primitive types
}
```

Now implementations reuse standard library namespaces:

```honey
import "std/ops"

int_hash_map :: namespace {
    impl HashMap
    
    Map :: struct { entries: [256]?Entry, count: usize }
    Entry :: struct { key: i32, value: []u8 }
    
    Key :: i32
    KeyOps :: ops.i32  # reuse standard library!
    Value :: []u8
    
    get :: func(m: @Map, key: @Key) ?@Value { 
        # can use KeyOps.compare(a, b) here
        ...
    }
    
    put :: func(m: @mut Map, key: Key, value: Value) void { ... }
}
```

For custom types, create your own ops namespace:

```honey
Point :: struct { x: f32, y: f32 }

point_ops :: namespace {
    impl Comparable
    
    Element :: Point
    
    compare :: func(a: @Element, b: @Element) Ordering {
        # compare by x, then by y
        if a.x < b.x { return .less }
        if a.x > b.x { return .greater }
        if a.y < b.y { return .less }
        if a.y > b.y { return .greater }
        return .equal
    }
}
```

### Multiple Associated Namespaces

When a generic data structure needs multiple kinds of behavior for the same type, require multiple associated namespaces:

```honey
Hashable :: interface {
    Element: type,
    hash: func(value: @Element) u64,
}

HashSet :: interface {
    Set: type,
    Element: type,
    Cmp: Comparable,   # comparison behavior
    Hash: Hashable,    # hashing behavior

    insert: func(s: @mut Set, value: Element) bool,
    contains: func(s: @Set, value: @Element) bool,
    remove: func(s: @mut Set, value: @Element) bool,
}
```

The implementing namespace provides a concrete namespace for each. The compiler catches type mismatches when the generic code uses them together:

### Comptime Functions Returning Namespaces

Comptime functions can return namespaces, enabling generic implementations:

```honey
storage :: comptime func(T: type, size: usize) namespace {
    return namespace {
        impl Collection
        
        Container :: struct { items: [size]T }
        Element :: T
        
        len :: func(c: @Container) usize { 
            return size 
        }
        
        get :: func(c: @Container, idx: usize) ?@Element {
            if idx >= size { return none }
            return &c.items[idx]
        }
        
        set :: func(c: @mut Container, idx: usize, value: Element) void {
            if idx >= size { return }
            c.items[idx] = value
        }
    }
}

# Instantiate specialized namespaces
IntStorage :: storage(i32, 100)
FloatStorage :: storage(f32, 256)

main :: func() void {
    mut ints: IntStorage.Container = undefined
    IntStorage.set(&ints, 0, 42)
    
    # Works with generic code
    total := sum_all(IntStorage, &ints)
}
```

### Fully Generic Data Structures

Combining all features for a fully generic hash map:

```honey
import "std/ops"

hash_map :: comptime func(
    K: type,
    V: type,
    KOps: Comparable,
    capacity: usize = 256,
) namespace {
    return namespace {
        impl HashMap
        
        Map :: struct {
            entries: [capacity]?Entry,
            count: usize,
        }
        Entry :: struct { key: K, value: V, hash: u64 }
        
        Key :: K
        KeyOps :: KOps
        Value :: V
        
        get :: func(m: @Map, key: @Key) ?@Value {
            # use KeyOps.compare for key comparison
            ...
        }
        
        put :: func(m: @mut Map, key: Key, value: Value) void { ... }
    }
}

# Usage with primitives
IntStringMap :: hash_map(i32, []u8, ops.i32)

# Usage with custom types
PointDataMap :: hash_map(Point, Data, point_ops, capacity: 1024)

main :: func() void {
    mut m: IntStringMap.Map = undefined
    IntStringMap.put(&m, 42, "answer")
    
    if IntStringMap.get(&m, &42) |val| {
        print(val^)
    }
}
```

## Interface Summary

| Syntax | Meaning |
| -- | -- |
| `interface { ... }` | Define an interface |
| `Name: type` | Associated type |
| `Name: Interface` | Associated namespace satisfying Interface |
| `impl InterfaceName` | Declare that this namespace satisfies the interface |
| `comptime I: Interface` | Generic parameter requiring a satisfying namespace |
| `I.TypeName` | Access associated type from implementation |
| `I.func_name(...)` | Call function from implementation |

## Design Principles

1. **Structs are pure data.** No methods, no behavior, just field layout.
2. **Namespaces hold behavior.** Functions, constants, types, nested namespaces.
3. **Interfaces are contracts.** They specify what a namespace must provide.
4. **Implementations are explicit.** You always see which namespace provides which functions at the call site.
5. **No hidden dispatch.** Runtime polymorphism requires explicit vtables.
6. **Composition over inheritance.** Build complex behavior by combining simple namespaces, not by inheriting from base classes.

## Error Handling

Honey handles errors as values. There is no hidden control flow — a function that can fail declares this in its signature, and the caller must explicitly handle the possibility of failure.

### Fallible Functions

Functions that can fail declare their error type after `!`:

```honey
read_file :: func(path: []u8) []u8 ! IoError { ... }
```

This reads as: "returns `[]u8` or fails with `IoError`." The space around `!` is idiomatic but not required.

### Error Types

Errors can be enums (when you only need to identify what went wrong) or tagged unions (when errors need to carry additional context).

**Simple errors (enum):**

```honey
SimpleError :: enum {
    OutOfMemory,
    InvalidSize,
    Timeout,
}
```

**Rich errors (tagged union):**

```honey
IoError :: union(enum) {
    NotFound: struct { path: []u8 },
    PermissionDenied: struct { path: []u8, operation: []u8 },
    Timeout: struct { after_ms: u64 },
    ConnectionReset: void,  # no additional data needed
}
```

**Constrained tagged union:**

When you have a predefined set of error kinds, you can constrain the union:

```honey
IoErrorKind :: enum {
    not_found,
    permission_denied,
    timeout,
}

IoError :: union(IoErrorKind) {
    not_found: struct { path: []u8 },
    permission_denied: struct { path: []u8 },
    timeout: struct { after_ms: u64 },
}
```

### Error Composition

Functions that can fail with multiple error types use `|` to compose them:

```honey
process :: func(path: []u8) Ast ! IoError | ParseError { ... }
```

Parentheses are optional but can aid readability:

```honey
process :: func(path: []u8) Ast ! (IoError | ParseError) { ... }
```

Composed error types can also be defined standalone:

```honey
ProcessError :: IoError | ParseError | ValidationError
process :: func(path: []u8) Ast ! ProcessError { ... }
```

### Inline Error Types

For simple one-off cases, you can inline the error type:

```honey
# Inline enum
simple :: func() void ! enum{Failed, Timeout} { ... }

# Inline tagged union (when payloads are needed)
complex :: func() Data ! union(enum) {
    OutOfMemory: struct { requested: usize },
    InvalidInput: void,
} { ... }
```

For anything non-trivial, prefer named error types for clarity.

### Returning Errors

The `error` keyword is a control flow statement that exits the function via the error channel — symmetric to return for success values:

```honey
parse_section :: func(p: @mut Parser) void ! ParseError {
    start_line := p.line
    p.advance()
    
    # ... parsing logic ...
    
    if p.pos >= p.input.len or p.input[p.pos] != ']' {
        error .unclosed_section{.line = start_line}
    }
    
    # ... continue on success ...
}
```

Since errors are just union values, you can also construct them separately:

```honey
# Construct error value (it's just a union)
e: ParseError = .timeout{.ms = 500}

# Return it via error channel later
error e
```

The symmetry:

* `return x` — exit function with success value
* `error e` — exit function with error value

### Propagation with `try`

The `try` keyword unwraps a successful result or returns early with the error:

```honey
process :: func(path: []u8) Ast ! IoError | ParseError {
    data := try read_file(path)   # returns early if IoError
    ast := try parse(data)        # returns early if ParseError
    return ast
}
```

When the function's error type is a composition, `try` automatically widens narrower error types to match the return type.

### Handling with `catch`

The `catch` keyword handles errors and provides a value to continue with.

**Provide a fallback value:**

```honey
data := read_file(path) catch default_data
```

**Handle with a block using** `yield`:

```honey
data := read_file(path) catch |e| {
    log("read failed: {}", {e})
    yield empty_data
}
use(data)  # continues with data = empty_data
```

The `yield` keyword provides a value from a block to the enclosing expression. Execution continues after the statement. For single expressions, yield is implicit (e.g., `catch default_data`). For blocks, explicit `yield` is required. See the Yield section under Control Flow for the full rule.

**Return from the function instead:**

```honey
data := read_file(path) catch |e| {
    log("read failed: {}", {e})
    return  # exits the enclosing function
}
use(data)  # never reached if error occurred
```

Use `return` when the error is unrecoverable at this level.

**Match on specific error variants:**

```honey
data := read_file(path) catch |e| match e {
    .NotFound |info|: {
        print("file not found: {s}", {info.path})
        yield create_default(info.path)
    },
    .Timeout |info|: {
        print("timed out after {d}ms", {info.after_ms})
        yield retry(path)
    },
    .PermissionDenied: panic("cannot recover from permission error"),
    .ConnectionReset: retry(path),
}
```

### Summary

| Syntax | Meaning |
| -- | -- |
| `T ! E` | Function returns `T` or fails with `E` |
| `T ! E1 | E2` | Function can fail with either error type |
| `T ! (E1 | E2)` | Same, with optional parentheses |
| `error e` | Exit function via error channel |
| `error .variant{...}` | Construct and return error in one step |
| `try expr` | Unwrap success or propagate error |
| `expr catch fallback` | Provide fallback value on error |
| `expr catch |e| { ... }` | Handle error with block |
| `yield value` | Provide value from innermost block |
| `yield :label value` | Provide value from labeled block |
| `return` / `return value` | Exit function from within catch block |

## Summary of Syntax

### Declarations

| Syntax | Meaning |
| -- | -- |
| `NAME :: expr` | Compile-time constant |
| `name := expr` | Runtime immutable (initialized at startup) |
| `mut name := expr` | Runtime mutable |
| `name: T = undefined` | Explicitly uninitialized variable |
| `_ = expr` | Discard a value |
| `name :: func(...) T { }` | Runtime function |
| `name :: comptime func(...) T { }` | Compile-time only function |
| `name :: inline func(...) T { }` | Force-inlined runtime function |
| `name :: noinline func(...) T { }` | Never-inlined runtime function |
| `name :: c func(...) T` | C function declaration (external) |
| `name :: c func(...) T { }` | Honey function with C calling convention |

### Control Flow

| Syntax | Meaning |
| -- | -- |
| `if cond { }` | Conditional execution |
| `if cond { } else { }` | Conditional with else branch |
| `match val { pat: expr, ... }` | Pattern matching (statement or expression) |
| `yield value` | Produce a value from the innermost block |
| `yield :label value` | Produce a value from the labeled block |
| `name: { ... }` | Labeled block (target for `yield :name`) |
| `for collection \|val\| { }` | Iterate over elements |
| `for collection \|val, idx\| { }` | Iterate with index |
| `for 0..n \|i\| { }` | Iterate over exclusive range |
| `for 0..=n \|i\| { }` | Iterate over inclusive range |
| `while cond { }` | While loop |
| `while cond : cont_expr { }` | While loop with continue expression |
| `break` | Exit innermost loop |
| `continue` | Skip to next iteration |

### Ranges

| Syntax | Meaning |
| -- | -- |
| `a..b` | Exclusive range (a to b-1) |
| `a..=b` | Inclusive range (a to b) |
| `0..(n + 1)` | Complex expressions must be parenthesized |

### Pointers

| Syntax | Meaning |
| -- | -- |
| `@T` | Single-item pointer (read-only, no arithmetic) |
| `@mut T` | Single-item pointer (mutable, no arithmetic) |
| `*T` | Many-item pointer (read-only, with arithmetic) |
| `*mut T` | Many-item pointer (mutable, with arithmetic) |
| `@@T` | Pointer to pointer (single-item) |
| `**T` | Pointer to pointer (many-item) |
| `&x` | Address of x |
| `p^` | Dereference p |
| `p^^` | Dereference twice |

### Pointer Mutability Combinations

Applies to both `@T` (single-item) and `*T` (many-item) pointers:

| Declaration | Reassign pointer? | Write through pointer? |
| -- | -- | -- |
| `ptr: @T` / `ptr: *T` | No | No |
| `ptr: @mut T` / `ptr: *mut T` | No | Yes |
| `mut ptr: @T` / `mut ptr: *T` | Yes | No |
| `mut ptr: @mut T` / `mut ptr: *mut T` | Yes | Yes |

### Arrays and Slices

| Syntax | Meaning |
| -- | -- |
| `[N]T` | Fixed-size array of N immutable elements |
| `[N]mut T` | Fixed-size array of N mutable elements |
| `[N:0]T` | Sentinel-terminated array, immutable elements |
| `[N:0]mut T` | Sentinel-terminated array, mutable elements |
| `[]T` | Slice of immutable elements |
| `[]mut T` | Slice of mutable elements |
| `[:0]T` | Sentinel-terminated slice, immutable elements |
| `[:0]mut T` | Sentinel-terminated slice, mutable elements |
| `[:S]T` | Slice terminated by sentinel value S |
| `"hello"` | String literal (type `[:0]u8`) |

### Multi-Line Strings

| Syntax | Meaning |
| -- | -- |
| `|content` | Line of multi-line string |
| `|` | Empty line (blank line in output) |

### Slice Mutability Combinations

| Declaration | Reassign? | Modify elements? |
| -- | -- | -- |
| `s: []T` | No | No |
| `s: []mut T` | No | Yes |
| `mut s: []T` | Yes | No |
| `mut s: []mut T` | Yes | Yes |
| `s: [:0]T` | No | No |
| `s: [:0]mut T` | No | Yes |
| `mut s: [:0]T` | Yes | No |
| `mut s: [:0]mut T` | Yes | Yes |

### Optional Types

| Syntax | Meaning |
| -- | -- |
| `?T` | Optional type (may be `none`) |
| `none` | Absent value |
| `x orelse default` | Unwrap with fallback value |
| `x?` | Force unwrap (traps if `none`) |
| `if x |v| { }` | Conditional unwrap |
| `if x |v : guard| { }` | Conditional unwrap with guard |
| `if x and y |a, b| { }` | Multi-unwrap (short-circuits) |
| `if (x and y) |a, b : guard| { }` | Multi-unwrap with guard |

### Function Calls

| Syntax | Meaning |
| -- | -- |
| `func_name(args)` | Runtime function call |
| `func_name(args)` | Comptime function call (same syntax as runtime calls) |

### Function Modifiers

| Syntax | Meaning |
| -- | -- |
| `name :: func(...) T { }` | Regular function |
| `name :: inline func(...) T { }` | Force-inlined function |
| `name :: noinline func(...) T { }` | Never-inlined function |
| `name :: comptime func(...) T { }` | Compile-time only function |
| `name :: c func(...) T` | C calling convention (external) |
| `name :: c func(...) T { }` | C calling convention (Honey impl) |

### Error Handling

| Syntax | Meaning |
| -- | -- |
| `T ! E` | Function returns `T` or fails with `E` |
| `T ! E1 | E2` | Function can fail with either error type |
| `error e` | Exit function via error channel |
| `error .variant{...}` | Construct and return error in one step |
| `try expr` | Unwrap success or propagate error |
| `expr catch fallback` | Provide fallback value on error |
| `expr catch |e| { ... }` | Handle error with block |
| `yield value` | Provide value from innermost block |
| `yield :label value` | Provide value from labeled block |

### Interfaces

| Syntax | Meaning |
| -- | -- |
| `interface { ... }` | Define an interface |
| `Name: type` | Associated type |
| `Name: Interface` | Associated namespace satisfying Interface |
| `impl InterfaceName` | Declare that this namespace satisfies the interface |
| `comptime I: Interface` | Generic parameter requiring a satisfying namespace |
| `I.TypeName` | Access associated type from implementation |
| `I.func_name(...)` | Call function from implementation |

### Tuples

| Syntax | Meaning |
| -- | -- |
| `struct {T1, T2}` | Tuple type (positional struct) |
| `Name :: struct {T1, T2}` | Named tuple type |
| `{val1, val2}` | Tuple literal |
| `t.0`, `t.1` | Access tuple fields by position |

### Variadic Arguments

| Syntax | Meaning |
| -- | -- |
| `func(args: ...) T` | Variadic parameter (must be last) |
| `func(...) T` | C variadic (unnamed, extern only) |

### C Interop

| Syntax | Meaning |
| -- | -- |
| `name :: c func(...)` | C function declaration (external) |
| `name :: c func(...) { }` | Honey function with C ABI |
| `Name :: c struct { }` | C-layout struct |
| `Name :: opaque` | Opaque C type (pointer-only) |
| `[:0]u8` | C string type (`const char*`) |
| `[:0]mut u8` | Mutable C string (`char*`) |

