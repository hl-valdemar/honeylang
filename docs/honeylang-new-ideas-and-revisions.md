# More ideas on the language

## Equivalent of Zig's `comptime_int` and `comptime_float`

Zig has a primitive type called `comptime_int` which is basically any int that can resolve at comptime.

It would be nice to have such a type in Honey as well, but maybe we can just call it `int` (instead of the rather cumbersome `comptime_int`)
Likewise, a `float` type equivalent to Zig's `comptime_float` would also be nice.

## Return type of `main`

The return type of the `main` function should be restricted to a `u8`, an `int` (that resolves to `u8`), and `void` (which implicitly returns `0`).

## Type declaration of constants and variables

What about:

```hon
# constants (`::`)
x :: 32
x int :: 32
x float :: 3.14

# variables (`=`)
a = 0xf0f1
a int = 0xf0f1

# arrays
arr [3]u8 :: [1, 2, 3]      # const binding, const elements
arr [3]mut u8 :: [1, 2, 3]  # const binding, mutable elements
arr [3]u8 = [1, 2, 3]       # mutable binding, const elements
arr [3]mut u8 = [1, 2, 3]   # mutable binding, mutable elements

arr [_]u8 :: [1, 2, 3]  # size inferred from literal

# single-item pointers (only points to *one* element, no arithmetic)
ptr @T :: &t      # const binding, const pointer (i.e., no write through pointer)
ptr @mut T :: &t  # const binding, mutable pointer
ptr @T = &t       # mutable binding, const pointer
ptr @mut T = &t   # mutable binding, mutable pointer

# c-like many-item pointers (arithmetic supported - primarily used for interop)
ptr *T :: &t      # const binding, const pointer (i.e., no write through pointer)
ptr *mut T :: &t  # const binding, mutable pointer
ptr *T = &t       # mutable binding, const pointer
ptr *mut T = &t   # mutable binding, mutable pointer

# slices (includes both a len field for native processing, and guarantees null termination for c interop)
s []u8 :: arr[..]    # slice including all elements
s []u8 :: arr[..2]   # slice including all elements up to index 2
s []u8 :: arr[2..]   # slice including all elements from index 2
s []u8 :: arr[2..4]  # slice including all elements between index 2 (incl) and 4 (excl)

# arrays with explicit sentinels (must be of same type as array)
s [_..0]u8 :: arr[1, 2, 3]    # null terminator (not default for arrays)
s [3..'A']u8 :: arr[1, 2, 3]  # use 'A' as sentinel

# slices with explicit sentinel (must be of same type as slice)
s [..0]u8 :: arr[..]    # explicit null terminator (default so not necessary)
s [..'A']u8 :: arr[..]  # use 'A' as sentinel

# optionals
x ?i32 = none      # optional with no value
y ?u8 :: 255       # optional with a value
z ?@Buffer = none  # optional pointer (nullable pointer)

# unwrapping optionals
y ?u8 :: 255
val :: y?  # val is u8, equals 255

x ?i32 = none
bad :: x?  # runtime trap - x is none

# unwrapping with fallback value
x ?i32 = none
y :: x orelse 0  # y is i32, equals 0
name ?[]u8 :: get_name()
display :: name orelse "anonymous"

# functions
sum :: func(a, b int) int { ... }

# to comptime or not to comptime
$x :: 32                                           # comptime evaluable constant
$sum :: func(a, b int) int { ... }                 # comptime evaluable function
max :: func($T type, a, b T) T { ... }             # comptime polymorphic type params
make_array :: func ($size usize) [size]u8 { ... }  # comptime value params

# c functions (for c interop)
sum :: c func(a, b int) int { ... } # with implementation defined in honey
sum :: c func(a, b int) int         # with implementation somewhere else (essentially a honey binding)

# functions variadic arguments
printf :: c func(fmt []u8, args ...) int  # `...` is essentially an anonomous tuple type used in function arguments. `...` collects remaining arguments in a type inferred tuple (infer from anchor)

# NB: variables are runtime dependent and as such not comptime compatible

# structs carry only data, no methods, no behavior
Stuff :: struct {
    first u32
    second float
    third [5]i32
}

# tuples are just structs without field names
SomeTupleType :: struct { i32, float, []u8 }
some_tuple :: SomeTupleType{ 0, 1.3, "hello" }  # type-named tuple

# anonomous tuple type inferred from literal and type anchors
some_other_tuple :: { 4, "hello" }

# enums
Animal :: enum {
    dog
    cat
    bird
    lizard
}

# enums with backing type
Int :: enum(u8) {
    one
    two
    three
    four
    five
}

# unions
SomeStuff :: union {
    float
    int
    Animal
}

# tagged unions (constrained to the backing enum)
AnimalNameOrHeight :: union(Animal) {  # use just `enum` instead of `Animal` for unconstrained tagged union
    dog []u8
    cat []u8
    bird int
    lizard int
}

# namespaces carry behavior (and also holds data) - can't be instantiated, no runtime concept of a namespace
person {
    # struct scoped in person namespace
    Data :: struct {
        name []u8  # `[]u8` is basically a string
        age u32
        position Vec2
    }

    # const scoped to person
    whos_in_charge :: Data{
        name = "Jane",
        age = 32,
        position = Vec2{ x = 0, y = 0},
    }

    # mutable variable scoped to person (sum of distances walked by all people)
    dist_sum float = 0

    walk :: func(p @mut Data, distance float) boolean {
        p.position.x += distance
        dist_sum += distance
    }
}

# instantiating a struct from a namespace
p :: person.Data{
    name = "John",
    age = 34,
    position = Vec2{ x = 24, y = 31 },
}

# using a function from a namespace
person.walk(&p, 2.7)

# IMPORTS

# importing honey files
import "path/to/file.hon"  # wrapped in namespace implicitly named after file (i.e., "file" in this case)

# importing honey files with explicit name
another_name :: import "path/to/file.hon"

# importing c header files
import c "path/to/header.c"  # generates honey bindings and wraps in namespace implicitly named after file (i.e., "header" in this case)

# importing c header files with explicit name
some_name :: import c "path/to/header.c"

# importing multiple c header files into explicit namespace
some_name :: import c {
    # include headers (flattened into namespace)
    include "path/to/header1.h"
    include "path/to/header2.h"

    # define macros
    define "PI 3.14"  # set's a constant on the namespace (access with `some_name.PI`)
    define "DEBUG"    # flag could toggle debug declarations in header files
}

# CONTROL FLOW

# if statements
if stmt {
    # do something
} else if other_stmt {
    # do something else
} else {
    # do something else entirely
}

# simple for/while loop
i = 0
for i < 100 {
    # do something
    i += 1
}

# enhanced for loops (with element capture)
elements [_]u8 :: [1, 2, 3, 4]
for elements |e| {  # uses copy semantics (by-value, not by-reference)
    printf("%s\n", e)
}

# enhanced for loops with reference capture
elements [_]u8 :: [1, 2, 3, 4]
for elements |&e| { # uses reference semantics (single-item pointer, i.e. `@`)
    e^ += 1
}

# match statements

# matching on enums
match status {
    .ok: print("success")
    .error: print("failure")
    .pending: {
        log("still waiting")
        retry()
    }
}

# matching on integers and other values
match code {
    0: print("zero")
    1: print("one")
    2: print("two")
    else: print("other")  # needed - missing variants
}

Status :: enum { ok, error, pending }

match status {
    .ok: handle_ok()
    .error: handle_error()
    .pending: handle_pending()
    # no else needed - all variants covered
}

# matching on tagged unions
Result :: union(enum) {
    success Data
    failure struct {
        msg []u8
        code i32
    }
    pending void
}

match result {
    .success |data|: {  # use `|name|` to capture the variant's payload
        process(data)
    }
    .failure |info|: {
        print("error {d}: {s}", { info.code, info.msg })
    }
    .pending: {
        # void payload - no capture needed
        wait()
    }
}

# when a union variant has a `void` payload, omit the capture
Event :: union(enum) {
    click struct { x i32, y i32 }
    keypress KeyCode
    quit void
}

match event {
    .click |pos|: handle_click(pos.x, pos.y)
    .keypress |key|: handle_key(key)
    .quit: should_exit = true
}

# single-expression arms yield value implicitly
label :: match priority {
    .critical: "CRIT"
    .high:     "HIGH"
    .normal:   "NORM"
    .low:      " LOW"
}

# multi-statement arms use `yield`
message :: match code {
    0: "success"
    1: {
        log("warning encountered")
        yield "warning"
    }
    else: "unknown"
}

# conditional optional unwrapping
name ?[]u8 :: get_name()
if name |n| {
    # n is []u8 here, block only runs if name != none
    print(n)
}

if config |c| {
    use(c)
} else {
    use_defaults()
}

# with guard clause after `:` (evaluated only if the optional contains a value)
age ?u8 :: get_age()
if age |a : a >= 18| {
    print("adult")
}

# multi-unwrap
name ?[]u8 :: get_name()
age ?u8 :: get_age()
if name and age |n, a| {
    # both n and a are guaranteed non-none
    print("{s} is {d} years old", { n, a })
}

if name and hat |n, h : n == "Huginn" and h.brand == .gucci| {
    print("{s}'s got that drip\n", { n })
}

if get_name() and get_hat() |n, h| {
    # get_hat() is only called if get_name() returned non-none
}
```

## YOLO publicity rules (who cares)

Everything is public. (This should be easy to change later if need be.)
