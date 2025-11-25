# Honey Language Documentation

## Comments

Comments are strings of characters prefixed with the `#` token. These are, of course, not evaluated or compiled into the binary, but are simply ignored by the compiler.

```honey
# Comments are prefixed with a pound, so this is a comment!
```

## Logical operators and comparators

Logical operators in the Honey language include `not`, `and`, and `or`, while `<`, `>`, `<=`, `>=`, `==`, and `!=` comprise the comparison operators. All pretty standard.

## Type Inference

Types are inferred from the context in which they're first used.
Once a type is determined for any given name, it will be locked in place and must be cast to the appropriate type explicitly if used in operations with different types.

```honey
# type is inferred from the immediate value, when not used in any context
# (though this results in a warning from the compiler about unused declarations)
DEBUG :: true
BAD_INDEX :: -1

# type is explicitly declared
PI: f64 :: 3.14

# type is inferred from the context in which it's first used
ANSWER :: 42  # type is pending
PI_SQUARED :: PI * PI  # PI has type f64, so PI_SQUARED resolves to f64
PI_CUBED :: PI * PI * PI  # same for PI_CUBED

# ANSWER is finally used, so it's type can be resolved.
# the type of the context is f64 due to PI, so ANSWER's type resolves to f64
SHOULD_WORK :: ANSWER * PI

# also, floats can be declared without decimals (either explicitly or inferred from the context)
X: f32 :: 10
```

## Functions

```honey
add :: func(a: i32, b: i32) i32 {
    return a + b
}

compute_result :: func(a: u32, b: u32) i32 {
    return add(a as i32, b as i32)
}

main :: func() void {
    compute(1, 2)
}
```

## Imports

```honey
import "std/mem"
import "std/mem/heap"

main :: func() void {
    buffer := heap.alloc(u8, 10)
    defer heap.free(buffer)

    # do stuff with the buffer...
    result := mem.eql(u8, buffer, "hello?")

    return result
}
```

Or, alternatively:

```honey
import "std/mem"

heap :: mem.heap

main :: func() void {
    buffer := heap.alloc(u8, 10)
    defer heap.free(buffer)

    # do stuff with the buffer...
    result := mem.eql(u8, buffer, "hello?")

    return result
}
```

Or even just:

```honey
import "std/mem"

some_func :: func() bool {
    buffer := mem.heap.alloc(u8, 10)
    defer mem.heap.free(buffer)

    # do stuff with the buffer...
    result := mem.eql(u8, buffer, "hello?")

    return result
}

main :: func() void {
    _ := some_func()  # assign to `_` to throw away expression value
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
    person_ptr := heap.create(PersonInfo)  # returns a pointer to the allocated memory
    defer heap.destroy(person_ptr)

    # or statically
    person_static := PersonInfo{
        .name = "Carol",
        .age = 42,
    }

    # do stuff with this person info
}
```

## Type aliasing

```honey
# type is `type`
EntityId :: u32
EntityId: type :: u32
```
