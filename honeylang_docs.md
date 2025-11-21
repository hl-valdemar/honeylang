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

main :: func() void {
    add(1, 2)
}
```
