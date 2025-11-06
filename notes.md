# Notes

## Compiler overview

```
Source Code > Lexer: Token Stream ... > Parser: AST ........ > Semantic Analysis: Typed AST > Code Gen: Machine Code
              ├─ Character scanning     ├─ Syntax validation   ├─ Symbol table construction   ├─ Instruction selection
              └─ Token classification   └─ AST construction    ├─ Type resolution             ├─ Register allocation
                                                               ├─ Type inference              └─ Assembly emission
                                                               ├─ Type checking
                                                               ├─ Comptime evaluation
                                                               └─ Comptime constant inlining
```

## Roadmap (v0.0.1)

Comprehensive implementation of basic compile-time functionality in the Honey language.

- [x] Strong typing (proper type checking)
- [ ] Proper diagnostics
    - [x] lexer
    - [ ] parser
    - [ ] semantic
    - [ ] code emission
- [ ] Arithmetic and boolean operations (disregard modulo operation for now)
    - [ ] arithmetic
        - [x] multiplicative
        - [ ] additive
    - [ ] boolean
- [x] Comptime expression evaluation
- [ ] Comptime constant inlining

## Roadmap (v0.0.2)

Comprehensive implementation of basic runtime dependent operations in the Honey language.

- [ ] Function declarations
    - [ ] stack frames
    - [ ] proper prologue/epilogue setup (depending on function properties, e.g. leaf-node, parameter count, etc.)
    - [ ] proper stack alignment
    - [ ] proper calling conventions adherence
- [ ] Function calls
- [ ] Control flow operations
    - [ ] defer statements (run on scope exit)
    - [ ] if statements
    - [ ] while loops
    - [ ] for loops
- [ ] Datatypes
    - [ ] structs
    - [ ] arrays
- [ ] Proper register allocation in codegen

## Ideology

- systems programming language

- explicit operations over implicit operations
    - especially when working with memory

- simplicity over complexity

- composition over inheritance

- minimize (visual) clutter, increase ergonomics
    - semicolons are unnecessary

- types must be explicitly cast when required
    - no implicit type casting

## Notable features

- functions as first-class citizens
- defer statements

## Stack frames

```
High Memory
┌─────────────────┐
│ Caller's Frame  │
├─────────────────┤
│ Return Address  │ ← Saved by 'bl'
├─────────────────┤
│ Old FP          │
├─────────────────┤
│ Old LR          │
├─────────────────┤ ← x29 (FP points here)
│ Parameter 0     │ ← [x29, #-8]
├─────────────────┤
│ Parameter 1     │ ← [x29, #-16]
├─────────────────┤
│ Local var 0     │ ← [x29, #-24]
├─────────────────┤
│ Local var 1     │ ← [x29, #-32]
├─────────────────┤
│ Temp space      │
├─────────────────┤ ← sp (SP points here)
Low Memory
```
