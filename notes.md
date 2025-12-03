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

## Todo

- [ ] Too much cloning in the parser (use slices instead?)
    - also, reference counted string slices (`Rc<str>`) may be preferable in cases with many-to-one relationsips (maybe a solution to the related problem?)
- [ ] Warn about unused values (e.g., value of expression is not assigned to a variable or used as function argument)
- [ ] Figure test scopes (inspired by Zig)
    - will probably require thinking about the build system as well

## Roadmap (v0.0.3)

Code emission.

- [ ] Code emission diagnostics
- [ ] Runtime with entry point
    - [ ] setup proper runtime
    - [ ] jump to user entry point
    - [ ] link dynamically (or statically if possible) with emitted code when assembling

## Roadmap (v0.0.2)

Comprehensive implementation of basic runtime dependent operations in the Honey language.

- [ ] Functions (codegen)
    - [ ] stack frames
    - [ ] proper prologue/epilogue setup (depending on function properties, e.g. leaf-node, parameter count, etc.)
    - [ ] proper stack alignment
    - [ ] proper calling conventions adherence
- [ ] Function calls
- [ ] Comptime constant inlining in function bodies
- [ ] Arbitrary scope nesting (statically allocated memory dropped on scope exit)
- [ ] Control flow operations
    - [ ] defer statements (run on scope exit)
    - [ ] if statements
    - [ ] while loops
    - [ ] for loops
- [ ] Datatypes
    - [ ] structs
    - [ ] arrays
- [ ] Proper register allocation in codegen
- [ ] Collect parser errors instead of failing immediately

## Roadmap (v0.0.1)

Comprehensive implementation of basic compile-time functionality in the Honey language.

- [ ] Proper diagnostics
    - [ ] lexer
    - [ ] parser
    - [ ] semantic
- [ ] Parse basic operations (disregard modulo operation for now)
    - [ ] arithmetic
        - [ ] multiplicative
        - [ ] additive
    - [ ] boolean
- [ ] Parse function declarations
- [ ] Comptime expression evaluation
- [ ] Comptime constant folding
- [ ] Proper type checking

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
- package management system that simply pulls the lib into a "vendor" folder coexisting with the project files instead of an invisible cache approach
- go-like coroutines? 🤔

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
