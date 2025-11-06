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

## Roadmap (v0.1.0)

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
- [ ] Control flow operations
    - [ ] if statements
    - [ ] while loops
    - [ ] for loops
- [ ] Proper register allocation in codegen

## Roadmap (v0.2.0)

- [ ] Datatypes
    - [ ] structs
    - [ ] arrays

## Todo

## Utilities

### Logger

- [x] Configure the logger to only log debug messages when in debug mode
- [ ] Buffer logs when writing to file (I/O bound operation, write in bulk)

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
