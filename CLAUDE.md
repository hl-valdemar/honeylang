# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Honey is a statically-typed systems programming language compiler written in Zig. It follows an "always compile" philosophy - the compiler always produces an executable, even when errors are present, by inserting runtime traps at error sites.

## Build Commands

```bash
zig build                    # Build the compiler
zig build run -- <file.hon>  # Compile and run a Honey source file
zig build test               # Run all tests
```

Requires Zig 0.15.2+.

## Testing

Test files are in `/test-files/`:
- `test-files/correct/` - valid Honey programs
- `test-files/incorrect/` - programs with intentional errors

Compile a specific test case: `zig build run -- test-files/correct/func_decls.hon`

## Compiler Architecture

The compiler is a classic multi-pass pipeline:

```
Source (.hon) → Lexer → Parser → Semantic Analysis → Comptime Evaluation → Code Generation → Linker → Executable
```

### Source Layout

- `src/source/` - Source file loading and indexing
- `src/lexer/` - Tokenization
- `src/parser/` - AST construction (recursive descent)
- `src/semantic/` - Type checking, symbol resolution, scope management
- `src/comptime/` - Compile-time expression evaluation (constants use `::`, runtime uses `:=`)
- `src/codegen/` - MIR generation and AArch64 code emission
- `src/runtime/` - Assembly startup code

### Key Design Patterns

- **Error accumulation**: Each phase collects errors and continues processing
- **Arena allocators**: Memory cleanup per compilation phase
- **Debug printers**: Every module has a corresponding `*-printer.zig` for visualization
- **Calling conventions**: Supports honey (default), c, cobol, fortran ABIs

### Main Entry Point

`src/main.zig` provides two compilation modes:
- `compileDebug`: Full diagnostic output for each phase
- `compileRelease`: Minimal output, errors only

### Target Support

Currently targets AArch64 architecture on darwin (macOS) and linux.

## Language Syntax Highlights

- Constants: `name :: value` (compile-time evaluated)
- Variables: `name := value` (runtime evaluated)
- Mutable variables: `mut name := value`
- Functions: `name :: func(params) ReturnType { body }`
- Foreign functions: `name :: c func(...)` for C ABI interop
