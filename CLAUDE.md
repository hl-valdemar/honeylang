# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Interaction Preferences

Do not ask interactive questions (AskUserQuestion, ExitPlanMode) excessively. When the user gives a clear directive, proceed with implementation. Minimize back-and-forth; deliver results first.

When the user asks about a design idea, engage directly with their specific proposal first before listing approaches from other languages. Stay focused on the user's context.

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

When writing integration tests for the compiler, ensure test functions are called from main() so they aren't eliminated by semantic analysis. Always verify tests actually exercise the target feature.

## Compiler Architecture

When implementing compiler features, always propagate resolved types from semantic analysis rather than duplicating type inference in codegen. Never default to width 32 or map f32 to i32.

### Code Style Conventions

Prefer SoA (Struct of Arrays) layout for performance-critical data structures. Use dedicated emitter methods over generic appendFmt.

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

Currently targets a generic LLVM backend for easy and performant cross-compilation.

## Language Syntax Highlights

- Constants: `name :: value` (compile-time evaluated)
- Variables: `name := value` (runtime evaluated)
- Mutable variables: `mut name := value`
- Functions: `name :: func(params) ReturnType { body }`
- Foreign functions: `name :: c func(...)` for C ABI interop
