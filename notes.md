# AArch64 Calling Convention (AAPCS64)

## Registers

| Register    | Alias        | Purpose                                      | Saved by |
| ----------- | ------------ | -------------------------------------------- | -------- |
| `x0`-`x7`   | \-           | Arguments & return values                    | Caller   |
| `x8`        | \-           | Indirect result location (for large returns) | Caller   |
| `x9`-`x15`  | \-           | Temporary (scratch) registers                | Caller   |
| `x16`-`x17` | `ip0`, `ip1` | Intra-procedure-call scratch (linker/PLT)    | Caller   |
| `x18`       | \-           | Platform register (reserved, don't use)      | \-       |
| `x19`-`x28` | \-           | Callee-saved registers                       | Callee   |
| `x29`       | `fp`         | Frame pointer                                | Callee   |
| `x30`       | `lr`         | Link register (return address)               | Callee   |
| `sp`        | \-           | Stack pointer                                | \-       |

## Floating-point/SIMD

| Register    | Purpose                           | Saved by |
| ----------- | --------------------------------- | -------- |
| `v0`-`v7`   | FP/SIMD arguments & returns       | Caller   |
| `v8`-`v15`  | Callee-saved (lower 64 bits only) | Callee   |
| `v16`-`v31` | Temporary                         | Caller   |

## Argument Passing

1. First 8 integer/pointer args -> `x0`-`x7`
2. First 8 FP/vector args → `v0`-`v7`
3. Additional args → pushed onto stack (8-byte aligned slots)
4. Arguments are **not** split between registers and stack

## Return Values

* Integer/pointer up to 128 bits → `x0` (and `x1` for 128-bit)
* FP/vector → `v0` (and `v1` for larger)
* Large structs → caller passes pointer in `x8`, callee writes there

## Stack requirements

* **16-byte aligned** at all times
* Grows downward
* `sp` must be aligned before any load/store

## Typical Function Prologue

```asm
sub  sp, sp, #32          ; allocate stack frame
stp  x29, x30, [sp, #16]  ; save frame pointer & return address
add  x29, sp, #16         ; set up frame pointer
```

## Typical Function Epilogue

```asm
ldp  x29, x30, [sp, #16]  ; restore fp & lr
add  sp, sp, #32          ; deallocate frame
ret                       ; return (jumps to lr)
```

## Key Differences from x86-64

* More argument registers (8 vs 6 for integers)
* Explicit link register (`lr`) instead of stack-based return address
* `stp`/`ldp` for paired load/stores (common idiom)
* `x18` is reserved on many platforms (macOS, Windows)
