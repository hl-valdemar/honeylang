# arm64

## function calling convention

Registers:

- `x0-x7`: Argument/result registers (caller-saved)
- `x8`: Indirect result location (caller-saved)
- `x9-x15`: Temporary registers (caller-saved)
- `x16-x17`: Intra-procedure-call scratch registers (caller-saved)
- `x18`: Platform register (reserved on some platforms)
- `x19-x28`: Callee-saved registers
- `x29`: Frame pointer (FP) - callee-saved
- `x30`: Link register (LR) - stores return address
- `sp` (x31): Stack pointer - must be 16-byte aligned

## allocating stack memory

```s
.global _function
.align 2  ; 2^2 bytes = 4 bytes = 32 bit alignment
_function:
    sub sp, sp, #64     ; allocate 64 bytes on stack
    str x30, [sp, #56]  ; save link register

    # doing stuff...

    ldr x30, [sp, #56]  ; load link register
    add sp, sp, #64     ; deallocate stack space
    ret
```
