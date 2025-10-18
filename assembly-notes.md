# arm64

## allocating stack memory

```s
.global _main
.align 2             // 2^2 byte alignment
_function:
    sub sp, sp, #64  // allocate 64 bytes on stack
    // doing stuff...
    add sp, sp, #64  // deallocate stack space
    ret
```
