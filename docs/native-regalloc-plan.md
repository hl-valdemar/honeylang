# Native Codegen: Register Allocation Plan

Saved for later — to be implemented when we return to the native (debug) backend.

## Reference

Poletto & Sarkar, "Linear Scan Register Allocation" (1999)
~/Documents/Papers/linearscan.pdf

## Current Problems

1. **No real register allocation** — `RegMap` in `aarch64.zig:269-298` assigns x9-x15
   sequentially and wraps on overflow, clobbering previous values.
2. **No spilling** — when registers run out, `nextReg()` wraps to x9 silently.
3. **No liveness info** — VRegs allocated monotonically but never freed.
4. **Width hardcoded** — almost everything uses `.w32` despite `typeIdToWidth()` existing.

## Plan: Linear Scan Register Allocation

### Part 1: Compute Live Intervals

Each VReg needs `[start, end]` — first def to last use in MIR instruction order.
Single linear pass suffices since MIR has no loops/branches yet.

### Part 2: Linear Scan

Core algorithm (Figure 1 from paper):
- Maintain `active` list sorted by endpoint
- Scan intervals by start point
- `ExpireOldIntervals`: free registers whose intervals ended before current start
- `SpillAtInterval`: when all R registers busy, spill the interval ending furthest
- Free register pool: `{x9, x10, x11, x12, x13, x14, x15}` (7 registers)

### Part 3: Rewrite Lowering

Replace `RegMap` with lookup table from linear scan results:
- VReg assigned PReg → use directly
- VReg spilled → emit load before use, store after def

### Architecture Notes

- Arguments already spilled to stack at function entry — no conflict with allocator
- x9-x15 are caller-saved in AAPCS64, so spilling across calls is correct
- No callee-saved register management needed

### Implementation Steps

1. Add `LiveInterval` struct: `{ vreg: VReg, start: u32, end: u32 }`
2. Liveness analysis pass per `MIRFunction` after MIR generation
3. Linear scan allocator producing `VReg -> PReg | StackSlot`
4. Replace `RegMap` in lowering with allocation results
