# An overview of the pipeline

```
Lexer → Parser → Semantic Analysis → Code Gen
                                     ├─ Stack frame tracking
                                     ├─ Local variable allocation
                                     ├─ Parameter saving to stack
                                     └─ Type-aware loads/stores
```

# Stack frames

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
│ (64 bytes)      │
├─────────────────┤ ← sp (SP points here)
Low Memory
```

