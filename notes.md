# Notes

## Compiler overview

```
Source Code > Lexer ................. > Parser ............. > Semantic Analysis .......... > Code Gen ............... > Machine Code
              ├─ Character scanning     ├─ Syntax validation   ├─ Symbol table construction   ├─ Instruction selection
              └─ Token classification   └─ AST construction    ├─ Type resolution             ├─ Register allocation
                                                               ├─ Type inference              └─ Assembly emission
                                                               └─ Type checking
```

## Roadmap (v0.1.0)

## Todo

## Utilities

### Logger

- [ ] Configure the logger to only log debug messages when in debug mode
