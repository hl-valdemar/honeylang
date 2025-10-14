# Notes

## Todo

- [ ] Implement binary operations (for arithmetic and comparisons)
- [ ] Function calls
- [ ] Local variables (requires stack frames)
- [ ] Conditional control flow (e.g. if statements)

## Things I like in Programming

- Semicolons probably not needed (just look at odin)
- Strong typing!
- Type reflection!
- C interop!
- Generics defined as functions that return a result type given backing types
- Errors as values (error unions)
- Tagged unions
- Closures! (functions should be first-class citizens)
- `defer`s! (defer expression to end of scope)
- const pointers (can't be used to modify value it points to)
- Test scopes! (like in zig)
- No macros, just let comptime work its magic
- No implicit casting! Only explicitly!
- Interfaces or traits
- No classes! No inheritance! Only composition (and traits)
- Good unicode string literal declaration support (e.g. `u"Hi :)"` for utf-8, `uu"Goodbye :)` for utf-16, `uuu"What?"` for utf-32) (might be necessary in some cases (e.g. UEFI applcations)
- String comparison inspired by Zig's simple memory oriented check (very clear what's going on)
- Scoping: files are essentially scopes —> define fields directly on the outermost scope in of the file (akin to zig)
- Need a way to specify compile time code (or dependencies that need to be compile time known) (zig inspired)
