# Notes

## Todo

- [x] Defer statements (affect code generation significantly - need to track scope exits)
- [x] Binary operations (for arithmetic and comparisons)
- [x] Function calls
- [x] Handle proper function calling convention (caller/callee saved registers, etc)
- [x] Local variables (requires stack frames)
- [x] Setup a proper test framework
- [x] Implement fuzz testing
- [ ] Comptime evaluation (start with simple comptime computable expressions; add comptime function calls later)
- [ ] Check all tests to make sure they actually test properly!
- [ ] Type inference on variable declaration (optional type declaration, allows for the `a := 10` syntax as opposed to `a: i32 = 10`)
        - probably requires a peer resolution system to avoid unnecessary manual type casting (i.e., what context is the variable used in?)
- [ ] Actually enforce typing! (semantic analysis)
- [ ] Figure out how imports should work
        - more like zig (verbose), rust (less verbose), or odin (minimally verbose, but also a bit opaque)?
- [ ] Conditional control flow (e.g. if statements)

## Things I like in Programming

- The compiler should allow for people to learn by observing warnings etc. but have the freedom to test what happens if they don't listen, like in this case:
  ```
  # compiler warning: "returning slice to stack memory"
  dangerous :: func() []u8 {
    buffer: [10]u8 = undefined
    return buffer[0..]  # ⚠️ warning (not error)
  }
  ```
  This should be a good learning experience

- Semicolons not needed (just look at odin!)

- No variable shadowing!

- Strong typing!

- Type reflection!

- C interop!

- Generics defined as functions that return a result type given backing types

- Errors as values (error unions)

- Tagged unions

- Closures! (functions should be first-class citizens)

- `defer`s! (defer expression to end of scope)

- const references (can't be used to modify value it points to)

- Test scopes! (like in zig)

- No macros, just let comptime work its magic

- No implicit casting! Only explicitly!

- Runtime bounds checking!

- Null terminated data where possible! (ALWAYS)

- Like in Zig, i want clear ownership by convention (clear ownership patterns)

- Interfaces or traits

- No classes! No inheritance! Only composition (and traits)

- Build system using just the language itself, like zig (and odin?)

- Good unicode string literal declaration support (e.g. `u8"Hi :)" == "Hi :)"` for utf-8, `u16"Goodbye :)` for utf-16, `u32"What?"` for utf-32) (might be necessary in some cases (e.g. UEFI applcations)

- String comparison inspired by Zig's simple memory oriented check (very clear what's going on)

- Scoping: files are essentially scopes —> define fields directly on the outermost scope in of the file (akin to zig)

- Need a way to specify compile time code (or dependencies that need to be compile time known) (zig inspired)

- Unused declarations should generate warnings (descend computation graph to check) and should not be part of the generated code

- Should be able to write bare-metal executables

- Syntax highlighting for multiline strings with codeblocks

## Memory Allocation Design

Philosophy:

Memory allocation in Honey is designed to be explicit but not verbose.
We reject the dogma that global state is inherently evil - allocators are
a cross-cutting concern that nearly every function needs, making them a 
perfect candidate for sensible defaults.

Core principles:

1. no hidden magic: allocation calls are clear at the call site
2. sensible defaults: thread-local global allocator for common cases (access using `mem.heap.alloc`, or maybe just `mem.alloc`)
3. explicit override: custom allocators when you need them
4. build-mode aware: different default allocators for debug/release builds
5. no scope-based mutations: avoid "action at a distance" problems (hard to reason about, not directly obvious what's happening)

Basic usage:

```honey
heap :: import("std/mem").heap

process_data :: func(data: []u8) []u8 {
  # uses heap.alloc() - clear thread-local global allocator
  temp := heap.alloc([]u8, len(data))
  defer heap.free(temp)
  return temp
}
```

Default allocator:

The `heap` allocator is a thread-local global that provides different implementations based on build mode:

- Debug: tracking allocator with leak detection and callbacks
- Release: fast allocator with zero overhead
- ReleaseSafe: safe allocator with bounds checking
- Test: deterministic allocator for reproducible tests (may be hard to pull off)

At compile time, we inject different allocators based on build mode.
`heap.alloc()` will then just use the appropriate allocator by default.

```honey
# Debug build: heap = tracking_allocator (with leak detection)
# Release build: heap = fast_allocator (no overhead)
# ReleaseSafe build: heap = safe_allocator (bounds checking)

process_data :: func(data: []u8) []u8 {
  temp := heap.alloc([]u8, len(data))
  defer heap.free(temp)
  return temp
}
```

User-instantiated allocators:

For when you have specific needs:
```honey
import "std/mem"

GameAllocator :: struct {
  frame_arena: mem.Arena,
  permanent_heap: mem.Heap,
  entity_pool: mem.Pool,
}

init_game :: func() GameAllocator {
  return GameAllocator{
    # cleared every frame
    frame_arena = mem.arena_create(mem.megabyte(1)),

    # lives for entire game session
    permanent_heap = mem.heap_create(mem.megabyte(100)),

    # fixed-size pool for entities
    entity_pool = mem.pool_create(sizeof(Entity), 1000),
  }
}

update_game :: func(game: @GameAllocator) void {
  # temporary per-frame allocations
  temp_buffer := game.frame_arena.alloc([]u8, 1024)

  # long-lived game data
  new_level := game.permanent_heap.alloc(Level) # QUESTION: maybe let users omit number of instances to allocate for like this so default is 1 (but maybe hard to pull off - figure out how to do defuault argument values generally)?

  # pool allocation (fast, no fragmentation)
  entity := game.entity_pool.alloc(Entity)
  defer game.entity_pool.free(entity)

  # clear frame arena at end of frame
  game.frame_arena.clear()
}
```

Allocator configuration options:

```honey
import "std/mem"

setup_allocators :: func() void {
  # configure heap with custom settings
  heap_config := mem.HeapConfig{
    initial_size = mem.megabyte(50),
    max_size = mem.megabyte(500),
    alignment = 16,
    track_allocations = DEBUG, # only in debug mode
  }

  # QUESTION: (thread-local) global for heap allocators? if so, probably best to only allow in build script (trouble if changing config when allocations exist)
  mem.heap.configure(heap_config)

  # or create a custom heap instance (requires explicit dependency injection where needed though... breaks with philosophy a bit)
  my_heap := mem.heap_create_with(heap_config)
  data := my_heap.alloc([]u8, 1000)
  defer my_heap.free(data)

  # or set default heap allocator (alternative maybe)?
  my_heap := mem.heap_create_with(heap_config)
  mem.heap_set(my_heap)

  data := mem.heap.alloc([]u8, 1000)
  defer mem.heap.free(data)
}
```

Arena allocator examples:

```honey
# simple arena for temporary work
process_file :: func(path: []const u8) Data! {
  # create arena with 1mb buffer
  arena := mem.arena_create(mem.megabyte(1))
  defer mem.arena_destroy(arena)

  # all temp allocations from arena
  file_contents := arena.alloc([]u8, file_size)
  parsed_data := arena.alloc(ParsedData)
  intermediate := arena.alloc([]Token, 1000)

  # convert to permanent allocation before returning
  result := heap.alloc(Data)
  mem.copy(result, parsed_data)

  return result
  # arena cleaned up automatically
}
```

Pool allocator for fixed-size objects:

```honey
# great for game entities, nodes in data structures, etc.
EntitySystem :: struct {
  pool: mem.Pool,
}

init_entities :: func() EntitySystem {
  return EntitySystem{
    pool = mem.pool_create(sizeof(Entity), 10_000),
  }
}

spawn_entity :: func(sys: @EntitySystem) @Entity {
  entity := sys.pool.alloc(Entity)
  # fast o(1) allocation, no fragmentation
  return entity
}

despawn_entity :: func(sys: @EntitySystem, entity: @Entity) void {
  sys.pool.free(entity)
  #  return to pool for reuse
}
```

What's nice about this: user code stays clean and simple:

```honey
# this code is the same regardless of build mode
my_function :: func() @[]u8 {
    data := heap.alloc([]u8, 100)
    return data
}
```

This all translates to **no magic**:

- clear which allocator is used
- no hidden context parameters
- no scope-based overrides
- easy to trace and reason about

Gives:

- convenience of global allocator for 95% of code
- power to use custom allocators when needed
- safety through debug builds with leak detection
- simplicity: no magic, just clear function calls
- grep-able: search for `heap.alloc` to find all heap allocations


Compared to:

- c - more explicit, safer defaults, no hidden malloc behavior
- c++ - no template complexity, clear allocator usage at call site
- rust - less verbose, no lifetime annotations required for allocators
- zig - more convenient defaults, but equally explicit when needed
- odin - no hidden context parameter, fully transparent behavior
- go - explicit control, no hidden garbage collection
