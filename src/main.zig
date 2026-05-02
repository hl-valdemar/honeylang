const std = @import("std");
const heap = std.heap;
const honey = @import("honeylang");

pub fn main(init: std.process.Init) !void {
    var gpa = heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    // parse arguments
    const argv = try init.minimal.args.toSlice(alloc);
    defer alloc.free(argv);

    var args = honey.Args.init();
    defer args.deinit(alloc);
    try args.parse(alloc, argv);

    // validate
    if (args.source_files.items.len == 0) {
        std.debug.print("Honey requires at least one source file to compile\n", .{});
        std.process.exit(1);
    }

    var src = try honey.SourceManager.init.fromFile(alloc, init.io, args.source_files.items[0]);
    defer src.deinit(alloc);

    var str_pool = honey.StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = honey.Diagnostic.init();
    defer diagnostics.deinit(alloc);

    try honey.compiler.run(.{
        .alloc = alloc,
        .io = init.io,
        .src = &src,
        .str_pool = &str_pool,
        .diagnostics = &diagnostics,
        .dump_pipeline = args.dump_pipeline,
    });

    if (diagnostics.hasProblems()) std.process.exit(1);
}
