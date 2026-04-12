const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const builtin = @import("builtin");
const honey = @import("honeylang");

pub fn main() !void {
    var gpa = heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();

    const alloc = gpa.allocator();

    // parse arguments
    var args = honey.Args.init();
    defer args.deinit(alloc);
    try args.parse(alloc);

    // validate
    if (args.source_files.items.len == 0) {
        std.debug.print("Honey requires at least one source file to compile\n", .{});
        std.process.exit(1);
    }

    var src = try honey.SourceManager.init.fromFile(alloc, args.source_files.items[0]);
    defer src.deinit(alloc);

    std.debug.print("\n[::Source Code::]\n\n", .{});
    std.debug.print("{s}\n", .{src.contents});

    var str_pool = honey.StringPool.init();
    defer str_pool.deinit(alloc);

    var lexer = honey.Lexer.init(.{ .src = &src, .str_pool = &str_pool });
    defer lexer.deinit(alloc);
    try lexer.scan(alloc);

    var parser = honey.Parser.init(.{ .src = &src, .tokens = lexer.tokens.slice() });
    defer parser.deinit(alloc);
    const ast = try parser.parse(alloc);

    const ast_render = try ast.render(alloc, src.contents, &str_pool);
    defer alloc.free(ast_render);

    std.debug.print("\n[::Rendered AST::]\n\n", .{});
    std.debug.print("{s}\n", .{ast_render});

    var hir = try honey.Parser.lower(alloc, &ast, &str_pool);
    defer hir.deinit(alloc);

    const hir_render = try hir.render(alloc);
    defer alloc.free(hir_render);

    std.debug.print("\n[::Rendered HIR::]\n\n", .{});
    std.debug.print("{s}\n", .{hir_render});

    var sema = honey.Sema.init(&hir, &str_pool);
    defer sema.deinit(alloc);
    try sema.analyze(alloc);

    const mir_render = try sema.mir.render(alloc, &str_pool);
    defer alloc.free(mir_render);

    std.debug.print("\n[::Rendered MIR (unoptimized)::]\n\n", .{});
    std.debug.print("{s}\n", .{mir_render});

    var opt = honey.Optimizer.init(.{ .mir = &sema.mir, .str_pool = &str_pool });
    defer opt.deinit(alloc);
    try opt.optimize(alloc);

    const mir_opt_render = try opt.mir_dce.render(alloc, &str_pool);
    defer alloc.free(mir_opt_render);

    std.debug.print("\n[::Rendered MIR (optimized)::]\n\n", .{});
    std.debug.print("{s}\n", .{mir_opt_render});
}
