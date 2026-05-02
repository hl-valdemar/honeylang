const std = @import("std");
const heap = std.heap;
const mem = std.mem;

const Diagnostic = @import("diagnostic/Store.zig");
const HIR = @import("parser/HIR.zig");
const Lexer = @import("lexer/Lexer.zig");
const Optimizer = @import("optimizer/Optimizer.zig");
const Parser = @import("parser/Parser.zig");
const Sema = @import("sema/Sema.zig");
const Source = @import("source/Source.zig");
const StringPool = @import("util/StringPool.zig");

pub const Context = struct {
    alloc: mem.Allocator,
    src: *const Source,
    str_pool: *StringPool,
    diagnostics: *Diagnostic,
    dump_pipeline: bool,
};

pub fn run(ctx: Context) !void {
    var render = Renderer.init(ctx.alloc, ctx.dump_pipeline);
    defer render.deinit();

    try render.printStatic("Source Code", ctx.src.contents);

    var hir_stage = try buildHir(ctx, &render);
    var sema_stage = try buildSema(ctx, &render, &hir_stage);
    var opt_stage = try buildOptimized(ctx, &render, &sema_stage);
    defer opt_stage.deinit();

    if (render.enabled) {
        const mir_opt_render = try opt_stage.opt.mir_optimized.render(render.alloc(), ctx.str_pool);
        render.printRendered("Rendered MIR (optimized)", mir_opt_render);
    }

    try render.printDiagnostics(ctx.diagnostics, ctx.src);
}

const Renderer = struct {
    arena: heap.ArenaAllocator,
    enabled: bool,

    fn init(parent_alloc: mem.Allocator, enabled: bool) Renderer {
        return .{
            .arena = heap.ArenaAllocator.init(parent_alloc),
            .enabled = enabled,
        };
    }

    fn deinit(self: *Renderer) void {
        self.arena.deinit();
    }

    fn alloc(self: *Renderer) mem.Allocator {
        return self.arena.allocator();
    }

    fn printStatic(self: *Renderer, comptime title: []const u8, contents: []const u8) !void {
        if (!self.enabled) return;
        printSection(title, contents);
        _ = self.arena.reset(.free_all);
    }

    fn printRendered(self: *Renderer, comptime title: []const u8, contents: []const u8) void {
        if (!self.enabled) return;
        printSection(title, contents);
        _ = self.arena.reset(.free_all);
    }

    fn printDiagnostics(self: *Renderer, diagnostics: *const Diagnostic, src: *const Source) !void {
        if (diagnostics.len() == 0) return;

        const diag_render = try diagnostics.render(self.alloc(), src);
        printSection("Diagnostics", diag_render);
    }
};

const HirStage = struct {
    arena: heap.ArenaAllocator,
    hir: HIR,

    fn deinit(self: *HirStage) void {
        const alloc = self.arena.allocator();
        self.hir.deinit(alloc);
        self.arena.deinit();
    }
};

const SemaStage = struct {
    arena: heap.ArenaAllocator,
    sema: Sema,

    fn deinit(self: *SemaStage) void {
        const alloc = self.arena.allocator();
        self.sema.deinit(alloc);
        self.arena.deinit();
    }
};

const OptimizerStage = struct {
    arena: heap.ArenaAllocator,
    opt: Optimizer,

    fn deinit(self: *OptimizerStage) void {
        const alloc = self.arena.allocator();
        self.opt.deinit(alloc);
        self.arena.deinit();
    }
};

fn buildHir(ctx: Context, render: *Renderer) !HirStage {
    var hir_arena = heap.ArenaAllocator.init(ctx.alloc);
    errdefer hir_arena.deinit();
    const hir_alloc = hir_arena.allocator();

    var parse_arena = heap.ArenaAllocator.init(ctx.alloc);
    defer parse_arena.deinit();
    const parse_alloc = parse_arena.allocator();

    var lexer = Lexer.init(.{
        .src = ctx.src,
        .str_pool = ctx.str_pool,
        .shared_alloc = ctx.alloc,
        .diagnostics = ctx.diagnostics,
    });
    defer lexer.deinit(parse_alloc);
    try lexer.scan(parse_alloc);

    var parser = Parser.init(.{
        .src = ctx.src,
        .tokens = lexer.tokens.slice(),
        .diagnostic_alloc = ctx.alloc,
        .diagnostics = ctx.diagnostics,
    });
    defer parser.deinit(parse_alloc);
    const ast = try parser.parse(parse_alloc);

    if (render.enabled) {
        const ast_render = try ast.render(render.alloc(), ctx.src.contents, ctx.str_pool);
        render.printRendered("Rendered AST", ast_render);
    }

    const hir = try Parser.lowerWithDiagnostics(hir_alloc, &ast, ctx.str_pool, ctx.diagnostics, ctx.alloc);
    return .{ .arena = hir_arena, .hir = hir };
}

fn buildSema(ctx: Context, render: *Renderer, hir_stage: *HirStage) !SemaStage {
    defer hir_stage.deinit();

    if (render.enabled) {
        const hir_render = try hir_stage.hir.render(render.alloc());
        render.printRendered("Rendered HIR", hir_render);
    }

    var sema_arena = heap.ArenaAllocator.init(ctx.alloc);
    errdefer sema_arena.deinit();
    const sema_alloc = sema_arena.allocator();

    var sema = Sema.init(&hir_stage.hir, ctx.str_pool, ctx.diagnostics, ctx.alloc);
    errdefer sema.deinit(sema_alloc);
    try sema.analyze(sema_alloc);

    return .{ .arena = sema_arena, .sema = sema };
}

fn buildOptimized(ctx: Context, render: *Renderer, sema_stage: *SemaStage) !OptimizerStage {
    defer sema_stage.deinit();

    if (render.enabled) {
        const mir_render = try sema_stage.sema.mir.render(render.alloc(), ctx.str_pool);
        render.printRendered("Rendered MIR (unoptimized)", mir_render);
    }

    var opt_arena = heap.ArenaAllocator.init(ctx.alloc);
    errdefer opt_arena.deinit();
    const opt_alloc = opt_arena.allocator();

    var opt = Optimizer.init(.{
        .mir = &sema_stage.sema.mir,
        .str_pool = ctx.str_pool,
        .shared_alloc = ctx.alloc,
        .diagnostics = ctx.diagnostics,
    });
    errdefer opt.deinit(opt_alloc);
    try opt.optimize(opt_alloc);

    return .{ .arena = opt_arena, .opt = opt };
}

fn printSection(comptime title: []const u8, contents: []const u8) void {
    std.debug.print("\n[::{s}::]\n\n", .{title});
    std.debug.print("{s}\n", .{contents});
}
