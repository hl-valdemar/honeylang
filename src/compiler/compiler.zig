const std = @import("std");
const heap = std.heap;
const mem = std.mem;

const Diagnostic = @import("../diagnostic/Store.zig");
const HIR = @import("../parser/HIR.zig");
const ImportLoader = @import("ImportLoader.zig");
const Optimizer = @import("../optimizer/Optimizer.zig");
const Sema = @import("../sema/Sema.zig");
const Source = @import("../source/Source.zig");
const SourceManager = @import("../source/Manager.zig");
const StringPool = @import("../util/StringPool.zig");

pub const Context = struct {
    alloc: mem.Allocator,
    io: std.Io,
    src: *const Source,
    str_pool: *StringPool,
    diagnostics: *Diagnostic,
    dump_pipeline: bool,
    print_diagnostics: bool = true,
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

    if (ctx.print_diagnostics)
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

    var loader = ImportLoader.init(ctx);
    defer loader.deinit();

    var ast_render: ?[]const u8 = null;
    const ast_dump: ?ImportLoader.AstDump = if (render.enabled) .{
        .alloc = render.alloc(),
        .rendered = &ast_render,
    } else null;

    var hir = try loader.parseAndLower(hir_alloc, ctx.src, ast_dump);
    try loader.expandImports(hir_alloc, &hir, ctx.src.path);

    if (ast_render) |rendered|
        render.printRendered("Rendered AST", rendered);

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

fn runFileForTest(alloc: mem.Allocator, path: []const u8) !Diagnostic {
    var src = try SourceManager.init.fromFile(alloc, std.testing.io, path);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = Diagnostic.init();
    errdefer diagnostics.deinit(alloc);

    try run(.{
        .alloc = alloc,
        .io = std.testing.io,
        .src = &src,
        .str_pool = &str_pool,
        .diagnostics = &diagnostics,
        .dump_pipeline = false,
        .print_diagnostics = false,
    });

    return diagnostics;
}

fn expectCompilerDiagnostic(path: []const u8, tag: Diagnostic.Tag) !void {
    var diagnostics = try runFileForTest(std.testing.allocator, path);
    defer diagnostics.deinit(std.testing.allocator);

    for (0..diagnostics.len()) |idx| {
        if (diagnostics.get(idx).tag == tag) return;
    }

    return error.ExpectedDiagnosticNotFound;
}

test "compiler resolves imported constant" {
    const alloc = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "math.hon", .data = "value :: 1\n" });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "main.hon", .data = "import \"math.hon\"\nx :: math.value + 1\n" });

    const main_path = try std.fmt.allocPrint(alloc, ".zig-cache/tmp/{s}/main.hon", .{tmp.sub_path});
    defer alloc.free(main_path);

    var diagnostics = try runFileForTest(alloc, main_path);
    defer diagnostics.deinit(alloc);
    try std.testing.expect(!diagnostics.hasErrors());
}

test "compiler reports missing import" {
    const alloc = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "main.hon", .data = "import \"missing.hon\"\n" });
    const main_path = try std.fmt.allocPrint(alloc, ".zig-cache/tmp/{s}/main.hon", .{tmp.sub_path});
    defer alloc.free(main_path);

    try expectCompilerDiagnostic(main_path, .sema_import_file_not_found);
}

test "compiler reports import cycle" {
    const alloc = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "a.hon", .data = "import \"b.hon\"\n" });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "b.hon", .data = "import \"a.hon\"\n" });
    const a_path = try std.fmt.allocPrint(alloc, ".zig-cache/tmp/{s}/a.hon", .{tmp.sub_path});
    defer alloc.free(a_path);

    try expectCompilerDiagnostic(a_path, .sema_import_cycle);
}
