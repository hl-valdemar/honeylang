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
    defer hir_stage.deinit();
    var sema_stage = try buildSema(ctx, &render, &hir_stage);
    defer sema_stage.deinit();
    var opt_stage = try buildOptimized(ctx, &render, &sema_stage);
    defer opt_stage.deinit();

    if (render.enabled) {
        const mir_opt_render = try opt_stage.opt.mir_optimized.render(render.alloc(), ctx.str_pool);
        render.printRendered("Rendered MIR (optimized)", mir_opt_render);
    }

    if (ctx.print_diagnostics)
        try render.printDiagnostics(ctx.diagnostics, hir_stage.loader.moduleSources(), ctx.src);
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

    fn printDiagnostics(self: *Renderer, diagnostics: *const Diagnostic, sources: []const Source, fallback_src: *const Source) !void {
        if (diagnostics.len() == 0) return;

        const diag_render = try diagnostics.renderWithSources(self.alloc(), sources, fallback_src);
        printSection("Diagnostics", diag_render);
    }
};

const HirStage = struct {
    arena: heap.ArenaAllocator,
    loader: ImportLoader,
    module_hirs: []const *const HIR,

    fn deinit(self: *HirStage) void {
        self.loader.deinit();
        self.arena.deinit();
    }
};

const SemaStage = struct {
    arena: heap.ArenaAllocator,
    sema: Sema,

    fn deinit(self: *SemaStage) void {
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
    errdefer loader.deinit();

    var ast_render: ?[]const u8 = null;
    const ast_dump: ?ImportLoader.AstDump = if (render.enabled) .{
        .alloc = render.alloc(),
        .rendered = &ast_render,
    } else null;

    _ = try loader.loadRoot(ast_dump);
    const module_hirs = try loader.moduleHirs(hir_alloc);

    if (ast_render) |rendered|
        render.printRendered("Rendered AST", rendered);

    return .{ .arena = hir_arena, .loader = loader, .module_hirs = module_hirs };
}

fn buildSema(ctx: Context, render: *Renderer, hir_stage: *HirStage) !SemaStage {
    if (render.enabled) {
        const hir_render = try hir_stage.loader.moduleHir(hir_stage.loader.root_module).render(render.alloc());
        render.printRendered("Rendered HIR", hir_render);
    }

    var sema_arena = heap.ArenaAllocator.init(ctx.alloc);
    errdefer sema_arena.deinit();
    const sema_alloc = sema_arena.allocator();

    var sema = Sema.initModules(
        hir_stage.module_hirs,
        hir_stage.loader.root_module,
        ctx.str_pool,
        ctx.diagnostics,
        ctx.alloc,
    );
    errdefer sema.deinit(sema_alloc);
    try sema.analyze(sema_alloc);

    return .{ .arena = sema_arena, .sema = sema };
}

fn buildOptimized(ctx: Context, render: *Renderer, sema_stage: *SemaStage) !OptimizerStage {
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

test "compiler import loader caches shared modules by canonical path" {
    const alloc = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "shared.hon", .data = "value :: 1\n" });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "left.hon", .data = "import \"shared.hon\"\nleft_value :: shared.value\n" });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "right.hon", .data = "import \"./shared.hon\"\nright_value :: shared.value\n" });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "main.hon", .data = "import \"left.hon\"\nimport \"right.hon\"\nx :: left.left_value + right.right_value\n" });

    const main_path = try std.fmt.allocPrint(alloc, ".zig-cache/tmp/{s}/main.hon", .{tmp.sub_path});
    defer alloc.free(main_path);

    var src = try SourceManager.init.fromFile(alloc, std.testing.io, main_path);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = Diagnostic.init();
    defer diagnostics.deinit(alloc);

    var loader = ImportLoader.init(.{
        .alloc = alloc,
        .io = std.testing.io,
        .src = &src,
        .str_pool = &str_pool,
        .diagnostics = &diagnostics,
    });
    defer loader.deinit();

    _ = try loader.loadRoot(null);

    try std.testing.expect(!diagnostics.hasErrors());
    try std.testing.expectEqual(@as(usize, 4), loader.moduleCount());
}

test "compiler import loader caches symlinked modules by real path" {
    const alloc = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "shared.hon", .data = "value :: 1\n" });
    tmp.dir.symLink(std.testing.io, "shared.hon", "shared_link.hon", .{}) catch |err| switch (err) {
        error.AccessDenied, error.PermissionDenied => return error.SkipZigTest,
        else => return err,
    };
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "left.hon", .data = "import \"shared.hon\"\nleft_value :: shared.value\n" });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "right.hon", .data = "import \"shared_link.hon\"\nright_value :: shared_link.value\n" });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "main.hon", .data = "import \"left.hon\"\nimport \"right.hon\"\nx :: left.left_value + right.right_value\n" });

    const main_path = try std.fmt.allocPrint(alloc, ".zig-cache/tmp/{s}/main.hon", .{tmp.sub_path});
    defer alloc.free(main_path);

    var src = try SourceManager.init.fromFile(alloc, std.testing.io, main_path);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = Diagnostic.init();
    defer diagnostics.deinit(alloc);

    var loader = ImportLoader.init(.{
        .alloc = alloc,
        .io = std.testing.io,
        .src = &src,
        .str_pool = &str_pool,
        .diagnostics = &diagnostics,
    });
    defer loader.deinit();

    _ = try loader.loadRoot(null);

    try std.testing.expect(!diagnostics.hasErrors());
    try std.testing.expectEqual(@as(usize, 4), loader.moduleCount());
}

test "compiler root HIR renders module namespace instead of copied import body" {
    const alloc = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "math.hon", .data = "value :: 1\n" });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "main.hon", .data = "import \"math.hon\"\nx :: math.value\n" });

    const main_path = try std.fmt.allocPrint(alloc, ".zig-cache/tmp/{s}/main.hon", .{tmp.sub_path});
    defer alloc.free(main_path);

    var src = try SourceManager.init.fromFile(alloc, std.testing.io, main_path);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = Diagnostic.init();
    defer diagnostics.deinit(alloc);

    var loader = ImportLoader.init(.{
        .alloc = alloc,
        .io = std.testing.io,
        .src = &src,
        .str_pool = &str_pool,
        .diagnostics = &diagnostics,
    });
    defer loader.deinit();

    _ = try loader.loadRoot(null);

    const rendered = try loader.moduleHir(loader.root_module).render(alloc);
    defer alloc.free(rendered);

    try std.testing.expect(std.mem.indexOf(u8, rendered, "decl_module_namespace(\"math\", module=M1)") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "decl_const(\"value\"") == null);
}

test "compiler renders diagnostics from imported files" {
    const alloc = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "lib.hon", .data = "value :: missing\n" });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "main.hon", .data = "import \"lib.hon\"\nx :: lib.value\n" });

    const main_path = try std.fmt.allocPrint(alloc, ".zig-cache/tmp/{s}/main.hon", .{tmp.sub_path});
    defer alloc.free(main_path);

    var src = try SourceManager.init.fromFile(alloc, std.testing.io, main_path);
    defer src.deinit(alloc);

    var str_pool = StringPool.init();
    defer str_pool.deinit(alloc);

    var diagnostics = Diagnostic.init();
    defer diagnostics.deinit(alloc);

    var loader = ImportLoader.init(.{
        .alloc = alloc,
        .io = std.testing.io,
        .src = &src,
        .str_pool = &str_pool,
        .diagnostics = &diagnostics,
    });
    defer loader.deinit();

    const root_module = try loader.loadRoot(null);
    const module_hirs = try loader.moduleHirs(alloc);
    defer alloc.free(module_hirs);

    var sema = Sema.initModules(module_hirs, root_module, &str_pool, &diagnostics, alloc);
    defer sema.deinit(alloc);
    try sema.analyze(alloc);

    const rendered = try diagnostics.renderWithSources(alloc, loader.moduleSources(), &src);
    defer alloc.free(rendered);

    try std.testing.expect(std.mem.indexOf(u8, rendered, "lib.hon:1:10") != null);
    try std.testing.expect(std.mem.indexOf(u8, rendered, "1 | value :: missing") != null);
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
