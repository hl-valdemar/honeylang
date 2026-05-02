const std = @import("std");
const fs = std.fs;
const heap = std.heap;
const mem = std.mem;

const Diagnostic = @import("diagnostic/Store.zig");
const HIR = @import("parser/HIR.zig");
const Lexer = @import("lexer/Lexer.zig");
const Optimizer = @import("optimizer/Optimizer.zig");
const Parser = @import("parser/Parser.zig");
const Payload = @import("root.zig").Payload;
const Sema = @import("sema/Sema.zig");
const Source = @import("source/Source.zig");
const SourceManager = @import("source/Manager.zig");
const StringPool = @import("util/StringPool.zig");

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

    var hir = try loader.parseAndLower(hir_alloc, ctx.src, render);
    try loader.expandImports(hir_alloc, &hir, ctx.src.path);

    return .{ .arena = hir_arena, .hir = hir };
}

const ImportLoader = struct {
    ctx: Context,
    stack: std.ArrayList(StringPool.ID),

    fn init(ctx: Context) ImportLoader {
        return .{
            .ctx = ctx,
            .stack = .empty,
        };
    }

    fn deinit(self: *ImportLoader) void {
        self.stack.deinit(self.ctx.alloc);
    }

    fn parseAndLower(self: *ImportLoader, hir_alloc: mem.Allocator, src: *const Source, render: *Renderer) !HIR {
        var parse_arena = heap.ArenaAllocator.init(self.ctx.alloc);
        defer parse_arena.deinit();
        const parse_alloc = parse_arena.allocator();

        var lexer = Lexer.init(.{
            .src = src,
            .str_pool = self.ctx.str_pool,
            .shared_alloc = self.ctx.alloc,
            .diagnostics = self.ctx.diagnostics,
        });
        defer lexer.deinit(parse_alloc);
        try lexer.scan(parse_alloc);

        var parser = Parser.init(.{
            .src = src,
            .tokens = lexer.tokens.slice(),
            .diagnostic_alloc = self.ctx.alloc,
            .diagnostics = self.ctx.diagnostics,
        });
        defer parser.deinit(parse_alloc);
        const ast = try parser.parse(parse_alloc);

        if (render.enabled and src == self.ctx.src) {
            const ast_render = try ast.render(render.alloc(), src.contents, self.ctx.str_pool);
            render.printRendered("Rendered AST", ast_render);
        }

        return Parser.lower(hir_alloc, &ast, self.ctx.str_pool, self.ctx.diagnostics, self.ctx.alloc);
    }

    fn expandImports(self: *ImportLoader, hir_alloc: mem.Allocator, hir: *HIR, importer_path: ?[]const u8) anyerror!void {
        const stack_top = self.stack.items.len;
        if (importer_path) |path| {
            if (self.resolvePathId(null, path)) |resolved| {
                var already_active = false;
                for (self.stack.items) |active_path| {
                    if (active_path == resolved) {
                        already_active = true;
                        break;
                    }
                }
                if (!already_active)
                    try self.stack.append(self.ctx.alloc, resolved);
            } else |_| {}
        }
        defer {
            self.stack.items.len = stack_top;
        }

        const root_refs = hir.rootDecls();
        const expanded = try self.expandDeclList(hir_alloc, hir, root_refs, importer_path);
        hir.root_start = expanded.start;
        hir.root_end = expanded.end;
    }

    fn expandDeclList(
        self: *ImportLoader,
        hir_alloc: mem.Allocator,
        hir: *HIR,
        refs: []const Payload,
        importer_path: ?[]const u8,
    ) anyerror!struct { start: Payload, end: Payload } {
        var expanded: std.ArrayList(Payload) = .empty;
        defer expanded.deinit(hir_alloc);

        for (refs) |ref| {
            const inst = hir.insts.get(@intFromEnum(ref));
            switch (inst.tag) {
                .import_decl => {
                    const namespace_ref = try self.expandImport(hir_alloc, hir, inst, importer_path);
                    try expanded.append(hir_alloc, namespace_ref);
                },
                .decl_namespace => {
                    try self.expandNamespaceImports(hir_alloc, hir, ref, importer_path);
                    try expanded.append(hir_alloc, ref);
                },
                else => try expanded.append(hir_alloc, ref),
            }
        }

        const range = try hir.appendRefList(hir_alloc, expanded.items);
        return .{ .start = range.start, .end = range.end };
    }

    fn expandNamespaceImports(
        self: *ImportLoader,
        hir_alloc: mem.Allocator,
        hir: *HIR,
        namespace_ref: HIR.Inst.Ref,
        importer_path: ?[]const u8,
    ) anyerror!void {
        const namespace = hir.insts.get(@intFromEnum(namespace_ref));
        const block_ref = namespace.data.b;
        const block = hir.insts.get(@intFromEnum(block_ref));
        std.debug.assert(block.tag == .block);

        const refs = hir.refSlice(block.data.a, block.data.b);
        const expanded = try self.expandDeclList(hir_alloc, hir, refs, importer_path);
        hir.insts.items(.data)[@intFromEnum(block_ref)] = .{ .a = expanded.start, .b = expanded.end };
    }

    fn expandImport(
        self: *ImportLoader,
        hir_alloc: mem.Allocator,
        hir: *HIR,
        inst: HIR.Inst,
        importer_path: ?[]const u8,
    ) anyerror!HIR.Inst.Ref {
        const namespace_name = try self.importNamespaceName(inst);
        if (!isValidNamespaceName(namespace_name))
            _ = try self.addDiagnostic(.sema_import_invalid_namespace_name);

        if (inst.data.b.to(StringPool.ID) == .none)
            return self.emitImportTrapNamespace(hir_alloc, hir, namespace_name, .sema_import_file_not_found);

        const import_path = self.ctx.str_pool.get(inst.data.b.to(StringPool.ID));
        const resolved_path_id = self.resolvePathId(importer_path, import_path) catch
            return self.emitImportTrapNamespace(hir_alloc, hir, namespace_name, .sema_import_file_not_found);
        const resolved_path = self.ctx.str_pool.get(resolved_path_id);

        for (self.stack.items) |active_path| {
            if (active_path == resolved_path_id)
                return self.emitImportTrapNamespace(hir_alloc, hir, namespace_name, .sema_import_cycle);
        }

        try self.stack.append(self.ctx.alloc, resolved_path_id);
        defer {
            _ = self.stack.pop();
        }

        var import_arena = heap.ArenaAllocator.init(self.ctx.alloc);
        defer import_arena.deinit();
        const import_alloc = import_arena.allocator();

        var src = SourceManager.init.fromFile(import_alloc, self.ctx.io, resolved_path) catch
            return self.emitImportTrapNamespace(hir_alloc, hir, namespace_name, .sema_import_file_not_found);
        defer src.deinit(import_alloc);

        var disabled_render = Renderer.init(self.ctx.alloc, false);
        defer disabled_render.deinit();
        var imported_hir = try self.parseAndLower(import_alloc, &src, &disabled_render);
        defer imported_hir.deinit(import_alloc);
        try self.expandImports(import_alloc, &imported_hir, src.path);

        const body = try self.copyImportedRootAsBlock(hir_alloc, hir, &imported_hir);
        return hir.emit(hir_alloc, .decl_namespace, .{
            .a = Payload.from(try self.ctx.str_pool.intern(self.ctx.alloc, namespace_name)),
            .b = body,
        });
    }

    fn importNamespaceName(self: *ImportLoader, inst: HIR.Inst) ![]const u8 {
        if (inst.data.a.to(StringPool.ID) != .none)
            return self.ctx.str_pool.get(inst.data.a.to(StringPool.ID));

        if (inst.data.b.to(StringPool.ID) == .none) return "";
        const path = self.ctx.str_pool.get(inst.data.b.to(StringPool.ID));
        const basename = fs.path.basename(path);
        if (mem.lastIndexOfScalar(u8, basename, '.')) |dot_idx|
            return basename[0..dot_idx];
        return basename;
    }

    fn resolvePathId(self: *ImportLoader, importer_path: ?[]const u8, import_path: []const u8) !StringPool.ID {
        const alloc = self.ctx.alloc;
        const joined = if (fs.path.isAbsolute(import_path))
            try alloc.dupe(u8, import_path)
        else joined: {
            const base = if (importer_path) |path| fs.path.dirname(path) orelse "." else ".";
            break :joined try fs.path.join(alloc, &.{ base, import_path });
        };
        defer alloc.free(joined);

        const resolved = try fs.path.resolve(alloc, &.{joined});
        defer alloc.free(resolved);
        return self.ctx.str_pool.intern(self.ctx.alloc, resolved);
    }

    fn emitImportTrapNamespace(
        self: *ImportLoader,
        hir_alloc: mem.Allocator,
        hir: *HIR,
        namespace_name: []const u8,
        tag: Diagnostic.Tag,
    ) !HIR.Inst.Ref {
        const diag_ref = try self.addDiagnostic(tag);
        const trap = try hir.emit(hir_alloc, .trap, .{ .a = Payload.from(diag_ref) });
        const refs = try hir.appendRefList(hir_alloc, &.{trap});
        const block = try hir.emit(hir_alloc, .block, .{ .a = refs.start, .b = refs.end });
        const name = try self.ctx.str_pool.intern(self.ctx.alloc, namespace_name);
        return hir.emit(hir_alloc, .decl_namespace, .{
            .a = Payload.from(name),
            .b = block,
        });
    }

    fn copyImportedRootAsBlock(self: *ImportLoader, hir_alloc: mem.Allocator, dst: *HIR, src: *const HIR) !HIR.Inst.Ref {
        const map = try hir_alloc.alloc(Payload, src.insts.len);
        defer hir_alloc.free(map);
        @memset(map, .none);

        var copied_refs: std.ArrayList(Payload) = .empty;
        defer copied_refs.deinit(hir_alloc);

        for (src.rootDecls()) |ref| {
            const copied = try self.copyInst(hir_alloc, dst, src, map, ref);
            try copied_refs.append(hir_alloc, copied);
        }

        const refs = try dst.appendRefList(hir_alloc, copied_refs.items);
        return dst.emit(hir_alloc, .block, .{ .a = refs.start, .b = refs.end });
    }

    fn copyInst(
        self: *ImportLoader,
        hir_alloc: mem.Allocator,
        dst: *HIR,
        src: *const HIR,
        map: []Payload,
        ref: HIR.Inst.Ref,
    ) anyerror!HIR.Inst.Ref {
        if (ref == .none) return .none;
        const idx = @intFromEnum(ref);
        if (map[idx] != .none) return map[idx];

        const inst = src.insts.get(idx);
        const copied: HIR.Inst.Ref = switch (inst.tag) {
            .int_literal, .float_literal, .str_literal, .trap, .ref => try dst.emit(hir_alloc, inst.tag, inst.data),
            .not, .negate => try dst.emit(hir_alloc, inst.tag, .{
                .a = try self.copyInst(hir_alloc, dst, src, map, inst.data.a),
            }),
            .qualified_ref => try dst.emit(hir_alloc, .qualified_ref, .{
                .a = try self.copyInst(hir_alloc, dst, src, map, inst.data.a),
                .b = inst.data.b,
            }),
            .add, .sub, .mul, .div => try dst.emit(hir_alloc, inst.tag, .{
                .a = try self.copyInst(hir_alloc, dst, src, map, inst.data.a),
                .b = try self.copyInst(hir_alloc, dst, src, map, inst.data.b),
            }),
            .decl_const, .decl_var => blk: {
                const info = src.declInfo(HIR.asDeclRef(inst.data.b));
                const decl_ref = try dst.emitDeclInfo(hir_alloc, .{
                    .type = try self.copyInst(hir_alloc, dst, src, map, info.type),
                    .value = try self.copyInst(hir_alloc, dst, src, map, info.value),
                });
                break :blk try dst.emit(hir_alloc, inst.tag, .{
                    .a = inst.data.a,
                    .b = HIR.asPayload(decl_ref),
                });
            },
            .decl_namespace => try dst.emit(hir_alloc, .decl_namespace, .{
                .a = inst.data.a,
                .b = try self.copyInst(hir_alloc, dst, src, map, inst.data.b),
            }),
            .param => try dst.emit(hir_alloc, .param, .{
                .a = inst.data.a,
                .b = try self.copyInst(hir_alloc, dst, src, map, inst.data.b),
            }),
            .ret => try dst.emit(hir_alloc, .ret, .{
                .a = try self.copyInst(hir_alloc, dst, src, map, inst.data.a),
            }),
            .block => blk: {
                const refs = src.refSlice(inst.data.a, inst.data.b);
                var copied_refs: std.ArrayList(Payload) = .empty;
                defer copied_refs.deinit(hir_alloc);
                for (refs) |item| try copied_refs.append(hir_alloc, try self.copyInst(hir_alloc, dst, src, map, item));
                const range = try dst.appendRefList(hir_alloc, copied_refs.items);
                break :blk try dst.emit(hir_alloc, .block, .{ .a = range.start, .b = range.end });
            },
            .decl_func => blk: {
                const info = src.funcInfo(HIR.asFuncRef(inst.data.b));
                const params = src.refSlice(info.params_start, info.params_end);
                var copied_params: std.ArrayList(Payload) = .empty;
                defer copied_params.deinit(hir_alloc);
                for (params) |param| try copied_params.append(hir_alloc, try self.copyInst(hir_alloc, dst, src, map, param));
                const param_range = try dst.appendRefList(hir_alloc, copied_params.items);
                const func_ref = try dst.emitFuncInfo(hir_alloc, .{
                    .params_start = param_range.start,
                    .params_end = param_range.end,
                    .ret_type = try self.copyInst(hir_alloc, dst, src, map, info.ret_type),
                    .body = try self.copyInst(hir_alloc, dst, src, map, info.body),
                    .flags = info.flags,
                });
                break :blk try dst.emit(hir_alloc, .decl_func, .{
                    .a = inst.data.a,
                    .b = HIR.asPayload(func_ref),
                });
            },
            .if_simple => try dst.emit(hir_alloc, .if_simple, .{
                .a = try self.copyInst(hir_alloc, dst, src, map, inst.data.a),
                .b = try self.copyInst(hir_alloc, dst, src, map, inst.data.b),
            }),
            .if_else => blk: {
                const info = src.branchInfo(HIR.asBranchRef(inst.data.b));
                const branch_ref = try dst.emitBranchInfo(hir_alloc, .{
                    .body = try self.copyInst(hir_alloc, dst, src, map, info.body),
                    .else_node = try self.copyInst(hir_alloc, dst, src, map, info.else_node),
                });
                break :blk try dst.emit(hir_alloc, .if_else, .{
                    .a = try self.copyInst(hir_alloc, dst, src, map, inst.data.a),
                    .b = HIR.asPayload(branch_ref),
                });
            },
            .import_decl => try self.emitImportTrapNamespace(hir_alloc, dst, "<import>", .sema_import_file_not_found),
        };

        map[idx] = copied;
        return copied;
    }

    fn addDiagnostic(self: *ImportLoader, tag: Diagnostic.Tag) !Diagnostic.Ref {
        return self.ctx.diagnostics.add(self.ctx.alloc, .{
            .stage = .sema,
            .severity = .err,
            .tag = tag,
            .span = null,
        });
    }
};

fn isValidNamespaceName(name: []const u8) bool {
    if (name.len == 0) return false;
    if (!std.ascii.isAlphabetic(name[0]) and name[0] != '_') return false;
    for (name[1..]) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_') return false;
    }
    return true;
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
