const std = @import("std");
const fs = std.fs;
const heap = std.heap;
const mem = std.mem;

const Self = @This();

const Diagnostic = @import("../diagnostic/Store.zig");
const HIR = @import("../parser/HIR.zig");
const Lexer = @import("../lexer/Lexer.zig");
const Parser = @import("../parser/Parser.zig");
const Payload = @import("../root.zig").Payload;
const Source = @import("../source/Source.zig");
const SourceManager = @import("../source/Manager.zig");
const StringPool = @import("../util/StringPool.zig");

pub const AstDump = struct {
    alloc: mem.Allocator,
    rendered: *?[]const u8,
};

pub const LoadState = enum {
    loading,
    loaded,
};

pub const Module = struct {
    arena: *heap.ArenaAllocator,
    source: Source,
    path_id: StringPool.ID,
    hir: HIR,
    state: LoadState,
    source_owned: bool,
};

const Context = struct {
    alloc: mem.Allocator,
    io: std.Io,
    root_src: *const Source,
    str_pool: *StringPool,
    diagnostics: *Diagnostic,
};

ctx: Context,
root_module: HIR.ModuleRef,
modules: std.MultiArrayList(Module),
path_map: std.AutoHashMapUnmanaged(StringPool.ID, HIR.ModuleRef),

pub fn init(ctx: anytype) Self {
    return .{
        .ctx = .{
            .alloc = ctx.alloc,
            .io = ctx.io,
            .root_src = ctx.src,
            .str_pool = ctx.str_pool,
            .diagnostics = ctx.diagnostics,
        },
        .root_module = .none,
        .modules = .empty,
        .path_map = .{},
    };
}

pub fn deinit(self: *Self) void {
    const arenas = self.modules.items(.arena);
    const sources = self.modules.items(.source);
    const hirs = self.modules.items(.hir);
    const source_owned = self.modules.items(.source_owned);

    for (0..self.modules.len) |idx| {
        const module_alloc = arenas[idx].allocator();
        hirs[idx].deinit(module_alloc);
        if (source_owned[idx])
            sources[idx].deinit(module_alloc);
        arenas[idx].deinit();
        self.ctx.alloc.destroy(arenas[idx]);
    }
    self.modules.deinit(self.ctx.alloc);
    self.path_map.deinit(self.ctx.alloc);
}

pub fn loadRoot(self: *Self, ast_dump: ?AstDump) !HIR.ModuleRef {
    const path_id = if (self.ctx.root_src.path) |path|
        try self.resolvePathId(null, path)
    else
        try self.ctx.str_pool.intern(self.ctx.alloc, "<root>");

    const arena = try self.createModuleArena();
    var module_owned_by_loader = false;
    errdefer if (!module_owned_by_loader) self.destroyModuleArena(arena);
    const module_alloc = arena.allocator();

    var hir = try self.parseAndLower(module_alloc, self.ctx.root_src, ast_dump);
    errdefer if (!module_owned_by_loader) hir.deinit(module_alloc);

    const module_ref = try self.appendModule(.{
        .arena = arena,
        .source = self.ctx.root_src.*,
        .path_id = path_id,
        .hir = hir,
        .state = .loading,
        .source_owned = false,
    });
    module_owned_by_loader = true;
    self.root_module = module_ref;
    try self.path_map.put(self.ctx.alloc, path_id, module_ref);
    errdefer _ = self.path_map.remove(path_id);

    try self.resolveModuleImports(module_ref);
    self.setModuleState(module_ref, .loaded);
    return module_ref;
}

pub fn moduleHir(self: *const Self, module_ref: HIR.ModuleRef) *const HIR {
    return &self.modules.items(.hir)[@intFromEnum(module_ref)];
}

pub fn modulePathId(self: *const Self, module_ref: HIR.ModuleRef) StringPool.ID {
    return self.modules.items(.path_id)[@intFromEnum(module_ref)];
}

pub fn moduleCount(self: *const Self) usize {
    return self.modules.len;
}

pub fn moduleHirs(self: *const Self, alloc: mem.Allocator) ![]const *const HIR {
    var hirs = try alloc.alloc(*const HIR, self.modules.len);
    const module_hirs_slice = self.modules.items(.hir);
    for (0..self.modules.len) |idx| {
        hirs[idx] = &module_hirs_slice[idx];
    }
    return hirs;
}

pub fn parseAndLower(self: *Self, hir_alloc: mem.Allocator, src: *const Source, ast_dump: ?AstDump) !HIR {
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

    if (ast_dump) |dump| {
        if (src == self.ctx.root_src)
            dump.rendered.* = try ast.render(dump.alloc, src.contents, self.ctx.str_pool);
    }

    return Parser.lower(hir_alloc, &ast, src.contents, self.ctx.str_pool, self.ctx.diagnostics, self.ctx.alloc);
}

fn appendModule(self: *Self, module_value: Module) !HIR.ModuleRef {
    const module_ref: HIR.ModuleRef = Payload.fromIndex(self.modules.len).to(HIR.ModuleRef);
    try self.modules.append(self.ctx.alloc, module_value);
    return module_ref;
}

fn createModuleArena(self: *Self) !*heap.ArenaAllocator {
    const arena = try self.ctx.alloc.create(heap.ArenaAllocator);
    arena.* = heap.ArenaAllocator.init(self.ctx.alloc);
    return arena;
}

fn destroyModuleArena(self: *Self, arena: *heap.ArenaAllocator) void {
    arena.deinit();
    self.ctx.alloc.destroy(arena);
}

fn moduleHirMut(self: *Self, module_ref: HIR.ModuleRef) *HIR {
    return &self.modules.items(.hir)[@intFromEnum(module_ref)];
}

fn moduleAlloc(self: *Self, module_ref: HIR.ModuleRef) mem.Allocator {
    return self.modules.items(.arena)[@intFromEnum(module_ref)].allocator();
}

fn moduleSourcePath(self: *Self, module_ref: HIR.ModuleRef) ?[]const u8 {
    return self.modules.items(.source)[@intFromEnum(module_ref)].path;
}

fn moduleState(self: *Self, module_ref: HIR.ModuleRef) LoadState {
    return self.modules.items(.state)[@intFromEnum(module_ref)];
}

fn setModuleState(self: *Self, module_ref: HIR.ModuleRef, state: LoadState) void {
    self.modules.items(.state)[@intFromEnum(module_ref)] = state;
}

fn resolveModuleImports(self: *Self, module_ref: HIR.ModuleRef) anyerror!void {
    const root_refs = self.moduleHir(module_ref).rootDecls();
    try self.resolveDeclList(module_ref, root_refs);
}

fn resolveDeclList(self: *Self, module_ref: HIR.ModuleRef, refs: []const Payload) anyerror!void {
    for (refs) |ref| {
        const inst = self.moduleHir(module_ref).insts.get(@intFromEnum(ref));
        switch (inst.tag) {
            .import_decl => try self.resolveImportInst(module_ref, ref, inst),
            .decl_namespace => try self.resolveNamespaceImports(module_ref, ref),
            else => {},
        }
    }
}

fn resolveNamespaceImports(self: *Self, module_ref: HIR.ModuleRef, namespace_ref: HIR.Inst.Ref) anyerror!void {
    const namespace = self.moduleHir(module_ref).insts.get(@intFromEnum(namespace_ref));
    const block_ref = namespace.data.b;
    const block = self.moduleHir(module_ref).insts.get(@intFromEnum(block_ref));
    std.debug.assert(block.tag == .block);

    const refs = self.moduleHir(module_ref).refSlice(block.data.a, block.data.b);
    try self.resolveDeclList(module_ref, refs);
}

fn resolveImportInst(self: *Self, importer_ref: HIR.ModuleRef, inst_ref: HIR.Inst.Ref, inst: HIR.Inst) anyerror!void {
    const namespace_name = try self.importNamespaceName(inst);
    if (!isValidNamespaceName(namespace_name))
        _ = try self.addDiagnostic(.sema_import_invalid_namespace_name);

    const name_id = try self.ctx.str_pool.intern(self.ctx.alloc, namespace_name);
    if (inst.data.b.to(StringPool.ID) == .none) {
        try self.replaceWithTrapNamespace(importer_ref, inst_ref, name_id, .sema_import_file_not_found);
        return;
    }

    const import_path = self.ctx.str_pool.get(inst.data.b.to(StringPool.ID));
    const target_ref = self.loadImportedModule(importer_ref, import_path) catch |err| switch (err) {
        error.ImportFileNotFound => {
            try self.replaceWithTrapNamespace(importer_ref, inst_ref, name_id, .sema_import_file_not_found);
            return;
        },
        error.ImportCycle => {
            try self.replaceWithTrapNamespace(importer_ref, inst_ref, name_id, .sema_import_cycle);
            return;
        },
        else => return err,
    };

    const hir = self.moduleHirMut(importer_ref);
    hir.insts.items(.tag)[@intFromEnum(inst_ref)] = .decl_module_namespace;
    hir.insts.items(.data)[@intFromEnum(inst_ref)] = .{
        .a = Payload.from(name_id),
        .b = Payload.from(target_ref),
    };
}

fn loadImportedModule(self: *Self, importer_ref: HIR.ModuleRef, import_path: []const u8) anyerror!HIR.ModuleRef {
    const importer_path = self.moduleSourcePath(importer_ref);
    const resolved_path_id = self.resolvePathId(importer_path, import_path) catch return error.ImportFileNotFound;

    if (self.path_map.get(resolved_path_id)) |cached_ref| {
        return switch (self.moduleState(cached_ref)) {
            .loaded => cached_ref,
            .loading => error.ImportCycle,
        };
    }

    const resolved_path = self.ctx.str_pool.get(resolved_path_id);
    const arena = try self.createModuleArena();
    var module_owned_by_loader = false;
    errdefer if (!module_owned_by_loader) self.destroyModuleArena(arena);
    const import_alloc = arena.allocator();

    var src = SourceManager.init.fromFile(import_alloc, self.ctx.io, resolved_path) catch return error.ImportFileNotFound;
    errdefer if (!module_owned_by_loader) src.deinit(import_alloc);

    var hir = try self.parseAndLower(import_alloc, &src, null);
    errdefer if (!module_owned_by_loader) hir.deinit(import_alloc);

    const module_ref = try self.appendModule(.{
        .arena = arena,
        .source = src,
        .path_id = resolved_path_id,
        .hir = hir,
        .state = .loading,
        .source_owned = true,
    });
    module_owned_by_loader = true;
    try self.path_map.put(self.ctx.alloc, resolved_path_id, module_ref);
    errdefer _ = self.path_map.remove(resolved_path_id);

    try self.resolveModuleImports(module_ref);
    self.setModuleState(module_ref, .loaded);
    return module_ref;
}

fn importNamespaceName(self: *Self, inst: HIR.Inst) ![]const u8 {
    if (inst.data.a.to(StringPool.ID) != .none)
        return self.ctx.str_pool.get(inst.data.a.to(StringPool.ID));

    if (inst.data.b.to(StringPool.ID) == .none) return "";
    const path = self.ctx.str_pool.get(inst.data.b.to(StringPool.ID));
    const basename = fs.path.basename(path);
    if (mem.lastIndexOfScalar(u8, basename, '.')) |dot_idx|
        return basename[0..dot_idx];
    return basename;
}

fn resolvePathId(self: *Self, importer_path: ?[]const u8, import_path: []const u8) !StringPool.ID {
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

fn replaceWithTrapNamespace(
    self: *Self,
    module_ref: HIR.ModuleRef,
    inst_ref: HIR.Inst.Ref,
    namespace_name: StringPool.ID,
    tag: Diagnostic.Tag,
) !void {
    const hir = self.moduleHirMut(module_ref);
    const module_alloc = self.moduleAlloc(module_ref);
    const diag_ref = try self.addDiagnostic(tag);
    const trap = try hir.emit(module_alloc, .trap, .{ .a = Payload.from(diag_ref) });
    const refs = try hir.appendRefList(module_alloc, &.{trap});
    const block = try hir.emit(module_alloc, .block, .{ .a = refs.start, .b = refs.end });

    hir.insts.items(.tag)[@intFromEnum(inst_ref)] = .decl_namespace;
    hir.insts.items(.data)[@intFromEnum(inst_ref)] = .{
        .a = Payload.from(namespace_name),
        .b = block,
    };
}

fn addDiagnostic(self: *Self, tag: Diagnostic.Tag) !Diagnostic.Ref {
    return self.ctx.diagnostics.add(self.ctx.alloc, .{
        .stage = .sema,
        .severity = .err,
        .tag = tag,
        .span = null,
    });
}

fn isValidNamespaceName(name: []const u8) bool {
    if (name.len == 0) return false;
    if (!std.ascii.isAlphabetic(name[0]) and name[0] != '_') return false;
    for (name[1..]) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_') return false;
    }
    return true;
}
