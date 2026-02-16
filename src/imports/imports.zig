const std = @import("std");
const mem = std.mem;
const fs = std.fs;

const source = @import("../source/source.zig");
const SourceCode = source.SourceCode;
const lexer = @import("../lexer/lexer.zig");
const TokenList = @import("../lexer/token.zig").TokenList;
const parser = @import("../parser/parser.zig");
const Ast = @import("../parser/ast.zig").Ast;
const NodeIndex = @import("../parser/ast.zig").NodeIndex;

pub const ResolvedImport = struct {
    namespace_name: []const u8,
    src: SourceCode,
    tokens: TokenList,
    ast: Ast,
    file_path: []const u8,
};

pub const ResolvedImports = struct {
    imports: std.ArrayList(ResolvedImport),
    map: std.AutoHashMapUnmanaged(NodeIndex, usize),
    has_errors: bool,

    pub fn empty(allocator: mem.Allocator) !ResolvedImports {
        return .{
            .imports = try std.ArrayList(ResolvedImport).initCapacity(allocator, 0),
            .map = .{},
            .has_errors = false,
        };
    }

    pub fn count(self: *const ResolvedImports) usize {
        return self.imports.items.len;
    }
};

pub fn resolveImports(
    allocator: mem.Allocator,
    ast: *const Ast,
    tokens: *const TokenList,
    main_src: *const SourceCode,
    main_file_path: []const u8,
) !ResolvedImports {
    var result = ResolvedImports{
        .imports = try std.ArrayList(ResolvedImport).initCapacity(allocator, 4),
        .map = .{},
        .has_errors = false,
    };

    // Track visited files to detect circular imports
    var visited = std.StringHashMap(void).init(allocator);
    defer visited.deinit();

    // Mark main file as visited
    const abs_main = fs.cwd().realpathAlloc(allocator, main_file_path) catch main_file_path;
    try visited.put(abs_main, {});

    // Get the directory of the main file
    const main_dir = std.fs.path.dirname(main_file_path) orelse ".";

    const program = ast.getProgram(ast.root);
    const decls = ast.getExtra(program.declarations);

    var next_src_id: source.Id = 1;

    for (decls) |decl_idx| {
        if (ast.getKind(decl_idx) != .import_decl) continue;

        const import_decl = ast.getImportDecl(decl_idx);
        const path_token = tokens.items[import_decl.path_token];
        const import_path = main_src.getSlice(path_token.start, path_token.start + path_token.len);

        // Resolve path relative to main file's directory
        const full_path = try std.fs.path.join(allocator, &.{ main_dir, import_path });

        // Check for circular imports
        const abs_path = fs.cwd().realpathAlloc(allocator, full_path) catch {
            // File doesn't exist
            std.debug.print("error: import file not found: {s}\n", .{import_path});
            result.has_errors = true;
            continue;
        };

        if (visited.contains(abs_path)) {
            std.debug.print("error: circular import detected: {s}\n", .{import_path});
            result.has_errors = true;
            continue;
        }
        try visited.put(abs_path, {});

        // Load, lex, and parse the imported file
        const imported_src = source.fromFile(allocator, full_path, next_src_id) catch {
            std.debug.print("error: cannot read import file: {s}\n", .{import_path});
            result.has_errors = true;
            continue;
        };
        next_src_id += 1;

        const lex_result = try lexer.scan(allocator, &imported_src);
        if (lex_result.errors.hasErrors()) {
            std.debug.print("error: lexer errors in imported file: {s}\n", .{import_path});
            result.has_errors = true;
            continue;
        }

        const parse_result = try parser.parse(allocator, lex_result.tokens, &imported_src);
        if (parse_result.errors.hasErrors()) {
            std.debug.print("error: parse errors in imported file: {s}\n", .{import_path});
            result.has_errors = true;
            continue;
        }

        // Derive namespace name from filename stem
        const basename = std.fs.path.basename(import_path);
        const ns_name = if (std.mem.indexOf(u8, basename, ".")) |dot_pos|
            basename[0..dot_pos]
        else
            basename;

        const idx = result.imports.items.len;
        try result.imports.append(allocator, .{
            .namespace_name = try allocator.dupe(u8, ns_name),
            .src = imported_src,
            .tokens = lex_result.tokens,
            .ast = parse_result.ast,
            .file_path = full_path,
        });
        try result.map.put(allocator, decl_idx, idx);
    }

    return result;
}
