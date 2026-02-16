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
const c_parser = @import("c_parser.zig");

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
        const kind = ast.getKind(decl_idx);

        if (kind == .c_import_block) {
            // Handle multi-include C import block
            const block = ast.getCImportBlock(decl_idx);
            const include_indices = ast.getExtra(block.includes);
            const define_indices = ast.getExtra(block.defines);

            // Get namespace name (required for block form)
            const name_tok = tokens.items[block.name_token];
            const ns_name = try allocator.dupe(u8, main_src.getSlice(name_tok.start, name_tok.start + name_tok.len));

            // Build combined C source from all includes
            var combined = try std.ArrayList(u8).initCapacity(allocator, 1024);
            const c_writer = combined.writer(allocator);

            // Prepend defines
            for (define_indices) |def_tok_idx| {
                const def_tok = tokens.items[def_tok_idx];
                const def_value = main_src.getSlice(def_tok.start, def_tok.start + def_tok.len);
                try c_writer.print("#define {s}\n", .{def_value});
            }

            // Read and concatenate all included headers
            var block_has_errors = false;
            for (include_indices) |inc_tok_idx| {
                const inc_tok = tokens.items[inc_tok_idx];
                const inc_path = main_src.getSlice(inc_tok.start, inc_tok.start + inc_tok.len);

                if (!mem.endsWith(u8, inc_path, ".h")) {
                    std.debug.print("error: import c include requires a .h header file, got: {s}\n", .{inc_path});
                    block_has_errors = true;
                    continue;
                }

                const inc_full_path = try std.fs.path.join(allocator, &.{ main_dir, inc_path });
                const file = fs.cwd().openFile(inc_full_path, .{ .mode = .read_only }) catch {
                    std.debug.print("error: cannot read C header file: {s}\n", .{inc_path});
                    block_has_errors = true;
                    continue;
                };
                defer file.close();
                const file_size = file.getEndPos() catch {
                    block_has_errors = true;
                    continue;
                };
                const header_buf = allocator.alloc(u8, file_size) catch {
                    block_has_errors = true;
                    continue;
                };
                _ = file.readAll(header_buf) catch {
                    block_has_errors = true;
                    continue;
                };
                try c_writer.writeAll(header_buf);
                try c_writer.writeAll("\n");
            }

            if (block_has_errors) {
                result.has_errors = true;
                continue;
            }

            const c_source = try combined.toOwnedSlice(allocator);

            const functions = c_parser.parse(allocator, c_source) catch {
                std.debug.print("error: failed to parse C headers in import block\n", .{});
                result.has_errors = true;
                continue;
            };

            const binding_source = generateBindingSource(allocator, functions) catch {
                result.has_errors = true;
                continue;
            };

            writeBindingFile(allocator, ns_name, binding_source);

            const binding_src = source.fromStr(binding_source, next_src_id);
            next_src_id += 1;

            const lex_result = try lexer.scan(allocator, &binding_src);
            if (lex_result.errors.hasErrors()) {
                std.debug.print("error: lexer errors in generated C binding for block import\n", .{});
                result.has_errors = true;
                continue;
            }

            const parse_result = try parser.parse(allocator, lex_result.tokens, &binding_src);
            if (parse_result.errors.hasErrors()) {
                std.debug.print("error: parse errors in generated C binding for block import\n", .{});
                result.has_errors = true;
                continue;
            }

            const import_result_idx = result.imports.items.len;
            try result.imports.append(allocator, .{
                .namespace_name = ns_name,
                .src = binding_src,
                .tokens = lex_result.tokens,
                .ast = parse_result.ast,
                .file_path = try std.fmt.allocPrint(allocator, "<c_import_block:{s}>", .{ns_name}),
            });
            try result.map.put(allocator, decl_idx, import_result_idx);
            continue;
        }

        if (kind != .import_decl and kind != .c_include_decl) continue;

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

        // Resolve namespace name: explicit name_token takes priority, otherwise derive from filename
        const ns_name = if (import_decl.name_token) |name_tok_idx| blk: {
            const name_tok = tokens.items[name_tok_idx];
            break :blk try allocator.dupe(u8, main_src.getSlice(name_tok.start, name_tok.start + name_tok.len));
        } else blk: {
            const basename = std.fs.path.basename(import_path);
            const stem = if (std.mem.indexOf(u8, basename, ".")) |dot_pos|
                basename[0..dot_pos]
            else
                basename;
            break :blk try allocator.dupe(u8, stem);
        };

        if (kind == .c_include_decl) {
            // C header import: validate extension, parse header, generate binding
            if (!mem.endsWith(u8, import_path, ".h")) {
                std.debug.print("error: import c include requires a .h header file, got: {s}\n", .{import_path});
                result.has_errors = true;
                continue;
            }

            const c_source = blk: {
                const file = fs.cwd().openFile(full_path, .{ .mode = .read_only }) catch {
                    std.debug.print("error: cannot read C header file: {s}\n", .{import_path});
                    result.has_errors = true;
                    continue;
                };
                defer file.close();
                const file_size = file.getEndPos() catch {
                    result.has_errors = true;
                    continue;
                };
                const buf = allocator.alloc(u8, file_size) catch {
                    result.has_errors = true;
                    continue;
                };
                _ = file.readAll(buf) catch {
                    result.has_errors = true;
                    continue;
                };
                break :blk buf;
            };

            const functions = c_parser.parse(allocator, c_source) catch {
                std.debug.print("error: failed to parse C header: {s}\n", .{import_path});
                result.has_errors = true;
                continue;
            };

            const binding_source = generateBindingSource(allocator, functions) catch {
                result.has_errors = true;
                continue;
            };

            // Write binding file to zig-out/ for inspection
            writeBindingFile(allocator, import_path, binding_source);

            // Lex and parse the generated binding
            const binding_src = source.fromStr(binding_source, next_src_id);
            next_src_id += 1;

            const lex_result = try lexer.scan(allocator, &binding_src);
            if (lex_result.errors.hasErrors()) {
                std.debug.print("error: lexer errors in generated C binding for: {s}\n", .{import_path});
                result.has_errors = true;
                continue;
            }

            const parse_result = try parser.parse(allocator, lex_result.tokens, &binding_src);
            if (parse_result.errors.hasErrors()) {
                std.debug.print("error: parse errors in generated C binding for: {s}\n", .{import_path});
                result.has_errors = true;
                continue;
            }

            const idx = result.imports.items.len;
            try result.imports.append(allocator, .{
                .namespace_name = ns_name,
                .src = binding_src,
                .tokens = lex_result.tokens,
                .ast = parse_result.ast,
                .file_path = full_path,
            });
            try result.map.put(allocator, decl_idx, idx);
        } else {
            // Native Honey import
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

            const idx = result.imports.items.len;
            try result.imports.append(allocator, .{
                .namespace_name = ns_name,
                .src = imported_src,
                .tokens = lex_result.tokens,
                .ast = parse_result.ast,
                .file_path = full_path,
            });
            try result.map.put(allocator, decl_idx, idx);
        }
    }

    return result;
}

/// Generate Honey binding source from extracted C functions.
fn generateBindingSource(allocator: mem.Allocator, functions: []const c_parser.CFunction) ![]const u8 {
    var buf = try std.ArrayList(u8).initCapacity(allocator, 256);
    const writer = buf.writer(allocator);

    for (functions) |func| {
        // pub <name> :: c func(<params>) <return_type>
        try writer.print("pub {s} :: c func(", .{func.name});

        for (func.params, 0..) |param, i| {
            if (i > 0) try writer.writeAll(", ");
            try writer.print("{s}: {s}", .{ param.name, c_parser.typeToHoneyStr(param.c_type) });
        }

        try writer.print(") {s}\n", .{c_parser.typeToHoneyStr(func.return_type)});
    }

    return try buf.toOwnedSlice(allocator);
}

/// Write the generated binding file to zig-out/ for debugging/inspection.
fn writeBindingFile(allocator: mem.Allocator, import_path: []const u8, binding_source: []const u8) void {
    // Ensure zig-out directory exists
    fs.cwd().makeDir("zig-out") catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return,
    };

    // Write to zig-out/<basename>.hon
    const basename = std.fs.path.basename(import_path);
    const out_name = std.fmt.allocPrint(allocator, "zig-out/{s}.hon", .{basename}) catch return;
    const file = fs.cwd().createFile(out_name, .{}) catch return;
    defer file.close();
    file.writeAll(binding_source) catch {};
}
