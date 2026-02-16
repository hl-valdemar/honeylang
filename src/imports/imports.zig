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
    sub_import_map: std.AutoHashMapUnmanaged(NodeIndex, usize),
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

    // Stack-based import chain for circular import detection.
    // Files are added before recursing and removed after, so diamond imports work.
    var import_chain = std.StringHashMap(void).init(allocator);
    defer import_chain.deinit();

    const abs_main = fs.cwd().realpathAlloc(allocator, main_file_path) catch main_file_path;
    try import_chain.put(abs_main, {});

    const main_dir = std.fs.path.dirname(main_file_path) orelse ".";
    var next_src_id: source.Id = 1;

    result.map = try resolveImportsInner(
        allocator,
        ast,
        tokens,
        main_src,
        main_dir,
        &result,
        &import_chain,
        &next_src_id,
    );

    return result;
}

/// Resolve imports for a single file's AST. Returns a map from that file's
/// import NodeIndex values to global indices in result.imports.
fn resolveImportsInner(
    allocator: mem.Allocator,
    ast: *const Ast,
    tokens: *const TokenList,
    file_src: *const SourceCode,
    file_dir: []const u8,
    result: *ResolvedImports,
    import_chain: *std.StringHashMap(void),
    next_src_id: *source.Id,
) !std.AutoHashMapUnmanaged(NodeIndex, usize) {
    var local_map = std.AutoHashMapUnmanaged(NodeIndex, usize){};

    const program = ast.getProgram(ast.root);
    const decls = ast.getExtra(program.declarations);

    for (decls) |decl_idx| {
        const kind = ast.getKind(decl_idx);

        if (kind == .c_import_block) {
            const block = ast.getCImportBlock(decl_idx);
            const include_indices = ast.getExtra(block.includes);
            const define_indices = ast.getExtra(block.defines);

            const name_tok = tokens.items[block.name_token];
            const ns_name = try allocator.dupe(u8, file_src.getSlice(name_tok.start, name_tok.start + name_tok.len));

            var combined = try std.ArrayList(u8).initCapacity(allocator, 1024);
            const c_writer = combined.writer(allocator);

            for (define_indices) |def_tok_idx| {
                const def_tok = tokens.items[def_tok_idx];
                const def_value = file_src.getSlice(def_tok.start, def_tok.start + def_tok.len);
                try c_writer.print("#define {s}\n", .{def_value});
            }

            var block_has_errors = false;
            for (include_indices) |inc_tok_idx| {
                const inc_tok = tokens.items[inc_tok_idx];
                const inc_path = file_src.getSlice(inc_tok.start, inc_tok.start + inc_tok.len);

                if (!mem.endsWith(u8, inc_path, ".h")) {
                    std.debug.print("error: import c include requires a .h header file, got: {s}\n", .{inc_path});
                    block_has_errors = true;
                    continue;
                }

                const inc_full_path = try std.fs.path.join(allocator, &.{ file_dir, inc_path });
                const file = fs.cwd().openFile(inc_full_path, .{ .mode = .read_only }) catch {
                    std.debug.print("error: cannot read C header file: {s}\n", .{inc_path});
                    block_has_errors = true;
                    continue;
                };
                defer file.close();
                const file_size = file.getEndPos() catch { block_has_errors = true; continue; };
                const header_buf = allocator.alloc(u8, file_size) catch { block_has_errors = true; continue; };
                _ = file.readAll(header_buf) catch { block_has_errors = true; continue; };
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

            const binding_src = source.fromStr(binding_source, next_src_id.*);
            next_src_id.* += 1;

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

            const global_idx = result.imports.items.len;
            try result.imports.append(allocator, .{
                .namespace_name = ns_name,
                .src = binding_src,
                .tokens = lex_result.tokens,
                .ast = parse_result.ast,
                .file_path = try std.fmt.allocPrint(allocator, "<c_import_block:{s}>", .{ns_name}),
                .sub_import_map = .{},
            });
            try local_map.put(allocator, decl_idx, global_idx);
            continue;
        }

        if (kind != .import_decl and kind != .c_include_decl) continue;

        const import_decl = ast.getImportDecl(decl_idx);
        const path_token = tokens.items[import_decl.path_token];
        const import_path = file_src.getSlice(path_token.start, path_token.start + path_token.len);

        const full_path = try std.fs.path.join(allocator, &.{ file_dir, import_path });

        const abs_path = fs.cwd().realpathAlloc(allocator, full_path) catch {
            std.debug.print("error: import file not found: {s}\n", .{import_path});
            result.has_errors = true;
            continue;
        };

        // Check for circular imports (file is currently being resolved up the call stack)
        if (import_chain.contains(abs_path)) {
            std.debug.print("error: circular import detected: {s}\n", .{import_path});
            result.has_errors = true;
            continue;
        }

        // Resolve namespace name: explicit name_token takes priority, otherwise derive from filename
        const ns_name = if (import_decl.name_token) |name_tok_idx| blk: {
            const name_tok = tokens.items[name_tok_idx];
            break :blk try allocator.dupe(u8, file_src.getSlice(name_tok.start, name_tok.start + name_tok.len));
        } else blk: {
            const basename = std.fs.path.basename(import_path);
            const stem = if (std.mem.indexOf(u8, basename, ".")) |dot_pos|
                basename[0..dot_pos]
            else
                basename;
            break :blk try allocator.dupe(u8, stem);
        };

        if (kind == .c_include_decl) {
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
                const file_size = file.getEndPos() catch { result.has_errors = true; continue; };
                const buf = allocator.alloc(u8, file_size) catch { result.has_errors = true; continue; };
                _ = file.readAll(buf) catch { result.has_errors = true; continue; };
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

            writeBindingFile(allocator, import_path, binding_source);

            const binding_src = source.fromStr(binding_source, next_src_id.*);
            next_src_id.* += 1;

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

            const global_idx = result.imports.items.len;
            try result.imports.append(allocator, .{
                .namespace_name = ns_name,
                .src = binding_src,
                .tokens = lex_result.tokens,
                .ast = parse_result.ast,
                .file_path = full_path,
                .sub_import_map = .{},
            });
            try local_map.put(allocator, decl_idx, global_idx);
        } else {
            // Native Honey import
            const imported_src = source.fromFile(allocator, full_path, next_src_id.*) catch {
                std.debug.print("error: cannot read import file: {s}\n", .{import_path});
                result.has_errors = true;
                continue;
            };
            next_src_id.* += 1;

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

            const global_idx = result.imports.items.len;
            try result.imports.append(allocator, .{
                .namespace_name = ns_name,
                .src = imported_src,
                .tokens = lex_result.tokens,
                .ast = parse_result.ast,
                .file_path = full_path,
                .sub_import_map = .{},
            });
            try local_map.put(allocator, decl_idx, global_idx);

            // Recursively resolve imports within this Honey file.
            // Copy AST/tokens/src to stack locals to survive ArrayList reallocation during recursion.
            const sub_ast = result.imports.items[global_idx].ast;
            const sub_tokens = result.imports.items[global_idx].tokens;
            const sub_src = result.imports.items[global_idx].src;
            const sub_dir = std.fs.path.dirname(full_path) orelse ".";

            try import_chain.put(abs_path, {});
            const sub_map = try resolveImportsInner(
                allocator,
                &sub_ast,
                &sub_tokens,
                &sub_src,
                sub_dir,
                result,
                import_chain,
                next_src_id,
            );
            _ = import_chain.remove(abs_path);

            result.imports.items[global_idx].sub_import_map = sub_map;
        }
    }

    return local_map;
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
