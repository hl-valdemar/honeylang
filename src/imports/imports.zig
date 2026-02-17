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
    include_paths: []const []const u8,
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
        include_paths,
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
    include_paths: []const []const u8,
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

                try c_writer.print("#include \"{s}\"\n", .{inc_path});
            }

            // Preprocess all includes in one pass
            const combined_source = try combined.toOwnedSlice(allocator);
            const preprocessed = preprocessHeader(allocator, null, combined_source, file_dir, include_paths) catch {
                std.debug.print("error: failed to preprocess C headers in import block\n", .{});
                block_has_errors = true;
                result.has_errors = true;
                continue;
            };

            if (block_has_errors) {
                result.has_errors = true;
                continue;
            }

            const c_parsed = c_parser.parse(allocator, preprocessed) catch {
                std.debug.print("error: failed to parse C headers in import block\n", .{});
                result.has_errors = true;
                continue;
            };

            const define_constants = try collectDefineConstants(allocator, preprocessed);

            const binding_source = generateBindingSource(allocator, c_parsed.functions, c_parsed.structs, define_constants) catch {
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

            // Generate #include directive and let zig cc -E resolve the path
            // (searches source_dir first via -I, then system include paths)
            const include_directive = try std.fmt.allocPrint(allocator, "#include \"{s}\"\n", .{import_path});
            const c_source = preprocessHeader(allocator, null, include_directive, file_dir, include_paths) catch {
                std.debug.print("error: failed to preprocess C header: {s}\n", .{import_path});
                result.has_errors = true;
                continue;
            };

            const c_parsed = c_parser.parse(allocator, c_source) catch {
                std.debug.print("error: failed to parse C header: {s}\n", .{import_path});
                result.has_errors = true;
                continue;
            };

            const define_constants = try collectDefineConstants(allocator, c_source);

            const binding_source = generateBindingSource(allocator, c_parsed.functions, c_parsed.structs, define_constants) catch {
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
            // Native Honey import — resolve path and check for circular imports
            const abs_path = fs.cwd().realpathAlloc(allocator, full_path) catch {
                std.debug.print("error: import file not found: {s}\n", .{import_path});
                result.has_errors = true;
                continue;
            };

            if (import_chain.contains(abs_path)) {
                std.debug.print("error: circular import detected: {s}\n", .{import_path});
                result.has_errors = true;
                continue;
            }

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
                include_paths,
            );
            _ = import_chain.remove(abs_path);

            result.imports.items[global_idx].sub_import_map = sub_map;
        }
    }

    return local_map;
}

const CDefine = struct {
    name: []const u8,
    value: []const u8,
};

/// Check if a string is a simple numeric literal (integer or float, optionally negative).
fn isNumericLiteral(s: []const u8) bool {
    if (s.len == 0) return false;
    var i: usize = 0;
    // optional leading minus
    if (s[i] == '-') {
        i += 1;
        if (i >= s.len) return false;
    }
    var has_digit = false;
    var has_dot = false;
    while (i < s.len) : (i += 1) {
        if (s[i] >= '0' and s[i] <= '9') {
            has_digit = true;
        } else if (s[i] == '.' and !has_dot) {
            has_dot = true;
        } else {
            return false;
        }
    }
    return has_digit;
}

/// Scan C source text for `#define NAME VALUE` lines and collect numeric constants.
fn collectDefineConstants(
    allocator: mem.Allocator,
    c_source: []const u8,
) ![]const CDefine {
    var defines = try std.ArrayList(CDefine).initCapacity(allocator, 8);

    var line_start: usize = 0;
    while (line_start < c_source.len) {
        // Find end of line
        const line_end = mem.indexOfScalarPos(u8, c_source, line_start, '\n') orelse c_source.len;
        const line = mem.trim(u8, c_source[line_start..line_end], " \t\r");

        if (mem.startsWith(u8, line, "#define ")) {
            const after_define = mem.trimLeft(u8, line["#define ".len..], " \t");
            // Split at first whitespace: NAME VALUE
            if (mem.indexOfAny(u8, after_define, " \t")) |space_idx| {
                const name = after_define[0..space_idx];
                const value = mem.trim(u8, after_define[space_idx + 1 ..], " \t");
                if (value.len > 0 and isNumericLiteral(value)) {
                    try defines.append(allocator, .{ .name = name, .value = value });
                }
            }
        }

        line_start = line_end + 1;
    }

    return try defines.toOwnedSlice(allocator);
}

/// Preprocess a C header using `zig cc -E` (Zig's bundled clang).
/// Either `file_path` (for a single header file) or `source_text` (for inline
/// C source, e.g. from a c_import_block) must be provided.
/// Strips `# <linenum> "file" ...` line markers from the output.
fn preprocessHeader(
    allocator: mem.Allocator,
    file_path: ?[]const u8,
    source_text: ?[]const u8,
    source_dir: []const u8,
    extra_include_paths: []const []const u8,
) ![]const u8 {
    var argv = try std.ArrayList([]const u8).initCapacity(allocator, 16);
    defer argv.deinit(allocator);

    try argv.append(allocator, "zig");
    try argv.append(allocator, "cc");
    try argv.append(allocator, "-E");

    // Add source directory as first include path so local headers are found
    try argv.append(allocator, "-I");
    try argv.append(allocator, source_dir);

    // Add extra include paths
    for (extra_include_paths) |path| {
        try argv.append(allocator, "-I");
        try argv.append(allocator, path);
    }

    if (file_path) |fp| {
        try argv.append(allocator, fp);
    } else if (source_text != null) {
        // Write source text to a temp file (Child.run doesn't support stdin piping)
        const tmp_path = "zig-out/_honey_pp_tmp.h";
        const tmp_file = fs.cwd().createFile(tmp_path, .{}) catch return error.PreprocessFailed;
        tmp_file.writeAll(source_text.?) catch {
            tmp_file.close();
            return error.PreprocessFailed;
        };
        tmp_file.close();
        try argv.append(allocator, tmp_path);
    } else {
        return error.PreprocessFailed;
    }

    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv.items,
        .max_output_bytes = 10 * 1024 * 1024, // 10 MB — system headers can be large
    }) catch return error.PreprocessFailed;
    defer allocator.free(result.stderr);

    if (result.term.Exited != 0) {
        if (result.stderr.len > 0) {
            std.debug.print("{s}", .{result.stderr});
        }
        allocator.free(result.stdout);
        return error.PreprocessFailed;
    }

    const raw_output = result.stdout;

    // Strip # line markers (lines starting with "# <digit>")
    return stripLineMarkers(allocator, raw_output);
}

/// Remove `# <linenum> "file" ...` line markers from preprocessor output.
fn stripLineMarkers(allocator: mem.Allocator, input: []const u8) ![]const u8 {
    var result = try std.ArrayList(u8).initCapacity(allocator, input.len);
    const writer = result.writer(allocator);

    var line_start: usize = 0;
    while (line_start < input.len) {
        const line_end = mem.indexOfScalarPos(u8, input, line_start, '\n') orelse input.len;
        const line = input[line_start..line_end];

        // Skip lines like: # 123 "/path/to/file.h" 3 4
        const is_line_marker = line.len >= 2 and line[0] == '#' and (line[1] == ' ' and line.len > 2 and line[2] >= '0' and line[2] <= '9');

        if (!is_line_marker) {
            try writer.writeAll(line);
            try writer.writeByte('\n');
        }

        line_start = line_end + 1;
    }

    return try result.toOwnedSlice(allocator);
}

/// Generate Honey binding source from extracted C functions, structs, and define constants.
fn generateBindingSource(
    allocator: mem.Allocator,
    functions: []const c_parser.CFunction,
    structs: []const c_parser.CStruct,
    defines: []const CDefine,
) ![]const u8 {
    var buf = try std.ArrayList(u8).initCapacity(allocator, 256);
    const writer = buf.writer(allocator);

    // Emit struct declarations first (functions may reference them)
    for (structs) |s| {
        try writer.print("pub {s} :: c struct {{ ", .{s.name});
        for (s.fields) |field| {
            const type_str = try c_parser.formatHoneyType(allocator, field.c_type, field.struct_type_name, field.pointee_type, field.pointee_struct_name);
            defer allocator.free(type_str);
            try writer.print("{s}: {s}, ", .{ field.name, type_str });
        }
        try writer.writeAll("}\n");
    }

    for (functions) |func| {
        // pub <name> :: c func(<params>) <return_type>
        try writer.print("pub {s} :: c func(", .{func.name});

        for (func.params, 0..) |param, i| {
            if (i > 0) try writer.writeAll(", ");
            const type_str = try c_parser.formatHoneyType(allocator, param.c_type, param.struct_type_name, param.pointee_type, param.pointee_struct_name);
            defer allocator.free(type_str);
            try writer.print("{s}: {s}", .{ param.name, type_str });
        }

        const ret_str = try c_parser.formatHoneyType(allocator, func.return_type, func.return_struct_name, null, null);
        defer allocator.free(ret_str);
        try writer.print(") {s}\n", .{ret_str});
    }

    for (defines) |def| {
        // pub <NAME>: <type> :: <VALUE>
        const is_float = mem.indexOfScalar(u8, def.value, '.') != null;
        const type_str: []const u8 = if (is_float) blk: {
            _ = std.fmt.parseFloat(f32, def.value) catch break :blk "f64";
            break :blk "f32";
        } else blk: {
            _ = std.fmt.parseInt(i32, def.value, 10) catch break :blk "i64";
            break :blk "i32";
        };
        try writer.print("pub {s}: {s} :: {s}\n", .{ def.name, type_str, def.value });
    }

    return try buf.toOwnedSlice(allocator);
}

test "isNumericLiteral accepts integers" {
    try std.testing.expect(isNumericLiteral("0"));
    try std.testing.expect(isNumericLiteral("42"));
    try std.testing.expect(isNumericLiteral("1024"));
}

test "isNumericLiteral accepts floats" {
    try std.testing.expect(isNumericLiteral("3.14"));
    try std.testing.expect(isNumericLiteral("0.5"));
    try std.testing.expect(isNumericLiteral("100.0"));
}

test "isNumericLiteral accepts negative numbers" {
    try std.testing.expect(isNumericLiteral("-1"));
    try std.testing.expect(isNumericLiteral("-3.14"));
}

test "isNumericLiteral rejects non-numeric" {
    try std.testing.expect(!isNumericLiteral(""));
    try std.testing.expect(!isNumericLiteral("abc"));
    try std.testing.expect(!isNumericLiteral("3.14.15"));
    try std.testing.expect(!isNumericLiteral("-"));
    try std.testing.expect(!isNumericLiteral("(1 + 2)"));
    try std.testing.expect(!isNumericLiteral("0xFF"));
}

test "collectDefineConstants skips valueless defines" {
    const defines = try collectDefineConstants(std.testing.allocator, "#define DEBUG\n");
    defer std.testing.allocator.free(defines);
    try std.testing.expectEqual(@as(usize, 0), defines.len);
}

test "collectDefineConstants extracts numeric defines from source" {
    const c_source =
        \\#define PI 3.14
        \\#define MAX_SIZE 1024
        \\#define DEBUG
        \\#define VERSION hello
        \\int add(int a, int b);
    ;
    const defines = try collectDefineConstants(std.testing.allocator, c_source);
    defer std.testing.allocator.free(defines);
    try std.testing.expectEqual(@as(usize, 2), defines.len);
    try std.testing.expectEqualStrings("PI", defines[0].name);
    try std.testing.expectEqualStrings("3.14", defines[0].value);
    try std.testing.expectEqualStrings("MAX_SIZE", defines[1].name);
    try std.testing.expectEqualStrings("1024", defines[1].value);
}

test "generateBindingSource includes define constants" {
    const result = try generateBindingSource(std.testing.allocator, &.{}, &.{}, &.{
        .{ .name = "PI", .value = "3.14" },
        .{ .name = "MAX_SIZE", .value = "1024" },
    });
    defer std.testing.allocator.free(result);
    try std.testing.expect(mem.indexOf(u8, result, "pub PI: f32 :: 3.14") != null);
    try std.testing.expect(mem.indexOf(u8, result, "pub MAX_SIZE: i32 :: 1024") != null);
}

test "generateBindingSource includes struct declarations" {
    const result = try generateBindingSource(std.testing.allocator, &.{}, &.{
        .{ .name = "vec2", .fields = &.{
            .{ .name = "x", .c_type = .f32 },
            .{ .name = "y", .c_type = .f32 },
        } },
    }, &.{});
    defer std.testing.allocator.free(result);
    try std.testing.expect(mem.indexOf(u8, result, "pub vec2 :: c struct { x: f32, y: f32, }") != null);
}

test "generateBindingSource structs with pointer fields" {
    const result = try generateBindingSource(std.testing.allocator, &.{}, &.{
        .{ .name = "node", .fields = &.{
            .{ .name = "value", .c_type = .i32 },
            .{ .name = "next", .c_type = .ptr, .pointee_struct_name = "node" },
        } },
    }, &.{});
    defer std.testing.allocator.free(result);
    try std.testing.expect(mem.indexOf(u8, result, "next: *mut node") != null);
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
