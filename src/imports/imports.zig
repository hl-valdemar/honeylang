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
                const resolved = resolveIncludes(allocator, inc_full_path) catch {
                    std.debug.print("error: cannot read C header file: {s}\n", .{inc_path});
                    block_has_errors = true;
                    continue;
                };
                try c_writer.writeAll(resolved);
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

            const define_constants = try collectDefineConstants(allocator, c_source);

            const binding_source = generateBindingSource(allocator, functions, define_constants) catch {
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

            const c_source = resolveIncludes(allocator, full_path) catch {
                std.debug.print("error: cannot read C header file: {s}\n", .{import_path});
                result.has_errors = true;
                continue;
            };

            const functions = c_parser.parse(allocator, c_source) catch {
                std.debug.print("error: failed to parse C header: {s}\n", .{import_path});
                result.has_errors = true;
                continue;
            };

            const define_constants = try collectDefineConstants(allocator, c_source);

            const binding_source = generateBindingSource(allocator, functions, define_constants) catch {
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

/// Read a C header file and recursively expand `#include "..."` directives.
/// System includes (`#include <...>`) are kept as-is (skipped by the c_parser).
fn resolveIncludes(allocator: mem.Allocator, file_path: []const u8) ![]const u8 {
    var visited = std.StringHashMap(void).init(allocator);
    defer visited.deinit();
    return resolveIncludesInner(allocator, file_path, &visited);
}

fn resolveIncludesInner(
    allocator: mem.Allocator,
    file_path: []const u8,
    visited: *std.StringHashMap(void),
) ![]const u8 {
    const abs_path = fs.cwd().realpathAlloc(allocator, file_path) catch file_path;
    if (visited.contains(abs_path)) return "";
    try visited.put(abs_path, {});

    const file = try fs.cwd().openFile(file_path, .{ .mode = .read_only });
    defer file.close();
    const file_size = try file.getEndPos();
    const content = try allocator.alloc(u8, file_size);
    _ = try file.readAll(content);

    const file_dir = std.fs.path.dirname(file_path) orelse ".";

    var result = try std.ArrayList(u8).initCapacity(allocator, content.len);
    const writer = result.writer(allocator);

    var line_start: usize = 0;
    while (line_start < content.len) {
        const line_end = mem.indexOfScalarPos(u8, content, line_start, '\n') orelse content.len;
        const line = mem.trimLeft(u8, content[line_start..line_end], " \t");

        if (mem.startsWith(u8, line, "#include \"")) {
            const path_start = mem.indexOf(u8, line, "\"").? + 1;
            if (mem.indexOfScalarPos(u8, line, path_start, '"')) |path_end| {
                const inc_path = line[path_start..path_end];
                const inc_full = try std.fs.path.join(allocator, &.{ file_dir, inc_path });
                const inc_content = resolveIncludesInner(allocator, inc_full, visited) catch "";
                try writer.writeAll(inc_content);
                try writer.writeAll("\n");
                line_start = line_end + 1;
                continue;
            }
        }

        // Keep the line as-is (including other #include <...>, #define, etc.)
        try writer.writeAll(content[line_start .. line_end + @as(usize, if (line_end < content.len) 1 else 0)]);
        line_start = line_end + 1;
    }

    return try result.toOwnedSlice(allocator);
}

/// Generate Honey binding source from extracted C functions and define constants.
fn generateBindingSource(allocator: mem.Allocator, functions: []const c_parser.CFunction, defines: []const CDefine) ![]const u8 {
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
    const result = try generateBindingSource(std.testing.allocator, &.{}, &.{
        .{ .name = "PI", .value = "3.14" },
        .{ .name = "MAX_SIZE", .value = "1024" },
    });
    defer std.testing.allocator.free(result);
    try std.testing.expect(mem.indexOf(u8, result, "pub PI: f32 :: 3.14") != null);
    try std.testing.expect(mem.indexOf(u8, result, "pub MAX_SIZE: i32 :: 1024") != null);
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
