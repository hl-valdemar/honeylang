const std = @import("std");
const mem = std.mem;

pub const CType = enum {
    void,
    bool_type,
    i8,
    u8,
    i16,
    u16,
    i32,
    u32,
    i64,
    u64,
    f32,
    f64,
    ptr,
};

pub const CParam = struct {
    name: []const u8,
    c_type: CType,
};

pub const CFunction = struct {
    name: []const u8,
    return_type: CType,
    params: []const CParam,
};

/// Parse C source and extract non-static function signatures.
/// All returned name strings are duped and owned by the caller's allocator.
pub fn parse(allocator: mem.Allocator, source: []const u8) ![]CFunction {
    // Strip comments first
    const stripped = try stripComments(allocator, source);
    defer allocator.free(stripped);

    var functions = try std.ArrayList(CFunction).initCapacity(allocator, 4);

    var pos: usize = 0;
    while (pos < stripped.len) {
        // Skip whitespace
        pos = skipWhitespace(stripped, pos);
        if (pos >= stripped.len) break;

        // Skip preprocessor lines
        if (stripped[pos] == '#') {
            pos = skipToNewline(stripped, pos);
            continue;
        }

        // Try to parse a top-level declaration
        const result = tryParseFunction(allocator, stripped, pos) catch {
            // Skip to next semicolon or closing brace at depth 0
            pos = skipToNextDecl(stripped, pos);
            continue;
        };

        if (result) |func_result| {
            pos = func_result.end_pos;

            // Skip static/empty-name functions (they use empty name as sentinel)
            if (func_result.func.name.len == 0) continue;

            // Dupe all name strings so they survive stripped buffer deallocation
            const duped_name = try allocator.dupe(u8, func_result.func.name);
            var duped_params = try allocator.alloc(CParam, func_result.func.params.len);
            for (func_result.func.params, 0..) |param, i| {
                duped_params[i] = .{
                    .name = try allocator.dupe(u8, param.name),
                    .c_type = param.c_type,
                };
            }
            try functions.append(allocator, .{
                .name = duped_name,
                .return_type = func_result.func.return_type,
                .params = duped_params,
            });
        } else {
            pos = skipToNextDecl(stripped, pos);
        }
    }

    return try allocator.dupe(CFunction, functions.items);
}

const FuncParseResult = struct {
    func: CFunction,
    end_pos: usize,
};

fn tryParseFunction(allocator: mem.Allocator, src: []const u8, start: usize) !?FuncParseResult {
    var pos = start;

    // Collect tokens before '('
    var tokens = try std.ArrayList([]const u8).initCapacity(allocator, 4);
    defer tokens.deinit(allocator);

    var is_static = false;

    while (pos < src.len) {
        pos = skipWhitespace(src, pos);
        if (pos >= src.len) return null;

        if (src[pos] == '(') break;
        if (src[pos] == ';' or src[pos] == '{' or src[pos] == '}' or src[pos] == '#') return null;

        const tok = readToken(src, pos) orelse return null;

        // Check for keywords that indicate this is not a function
        if (mem.eql(u8, tok.text, "typedef") or
            mem.eql(u8, tok.text, "struct") or
            mem.eql(u8, tok.text, "enum") or
            mem.eql(u8, tok.text, "union"))
        {
            return null;
        }

        if (mem.eql(u8, tok.text, "static")) {
            is_static = true;
            pos = tok.end;
            continue;
        }

        try tokens.append(allocator, tok.text);
        pos = tok.end;
    }

    if (pos >= src.len or src[pos] != '(') return null;
    if (tokens.items.len < 2) return null; // Need at least return type + name

    // Skip static functions — advance past body but don't return a function
    if (is_static) {
        const skip_pos = skipPastBody(src, pos);
        return FuncParseResult{ .func = .{ .name = "", .return_type = .void, .params = &.{} }, .end_pos = skip_pos };
    }

    // Last token is the function name, everything before is the return type
    const func_name = tokens.items[tokens.items.len - 1];

    // Check if name starts with '*' (pointer return, name is after *)
    if (func_name[0] == '*') return null;

    const return_type = mapReturnType(tokens.items[0 .. tokens.items.len - 1]);

    // Parse parameters
    pos += 1; // skip '('
    const params_result = try parseParams(allocator, src, pos);

    pos = params_result.end_pos;
    if (pos < src.len and src[pos] == ')') pos += 1;

    // Skip body or semicolon
    pos = skipWhitespace(src, pos);
    if (pos < src.len) {
        if (src[pos] == '{') {
            pos = skipBracedBlock(src, pos);
        } else if (src[pos] == ';') {
            pos += 1;
        }
    }

    return FuncParseResult{
        .func = .{
            .name = func_name,
            .return_type = return_type,
            .params = params_result.params,
        },
        .end_pos = pos,
    };
}

fn parseParams(allocator: mem.Allocator, src: []const u8, start: usize) !struct { params: []const CParam, end_pos: usize } {
    var params = try std.ArrayList(CParam).initCapacity(allocator, 4);
    var pos = start;

    // Check for (void) or ()
    const check_pos = skipWhitespace(src, pos);
    if (check_pos < src.len and src[check_pos] == ')') {
        return .{ .params = &.{}, .end_pos = check_pos };
    }

    // Check for (void)
    const void_tok = readToken(src, check_pos);
    if (void_tok) |vt| {
        if (mem.eql(u8, vt.text, "void")) {
            const after_void = skipWhitespace(src, vt.end);
            if (after_void < src.len and src[after_void] == ')') {
                return .{ .params = &.{}, .end_pos = after_void };
            }
        }
    }

    while (pos < src.len and src[pos] != ')') {
        pos = skipWhitespace(src, pos);
        if (pos >= src.len or src[pos] == ')') break;

        // Collect tokens for this parameter (until ',' or ')')
        var param_tokens = try std.ArrayList([]const u8).initCapacity(allocator, 4);
        defer param_tokens.deinit(allocator);
        var has_pointer = false;

        while (pos < src.len and src[pos] != ',' and src[pos] != ')') {
            pos = skipWhitespace(src, pos);
            if (pos >= src.len or src[pos] == ',' or src[pos] == ')') break;

            if (src[pos] == '*') {
                has_pointer = true;
                pos += 1;
                continue;
            }

            const tok = readToken(src, pos) orelse break;
            try param_tokens.append(allocator, tok.text);
            pos = tok.end;
        }

        if (param_tokens.items.len >= 2) {
            const param_name = param_tokens.items[param_tokens.items.len - 1];
            const type_tokens = param_tokens.items[0 .. param_tokens.items.len - 1];
            const param_type = if (has_pointer) CType.ptr else mapReturnType(type_tokens);
            try params.append(allocator, .{ .name = param_name, .c_type = param_type });
        } else if (param_tokens.items.len == 1 and has_pointer) {
            // e.g., `void *` with no name — skip
        }

        if (pos < src.len and src[pos] == ',') pos += 1;
    }

    return .{ .params = try allocator.dupe(CParam, params.items), .end_pos = pos };
}

fn mapReturnType(tokens: []const []const u8) CType {
    if (tokens.len == 0) return .i32; // default: int

    // Check for pointer
    for (tokens) |tok| {
        if (tok[0] == '*') return .ptr;
    }

    // Single token types
    if (tokens.len == 1) {
        return mapSingleType(tokens[0]);
    }

    // Multi-token types
    if (tokens.len == 2) {
        const a = tokens[0];
        const b = tokens[1];

        if (mem.eql(u8, a, "unsigned")) {
            if (mem.eql(u8, b, "int")) return .u32;
            if (mem.eql(u8, b, "char")) return .u8;
            if (mem.eql(u8, b, "short")) return .u16;
            if (mem.eql(u8, b, "long")) return .u64;
        }
        if (mem.eql(u8, a, "signed")) {
            if (mem.eql(u8, b, "int")) return .i32;
            if (mem.eql(u8, b, "char")) return .i8;
            if (mem.eql(u8, b, "short")) return .i16;
            if (mem.eql(u8, b, "long")) return .i64;
        }
        if (mem.eql(u8, a, "long")) {
            if (mem.eql(u8, b, "int")) return .i64;
            if (mem.eql(u8, b, "long")) return .i64;
            if (mem.eql(u8, b, "double")) return .f64;
        }
    }

    if (tokens.len == 3) {
        const a = tokens[0];
        const b = tokens[1];
        const c = tokens[2];

        if (mem.eql(u8, a, "unsigned") and mem.eql(u8, b, "long") and mem.eql(u8, c, "long")) return .u64;
        if (mem.eql(u8, a, "long") and mem.eql(u8, b, "long") and mem.eql(u8, c, "int")) return .i64;
    }

    return .i32; // fallback
}

fn mapSingleType(tok: []const u8) CType {
    if (mem.eql(u8, tok, "void")) return .void;
    if (mem.eql(u8, tok, "int")) return .i32;
    if (mem.eql(u8, tok, "char")) return .i8;
    if (mem.eql(u8, tok, "short")) return .i16;
    if (mem.eql(u8, tok, "long")) return .i64;
    if (mem.eql(u8, tok, "float")) return .f32;
    if (mem.eql(u8, tok, "double")) return .f64;
    if (mem.eql(u8, tok, "unsigned")) return .u32;
    if (mem.eql(u8, tok, "bool")) return .bool_type;
    if (mem.eql(u8, tok, "_Bool")) return .bool_type;

    // Fixed-width types
    if (mem.eql(u8, tok, "int8_t")) return .i8;
    if (mem.eql(u8, tok, "int16_t")) return .i16;
    if (mem.eql(u8, tok, "int32_t")) return .i32;
    if (mem.eql(u8, tok, "int64_t")) return .i64;
    if (mem.eql(u8, tok, "uint8_t")) return .u8;
    if (mem.eql(u8, tok, "uint16_t")) return .u16;
    if (mem.eql(u8, tok, "uint32_t")) return .u32;
    if (mem.eql(u8, tok, "uint64_t")) return .u64;
    if (mem.eql(u8, tok, "size_t")) return .u64;

    return .i32; // fallback for unknown types
}

/// Map CType to Honey type string for binding generation.
pub fn typeToHoneyStr(t: CType) []const u8 {
    return switch (t) {
        .void => "void",
        .bool_type => "bool",
        .i8 => "i8",
        .u8 => "u8",
        .i16 => "i16",
        .u16 => "u16",
        .i32 => "i32",
        .u32 => "u32",
        .i64 => "i64",
        .u64 => "u64",
        .f32 => "f32",
        .f64 => "f64",
        .ptr => "i64",
    };
}

// ── Helpers ──────────────────────────────────────────────────

const Token = struct {
    text: []const u8,
    end: usize,
};

fn readToken(src: []const u8, pos: usize) ?Token {
    var p = pos;
    if (p >= src.len) return null;

    // Handle * as a single token
    if (src[p] == '*') {
        return .{ .text = src[p .. p + 1], .end = p + 1 };
    }

    // Identifier or keyword
    if (!isIdentStart(src[p])) return null;

    const start = p;
    while (p < src.len and isIdentCont(src[p])) : (p += 1) {}
    return .{ .text = src[start..p], .end = p };
}

fn isIdentStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isIdentCont(c: u8) bool {
    return isIdentStart(c) or (c >= '0' and c <= '9');
}

fn skipWhitespace(src: []const u8, pos: usize) usize {
    var p = pos;
    while (p < src.len and (src[p] == ' ' or src[p] == '\t' or src[p] == '\n' or src[p] == '\r')) : (p += 1) {}
    return p;
}

fn skipToNewline(src: []const u8, pos: usize) usize {
    var p = pos;
    while (p < src.len and src[p] != '\n') : (p += 1) {}
    if (p < src.len) p += 1; // skip the \n
    return p;
}

fn skipBracedBlock(src: []const u8, pos: usize) usize {
    if (pos >= src.len or src[pos] != '{') return pos;
    var p = pos + 1;
    var depth: usize = 1;
    while (p < src.len and depth > 0) : (p += 1) {
        if (src[p] == '{') depth += 1;
        if (src[p] == '}') depth -= 1;
    }
    return p;
}

fn skipPastBody(src: []const u8, pos: usize) usize {
    var p = pos;
    // Find the opening brace or semicolon
    while (p < src.len) : (p += 1) {
        if (src[p] == '{') return skipBracedBlock(src, p);
        if (src[p] == ';') return p + 1;
    }
    return p;
}

fn skipToNextDecl(src: []const u8, pos: usize) usize {
    var p = pos;
    var depth: usize = 0;
    while (p < src.len) : (p += 1) {
        if (src[p] == '{') depth += 1;
        if (src[p] == '}') {
            if (depth > 0) depth -= 1;
            if (depth == 0) return p + 1;
        }
        if (src[p] == ';' and depth == 0) return p + 1;
    }
    return p;
}

fn stripComments(allocator: mem.Allocator, src: []const u8) ![]u8 {
    var result = try allocator.alloc(u8, src.len);
    var i: usize = 0;
    var j: usize = 0;

    while (i < src.len) {
        if (i + 1 < src.len and src[i] == '/' and src[i + 1] == '/') {
            // Line comment: replace with spaces until newline
            while (i < src.len and src[i] != '\n') : (i += 1) {
                result[j] = ' ';
                j += 1;
            }
        } else if (i + 1 < src.len and src[i] == '/' and src[i + 1] == '*') {
            // Block comment: replace with spaces
            result[j] = ' ';
            j += 1;
            i += 2;
            result[j] = ' ';
            j += 1;
            while (i < src.len) {
                if (i + 1 < src.len and src[i] == '*' and src[i + 1] == '/') {
                    result[j] = ' ';
                    j += 1;
                    i += 1;
                    result[j] = ' ';
                    j += 1;
                    i += 1;
                    break;
                }
                result[j] = if (src[i] == '\n') '\n' else ' ';
                j += 1;
                i += 1;
            }
        } else {
            result[j] = src[i];
            j += 1;
            i += 1;
        }
    }

    // Shrink to actual size
    const final = try allocator.alloc(u8, j);
    @memcpy(final, result[0..j]);
    allocator.free(result);
    return final;
}

// ── Tests ──────────────────────────────────────────────────

test "parse simple C functions" {
    const src =
        \\int add(int a, int b) {
        \\    return a + b;
        \\}
        \\
        \\int sub(int a, int b) {
        \\    return a - b;
        \\}
    ;
    const functions = try parse(std.testing.allocator, src);
    defer std.testing.allocator.free(functions);

    try std.testing.expectEqual(@as(usize, 2), functions.len);
    try std.testing.expectEqualStrings("add", functions[0].name);
    try std.testing.expectEqual(CType.i32, functions[0].return_type);
    try std.testing.expectEqual(@as(usize, 2), functions[0].params.len);
    try std.testing.expectEqualStrings("sub", functions[1].name);
}

test "parse skips static functions" {
    const src =
        \\int add(int a, int b) { return a + b; }
        \\static int helper(int x) { return x * 2; }
    ;
    const functions = try parse(std.testing.allocator, src);
    defer std.testing.allocator.free(functions);

    try std.testing.expectEqual(@as(usize, 1), functions.len);
    try std.testing.expectEqualStrings("add", functions[0].name);
}

test "parse void function with void params" {
    const src =
        \\void init(void) { }
    ;
    const functions = try parse(std.testing.allocator, src);
    defer std.testing.allocator.free(functions);

    try std.testing.expectEqual(@as(usize, 1), functions.len);
    try std.testing.expectEqualStrings("init", functions[0].name);
    try std.testing.expectEqual(CType.void, functions[0].return_type);
    try std.testing.expectEqual(@as(usize, 0), functions[0].params.len);
}

test "parse with comments" {
    const src =
        \\// A helper function
        \\int add(int a, int b) {
        \\    /* sum */
        \\    return a + b;
        \\}
    ;
    const functions = try parse(std.testing.allocator, src);
    defer std.testing.allocator.free(functions);

    try std.testing.expectEqual(@as(usize, 1), functions.len);
    try std.testing.expectEqualStrings("add", functions[0].name);
}

test "parse function declarations (no body)" {
    const src =
        \\int add(int a, int b);
        \\void exit(int code);
    ;
    const functions = try parse(std.testing.allocator, src);
    defer std.testing.allocator.free(functions);

    try std.testing.expectEqual(@as(usize, 2), functions.len);
    try std.testing.expectEqualStrings("add", functions[0].name);
    try std.testing.expectEqualStrings("exit", functions[1].name);
    try std.testing.expectEqual(CType.void, functions[1].return_type);
}

test "parse fixed-width types" {
    const src =
        \\int32_t process(uint8_t tag, int64_t value) { return 0; }
    ;
    const functions = try parse(std.testing.allocator, src);
    defer std.testing.allocator.free(functions);

    try std.testing.expectEqual(@as(usize, 1), functions.len);
    try std.testing.expectEqual(CType.i32, functions[0].return_type);
    try std.testing.expectEqual(@as(usize, 2), functions[0].params.len);
    try std.testing.expectEqual(CType.u8, functions[0].params[0].c_type);
    try std.testing.expectEqual(CType.i64, functions[0].params[1].c_type);
}

test "type to honey string" {
    try std.testing.expectEqualStrings("i32", typeToHoneyStr(.i32));
    try std.testing.expectEqualStrings("void", typeToHoneyStr(.void));
    try std.testing.expectEqualStrings("f64", typeToHoneyStr(.f64));
    try std.testing.expectEqualStrings("i64", typeToHoneyStr(.ptr));
}
