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
    /// Non-null when the type is a named struct (c_type is unused in that case).
    struct_type_name: ?[]const u8 = null,
    /// For pointer types: the pointee primitive type (null if pointee is a struct).
    pointee_type: ?CType = null,
    /// For pointer-to-struct: the pointee struct name.
    pointee_struct_name: ?[]const u8 = null,
};

pub const CFunction = struct {
    name: []const u8,
    return_type: CType,
    return_struct_name: ?[]const u8 = null,
    params: []const CParam,

    pub fn freeAll(functions: []const CFunction, allocator: mem.Allocator) void {
        for (functions) |func| {
            allocator.free(func.name);
            if (func.return_struct_name) |n| allocator.free(n);
            for (func.params) |param| {
                allocator.free(param.name);
                if (param.struct_type_name) |n| allocator.free(n);
                if (param.pointee_struct_name) |n| allocator.free(n);
            }
            allocator.free(func.params);
        }
        allocator.free(functions);
    }
};

pub const CStructField = struct {
    name: []const u8,
    c_type: CType,
    struct_type_name: ?[]const u8 = null,
    pointee_type: ?CType = null,
    pointee_struct_name: ?[]const u8 = null,
};

pub const CStruct = struct {
    name: []const u8,
    fields: []const CStructField,

    pub fn freeAll(structs: []const CStruct, allocator: mem.Allocator) void {
        for (structs) |s| {
            allocator.free(s.name);
            for (s.fields) |field| {
                allocator.free(field.name);
                if (field.struct_type_name) |n| allocator.free(n);
                if (field.pointee_struct_name) |n| allocator.free(n);
            }
            allocator.free(s.fields);
        }
        allocator.free(structs);
    }
};

pub const CParseResult = struct {
    functions: []const CFunction,
    structs: []const CStruct,
};

/// Parse C source and extract function signatures and struct definitions.
/// All returned name strings are duped and owned by the caller's allocator.
pub fn parse(allocator: mem.Allocator, source: []const u8) !CParseResult {
    // Strip comments first
    const stripped = try stripComments(allocator, source);
    defer allocator.free(stripped);

    // Evaluate preprocessor conditionals (#ifdef, #ifndef, #else, #endif)
    const preprocessed = try preprocessConditionals(allocator, stripped);
    defer allocator.free(preprocessed);

    var functions = try std.ArrayList(CFunction).initCapacity(allocator, 4);
    defer functions.deinit(allocator);

    var structs = try std.ArrayList(CStruct).initCapacity(allocator, 4);
    defer structs.deinit(allocator);

    // Track known struct/typedef names so they can be recognized as types in function signatures
    var known_structs = std.StringHashMap(void).init(allocator);
    defer known_structs.deinit();

    var pos: usize = 0;
    while (pos < preprocessed.len) {
        // Skip whitespace
        pos = skipWhitespace(preprocessed, pos);
        if (pos >= preprocessed.len) break;

        // Skip preprocessor lines
        if (preprocessed[pos] == '#') {
            pos = skipToNewline(preprocessed, pos);
            continue;
        }

        // Try to parse a struct definition first
        if (tryParseStruct(allocator, preprocessed, pos, &known_structs) catch null) |struct_result| {
            pos = struct_result.end_pos;
            const duped_name = try allocator.dupe(u8, struct_result.struc.name);
            var duped_fields = try allocator.alloc(CStructField, struct_result.struc.fields.len);
            for (struct_result.struc.fields, 0..) |field, i| {
                duped_fields[i] = .{
                    .name = try allocator.dupe(u8, field.name),
                    .c_type = field.c_type,
                    .struct_type_name = if (field.struct_type_name) |n| try allocator.dupe(u8, n) else null,
                    .pointee_type = field.pointee_type,
                    .pointee_struct_name = if (field.pointee_struct_name) |n| try allocator.dupe(u8, n) else null,
                };
            }
            allocator.free(struct_result.struc.fields);
            try structs.append(allocator, .{ .name = duped_name, .fields = duped_fields });
            continue;
        }

        // Try to parse a top-level function declaration
        const result = tryParseFunction(allocator, preprocessed, pos, &known_structs) catch {
            // Skip to next semicolon or closing brace at depth 0
            pos = skipToNextDecl(preprocessed, pos);
            continue;
        };

        if (result) |func_result| {
            pos = func_result.end_pos;

            // Skip static/empty-name functions (they use empty name as sentinel)
            if (func_result.func.name.len == 0) continue;

            // Dupe all name strings so they survive stripped buffer deallocation
            const duped_name = try allocator.dupe(u8, func_result.func.name);
            const duped_ret_struct = if (func_result.func.return_struct_name) |n| try allocator.dupe(u8, n) else null;
            var duped_params = try allocator.alloc(CParam, func_result.func.params.len);
            for (func_result.func.params, 0..) |param, i| {
                duped_params[i] = .{
                    .name = try allocator.dupe(u8, param.name),
                    .c_type = param.c_type,
                    .struct_type_name = if (param.struct_type_name) |n| try allocator.dupe(u8, n) else null,
                    .pointee_type = param.pointee_type,
                    .pointee_struct_name = if (param.pointee_struct_name) |n| try allocator.dupe(u8, n) else null,
                };
            }
            // Free the intermediate params slice from parseParams
            allocator.free(func_result.func.params);
            try functions.append(allocator, .{
                .name = duped_name,
                .return_type = func_result.func.return_type,
                .return_struct_name = duped_ret_struct,
                .params = duped_params,
            });
        } else {
            pos = skipToNextDecl(preprocessed, pos);
        }
    }

    return .{
        .functions = try allocator.dupe(CFunction, functions.items),
        .structs = try allocator.dupe(CStruct, structs.items),
    };
}

const FuncParseResult = struct {
    func: CFunction,
    end_pos: usize,
};

fn tryParseFunction(allocator: mem.Allocator, src: []const u8, start: usize, known_structs: *std.StringHashMap(void)) !?FuncParseResult {
    var pos = start;

    // Collect tokens before '('
    var tokens = try std.ArrayList([]const u8).initCapacity(allocator, 4);
    defer tokens.deinit(allocator);

    var is_static = false;
    var has_ret_pointer = false;

    while (pos < src.len) {
        pos = skipWhitespace(src, pos);
        if (pos >= src.len) return null;

        if (src[pos] == '(') break;
        if (src[pos] == ';' or src[pos] == '{' or src[pos] == '}' or src[pos] == '#') return null;

        if (src[pos] == '*') {
            has_ret_pointer = true;
            pos += 1;
            continue;
        }

        const tok = readToken(src, pos) orelse return null;

        // Check for keywords that indicate this is not a function
        if (mem.eql(u8, tok.text, "typedef") or
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

        // Skip qualifiers and GCC extensions
        if (mem.eql(u8, tok.text, "const") or
            mem.eql(u8, tok.text, "extern") or
            mem.eql(u8, tok.text, "inline") or
            mem.eql(u8, tok.text, "__inline") or
            mem.eql(u8, tok.text, "__inline__") or
            mem.eql(u8, tok.text, "__extension__") or
            mem.eql(u8, tok.text, "volatile") or
            mem.eql(u8, tok.text, "__volatile__") or
            mem.eql(u8, tok.text, "restrict") or
            mem.eql(u8, tok.text, "__restrict") or
            mem.eql(u8, tok.text, "_Nonnull") or
            mem.eql(u8, tok.text, "_Nullable"))
        {
            pos = tok.end;
            continue;
        }

        // Skip __attribute__((...)) blocks
        if (mem.eql(u8, tok.text, "__attribute__")) {
            pos = skipBalancedParens(src, tok.end);
            continue;
        }

        // Skip __asm__(...) blocks
        if (mem.eql(u8, tok.text, "__asm__") or mem.eql(u8, tok.text, "__asm") or mem.eql(u8, tok.text, "asm")) {
            pos = skipBalancedParens(src, tok.end);
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

    const ret_type_tokens = tokens.items[0 .. tokens.items.len - 1];

    // Resolve return type, handling struct types and pointers
    var return_type: CType = undefined;
    var return_struct_name: ?[]const u8 = null;

    if (has_ret_pointer) {
        return_type = .ptr;
    } else if (isStructType(ret_type_tokens, known_structs)) {
        return_struct_name = ret_type_tokens[ret_type_tokens.len - 1];
        return_type = .i32; // placeholder, unused when struct_name is set
    } else {
        return_type = mapReturnType(ret_type_tokens);
    }

    // Parse parameters
    pos += 1; // skip '('
    const params_result = try parseParams(allocator, src, pos, known_structs);

    pos = params_result.end_pos;
    if (pos < src.len and src[pos] == ')') pos += 1;

    // Skip __attribute__, __asm__ and other trailing annotations before ; or {
    pos = skipTrailingAnnotations(src, pos);

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
            .return_struct_name = return_struct_name,
            .params = params_result.params,
        },
        .end_pos = pos,
    };
}

fn parseParams(allocator: mem.Allocator, src: []const u8, start: usize, known_structs: *std.StringHashMap(void)) !struct { params: []const CParam, end_pos: usize } {
    var params = try std.ArrayList(CParam).initCapacity(allocator, 4);
    defer params.deinit(allocator);
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

            // Variadic `...` — skip and mark parameter as variadic
            if (pos + 2 < src.len and src[pos] == '.' and src[pos + 1] == '.' and src[pos + 2] == '.') {
                pos += 3;
                param_tokens.clearRetainingCapacity();
                break;
            }

            if (src[pos] == '*') {
                has_pointer = true;
                pos += 1;
                continue;
            }

            // Function pointer or grouped type — skip balanced parens
            if (src[pos] == '(') {
                pos = skipBalancedParens(src, pos);
                continue;
            }

            // Array parameter — skip balanced brackets
            if (src[pos] == '[') {
                pos = skipBalancedBrackets(src, pos);
                continue;
            }

            const tok = readToken(src, pos) orelse {
                pos += 1; // skip unknown character to avoid infinite loop
                continue;
            };

            // Skip qualifiers and GCC extensions
            if (mem.eql(u8, tok.text, "const") or
                mem.eql(u8, tok.text, "restrict") or
                mem.eql(u8, tok.text, "__restrict") or
                mem.eql(u8, tok.text, "_Nonnull") or
                mem.eql(u8, tok.text, "_Nullable") or
                mem.eql(u8, tok.text, "volatile") or
                mem.eql(u8, tok.text, "__volatile__") or
                mem.eql(u8, tok.text, "__extension__"))
            {
                pos = tok.end;
                continue;
            }

            // Skip __attribute__((...)) blocks
            if (mem.eql(u8, tok.text, "__attribute__")) {
                pos = skipBalancedParens(src, tok.end);
                continue;
            }

            try param_tokens.append(allocator, tok.text);
            pos = tok.end;
        }

        if (param_tokens.items.len >= 2) {
            const param_name = param_tokens.items[param_tokens.items.len - 1];
            const type_tokens = param_tokens.items[0 .. param_tokens.items.len - 1];
            try params.append(allocator, resolveParamType(param_name, type_tokens, has_pointer, known_structs));
        } else if (param_tokens.items.len == 1) {
            // Nameless parameter (e.g. `int` in `int abs(int);`) — use synthetic name
            const synth_names = [_][]const u8{ "_0", "_1", "_2", "_3", "_4", "_5", "_6", "_7" };
            const synth_name = if (params.items.len < synth_names.len) synth_names[params.items.len] else "_";
            if (has_pointer) {
                const pointee = mapReturnType(param_tokens.items[0..1]);
                try params.append(allocator, .{ .name = synth_name, .c_type = .ptr, .pointee_type = if (pointee != .ptr) pointee else null });
            } else {
                try params.append(allocator, .{ .name = synth_name, .c_type = mapReturnType(param_tokens.items[0..1]) });
            }
        }

        if (pos < src.len and src[pos] == ',') pos += 1;
    }

    return .{ .params = try allocator.dupe(CParam, params.items), .end_pos = pos };
}

/// Resolve a parameter's type info from its tokens, pointer flag, and known struct names.
fn resolveParamType(name: []const u8, type_tokens: []const []const u8, has_pointer: bool, known_structs: *std.StringHashMap(void)) CParam {
    if (has_pointer) {
        // Pointer type — figure out pointee
        if (isStructType(type_tokens, known_structs)) {
            return .{ .name = name, .c_type = .ptr, .pointee_struct_name = type_tokens[type_tokens.len - 1] };
        }
        const pointee = mapReturnType(type_tokens);
        return .{ .name = name, .c_type = .ptr, .pointee_type = if (pointee != .ptr) pointee else null };
    }
    if (isStructType(type_tokens, known_structs)) {
        return .{ .name = name, .c_type = .i32, .struct_type_name = type_tokens[type_tokens.len - 1] };
    }
    return .{ .name = name, .c_type = mapReturnType(type_tokens) };
}

/// Check if type tokens refer to a struct type:
/// - `struct TypeName` (first token is "struct")
/// - bare typedef name (single token in known_structs)
fn isStructType(type_tokens: []const []const u8, known_structs: *std.StringHashMap(void)) bool {
    if (type_tokens.len >= 2 and mem.eql(u8, type_tokens[0], "struct")) return true;
    if (type_tokens.len == 1 and known_structs.contains(type_tokens[0])) return true;
    return false;
}

// ── Struct parsing ──────────────────────────────────────────

const StructParseResult = struct {
    struc: CStruct,
    end_pos: usize,
};

/// Try to parse a C struct definition at `start`. Handles:
///   struct name { fields };
///   typedef struct { fields } name;
///   typedef struct name { fields } name;
fn tryParseStruct(allocator: mem.Allocator, src: []const u8, start: usize, known_structs: *std.StringHashMap(void)) !?StructParseResult {
    var pos = skipWhitespace(src, start);
    if (pos >= src.len) return null;

    var tok = readToken(src, pos) orelse return null;
    var is_typedef = false;

    if (mem.eql(u8, tok.text, "typedef")) {
        is_typedef = true;
        pos = skipWhitespace(src, tok.end);
        tok = readToken(src, pos) orelse return null;
    }

    if (!mem.eql(u8, tok.text, "struct")) return null;
    pos = skipWhitespace(src, tok.end);
    if (pos >= src.len) return null;

    // Read optional struct tag name (absent for anonymous typedef structs)
    var struct_tag: ?[]const u8 = null;
    if (src[pos] != '{') {
        const name_tok = readToken(src, pos) orelse return null;
        // Forward declaration `struct name;` — skip
        if (name_tok.text[0] == ';') return null;
        struct_tag = name_tok.text;
        pos = skipWhitespace(src, name_tok.end);
    }

    if (pos >= src.len or src[pos] != '{') return null;
    pos += 1; // skip '{'

    const fields_result = try parseStructFields(allocator, src, pos, known_structs);
    pos = fields_result.end_pos;

    pos = skipWhitespace(src, pos);
    if (pos >= src.len or src[pos] != '}') return null;
    pos += 1; // skip '}'
    pos = skipWhitespace(src, pos);

    var final_name: []const u8 = undefined;
    if (is_typedef) {
        // After '}', read the typedef alias name
        const alias_tok = readToken(src, pos) orelse return null;
        final_name = alias_tok.text;
        pos = alias_tok.end;
    } else {
        final_name = struct_tag orelse return null;
    }

    // Register so subsequent functions/structs can reference this type
    try known_structs.put(final_name, {});

    // Consume trailing semicolon
    pos = skipWhitespace(src, pos);
    if (pos < src.len and src[pos] == ';') pos += 1;

    return StructParseResult{
        .struc = .{ .name = final_name, .fields = fields_result.fields },
        .end_pos = pos,
    };
}

/// Parse struct fields between { and }. Fields are `type name;` separated by semicolons.
fn parseStructFields(allocator: mem.Allocator, src: []const u8, start: usize, known_structs: *std.StringHashMap(void)) !struct { fields: []const CStructField, end_pos: usize } {
    var fields = try std.ArrayList(CStructField).initCapacity(allocator, 4);
    defer fields.deinit(allocator);
    var pos = start;

    while (pos < src.len and src[pos] != '}') {
        pos = skipWhitespace(src, pos);
        if (pos >= src.len or src[pos] == '}') break;

        var field_tokens = try std.ArrayList([]const u8).initCapacity(allocator, 4);
        defer field_tokens.deinit(allocator);
        var has_pointer = false;

        while (pos < src.len and src[pos] != ';' and src[pos] != '}') {
            pos = skipWhitespace(src, pos);
            if (pos >= src.len or src[pos] == ';' or src[pos] == '}') break;

            if (src[pos] == '*') {
                has_pointer = true;
                pos += 1;
                continue;
            }

            // Function pointer field — skip balanced parens
            if (src[pos] == '(') {
                pos = skipBalancedParens(src, pos);
                continue;
            }

            // Array field — skip balanced brackets
            if (src[pos] == '[') {
                pos = skipBalancedBrackets(src, pos);
                continue;
            }

            const tok = readToken(src, pos) orelse {
                pos += 1;
                continue;
            };

            // Skip qualifiers and GCC extensions
            if (mem.eql(u8, tok.text, "const") or
                mem.eql(u8, tok.text, "restrict") or
                mem.eql(u8, tok.text, "__restrict") or
                mem.eql(u8, tok.text, "_Nonnull") or
                mem.eql(u8, tok.text, "_Nullable") or
                mem.eql(u8, tok.text, "volatile") or
                mem.eql(u8, tok.text, "__volatile__") or
                mem.eql(u8, tok.text, "__extension__") or
                mem.eql(u8, tok.text, "__attribute__"))
            {
                if (mem.eql(u8, tok.text, "__attribute__")) {
                    pos = skipBalancedParens(src, tok.end);
                } else {
                    pos = tok.end;
                }
                continue;
            }

            try field_tokens.append(allocator, tok.text);
            pos = tok.end;
        }

        if (pos < src.len and src[pos] == ';') pos += 1;

        if (field_tokens.items.len >= 2) {
            const field_name = field_tokens.items[field_tokens.items.len - 1];
            const type_tokens = field_tokens.items[0 .. field_tokens.items.len - 1];
            const param = resolveParamType(field_name, type_tokens, has_pointer, known_structs);
            try fields.append(allocator, .{
                .name = param.name,
                .c_type = param.c_type,
                .struct_type_name = param.struct_type_name,
                .pointee_type = param.pointee_type,
                .pointee_struct_name = param.pointee_struct_name,
            });
        }
    }

    return .{ .fields = try allocator.dupe(CStructField, fields.items), .end_pos = pos };
}

// ── Type mapping ────────────────────────────────────────────

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

/// Format a CParam/CStructField type as a Honey type string. Handles struct names and pointers.
pub fn formatHoneyType(allocator: mem.Allocator, c_type: CType, struct_type_name: ?[]const u8, pointee_type: ?CType, pointee_struct_name: ?[]const u8) ![]const u8 {
    if (struct_type_name) |name| return try allocator.dupe(u8, name);
    if (c_type == .ptr) {
        // char* → [:0]u8 (null-terminated string, Honey convention)
        if (pointee_type) |pt| {
            if (pt == .i8) return try allocator.dupe(u8, "[:0]u8");
        }
        if (pointee_struct_name) |name| return try std.fmt.allocPrint(allocator, "*mut {s}", .{name});
        if (pointee_type) |pt| return try std.fmt.allocPrint(allocator, "*mut {s}", .{typeToHoneyStr(pt)});
        return try allocator.dupe(u8, "*mut i8"); // fallback: void* → *mut i8
    }
    return try allocator.dupe(u8, typeToHoneyStr(c_type));
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

/// Skip balanced parentheses: advances past the first `(` and matches to its `)`.
fn skipBalancedParens(src: []const u8, pos: usize) usize {
    var p = skipWhitespace(src, pos);
    if (p >= src.len or src[p] != '(') return p;
    p += 1;
    var depth: usize = 1;
    while (p < src.len and depth > 0) : (p += 1) {
        if (src[p] == '(') depth += 1;
        if (src[p] == ')') depth -= 1;
    }
    return p;
}

/// Skip balanced brackets: advances past the first `[` and matches to its `]`.
fn skipBalancedBrackets(src: []const u8, pos: usize) usize {
    var p = pos;
    if (p >= src.len or src[p] != '[') return p;
    p += 1;
    var depth: usize = 1;
    while (p < src.len and depth > 0) : (p += 1) {
        if (src[p] == '[') depth += 1;
        if (src[p] == ']') depth -= 1;
    }
    return p;
}

/// Skip trailing __attribute__((...)), __asm__(...), etc. between ) and ; or {.
fn skipTrailingAnnotations(src: []const u8, pos: usize) usize {
    var p = skipWhitespace(src, pos);
    while (p < src.len) {
        if (src[p] == ';' or src[p] == '{') break;
        const tok = readToken(src, p) orelse break;
        if (mem.eql(u8, tok.text, "__attribute__") or
            mem.eql(u8, tok.text, "__asm__") or
            mem.eql(u8, tok.text, "__asm") or
            mem.eql(u8, tok.text, "asm"))
        {
            p = skipBalancedParens(src, tok.end);
            p = skipWhitespace(src, p);
            continue;
        }
        // Unknown annotation — skip the token
        p = tok.end;
        p = skipWhitespace(src, p);
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

/// Evaluate C preprocessor conditionals. Strips false branches and passes
/// through all lines in active branches (including #define lines).
fn preprocessConditionals(allocator: mem.Allocator, src: []const u8) ![]u8 {
    var defined = std.StringHashMap(void).init(allocator);
    defer defined.deinit();

    // Stack tracks whether we're in an active branch at each nesting level.
    // Each entry: .{ .active = is this branch active?, .seen_true = has any branch been true? }
    var stack = try std.ArrayList(struct { active: bool, seen_true: bool }).initCapacity(allocator, 4);
    defer stack.deinit(allocator);

    var result = try std.ArrayList(u8).initCapacity(allocator, src.len);
    const writer = result.writer(allocator);

    var line_start: usize = 0;
    while (line_start <= src.len) {
        const line_end = mem.indexOfScalarPos(u8, src, line_start, '\n') orelse src.len;
        const line = mem.trimLeft(u8, src[line_start..line_end], " \t");

        const all_active = for (stack.items) |entry| {
            if (!entry.active) break false;
        } else true;

        if (mem.startsWith(u8, line, "#ifdef ")) {
            const name = mem.trim(u8, line["#ifdef ".len..], " \t\r");
            const is_defined = defined.contains(name);
            try stack.append(allocator, .{ .active = all_active and is_defined, .seen_true = is_defined });
        } else if (mem.startsWith(u8, line, "#ifndef ")) {
            const name = mem.trim(u8, line["#ifndef ".len..], " \t\r");
            const is_defined = defined.contains(name);
            try stack.append(allocator, .{ .active = all_active and !is_defined, .seen_true = !is_defined });
        } else if (mem.startsWith(u8, line, "#else")) {
            if (stack.items.len > 0) {
                const top = &stack.items[stack.items.len - 1];
                // Check parent is active
                const parent_active = for (stack.items[0 .. stack.items.len - 1]) |entry| {
                    if (!entry.active) break false;
                } else true;
                top.active = parent_active and !top.seen_true;
                if (top.active) top.seen_true = true;
            }
        } else if (mem.startsWith(u8, line, "#endif")) {
            if (stack.items.len > 0) {
                _ = stack.pop();
            }
        } else if (all_active) {
            // Active branch — output the line
            if (mem.startsWith(u8, line, "#define ")) {
                // Track the defined name
                const after_define = mem.trimLeft(u8, line["#define ".len..], " \t");
                const name_end = mem.indexOfAny(u8, after_define, " \t\r") orelse after_define.len;
                if (name_end > 0) {
                    try defined.put(after_define[0..name_end], {});
                }
            }
            try writer.writeAll(src[line_start..line_end]);
            try writer.writeAll("\n");
        }

        if (line_end >= src.len) break;
        line_start = line_end + 1;
    }

    return try result.toOwnedSlice(allocator);
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

// ── Helpers for tests ───────────────────────────────────────

fn expectParseResult(src: []const u8) !CParseResult {
    return parse(std.testing.allocator, src);
}

fn freeResult(result: CParseResult) void {
    CFunction.freeAll(result.functions, std.testing.allocator);
    CStruct.freeAll(result.structs, std.testing.allocator);
}

// ── Function parsing tests ──────────────────────────────────

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
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 2), result.functions.len);
    try std.testing.expectEqualStrings("add", result.functions[0].name);
    try std.testing.expectEqual(CType.i32, result.functions[0].return_type);
    try std.testing.expectEqual(@as(usize, 2), result.functions[0].params.len);
    try std.testing.expectEqualStrings("sub", result.functions[1].name);
}

test "parse skips static functions" {
    const src =
        \\int add(int a, int b) { return a + b; }
        \\static int helper(int x) { return x * 2; }
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 1), result.functions.len);
    try std.testing.expectEqualStrings("add", result.functions[0].name);
}

test "parse void function with void params" {
    const src =
        \\void init(void) { }
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 1), result.functions.len);
    try std.testing.expectEqualStrings("init", result.functions[0].name);
    try std.testing.expectEqual(CType.void, result.functions[0].return_type);
    try std.testing.expectEqual(@as(usize, 0), result.functions[0].params.len);
}

test "parse with comments" {
    const src =
        \\// A helper function
        \\int add(int a, int b) {
        \\    /* sum */
        \\    return a + b;
        \\}
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 1), result.functions.len);
    try std.testing.expectEqualStrings("add", result.functions[0].name);
}

test "parse function declarations (no body)" {
    const src =
        \\int add(int a, int b);
        \\void exit(int code);
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 2), result.functions.len);
    try std.testing.expectEqualStrings("add", result.functions[0].name);
    try std.testing.expectEqualStrings("exit", result.functions[1].name);
    try std.testing.expectEqual(CType.void, result.functions[1].return_type);
}

test "parse fixed-width types" {
    const src =
        \\int32_t process(uint8_t tag, int64_t value) { return 0; }
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 1), result.functions.len);
    try std.testing.expectEqual(CType.i32, result.functions[0].return_type);
    try std.testing.expectEqual(@as(usize, 2), result.functions[0].params.len);
    try std.testing.expectEqual(CType.u8, result.functions[0].params[0].c_type);
    try std.testing.expectEqual(CType.i64, result.functions[0].params[1].c_type);
}

test "type to honey string" {
    try std.testing.expectEqualStrings("i32", typeToHoneyStr(.i32));
    try std.testing.expectEqualStrings("void", typeToHoneyStr(.void));
    try std.testing.expectEqualStrings("f64", typeToHoneyStr(.f64));
    try std.testing.expectEqualStrings("i64", typeToHoneyStr(.ptr));
}

// ── Struct parsing tests ────────────────────────────────────

test "parse named C struct" {
    const src = "struct vec2 { float x; float y; };";
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 0), result.functions.len);
    try std.testing.expectEqual(@as(usize, 1), result.structs.len);
    try std.testing.expectEqualStrings("vec2", result.structs[0].name);
    try std.testing.expectEqual(@as(usize, 2), result.structs[0].fields.len);
    try std.testing.expectEqualStrings("x", result.structs[0].fields[0].name);
    try std.testing.expectEqual(CType.f32, result.structs[0].fields[0].c_type);
    try std.testing.expectEqualStrings("y", result.structs[0].fields[1].name);
    try std.testing.expectEqual(CType.f32, result.structs[0].fields[1].c_type);
}

test "parse typedef anonymous struct" {
    const src = "typedef struct { int width; int height; } Size;";
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 1), result.structs.len);
    try std.testing.expectEqualStrings("Size", result.structs[0].name);
    try std.testing.expectEqual(@as(usize, 2), result.structs[0].fields.len);
    try std.testing.expectEqualStrings("width", result.structs[0].fields[0].name);
    try std.testing.expectEqual(CType.i32, result.structs[0].fields[0].c_type);
}

test "parse typedef named struct" {
    const src = "typedef struct vec2 { float x; float y; } vec2;";
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 1), result.structs.len);
    try std.testing.expectEqualStrings("vec2", result.structs[0].name);
}

test "parse struct with struct-typed field" {
    const src =
        \\struct vec2 { float x; float y; };
        \\struct rect { struct vec2 origin; struct vec2 size; };
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 2), result.structs.len);
    try std.testing.expectEqualStrings("rect", result.structs[1].name);
    try std.testing.expectEqualStrings("vec2", result.structs[1].fields[0].struct_type_name.?);
    try std.testing.expectEqualStrings("vec2", result.structs[1].fields[1].struct_type_name.?);
}

test "parse function with struct param" {
    const src =
        \\struct vec2 { float x; float y; };
        \\void draw(struct vec2 pos);
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 1), result.functions.len);
    try std.testing.expectEqualStrings("draw", result.functions[0].name);
    try std.testing.expectEqualStrings("vec2", result.functions[0].params[0].struct_type_name.?);
}

test "parse function with struct pointer param" {
    const src =
        \\struct vec2 { float x; float y; };
        \\void modify(struct vec2 *v);
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(CType.ptr, result.functions[0].params[0].c_type);
    try std.testing.expect(result.functions[0].params[0].struct_type_name == null);
    try std.testing.expectEqualStrings("vec2", result.functions[0].params[0].pointee_struct_name.?);
}

test "parse function returning struct" {
    const src =
        \\struct vec2 { float x; float y; };
        \\struct vec2 make_vec(float x, float y);
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 1), result.functions.len);
    try std.testing.expectEqualStrings("vec2", result.functions[0].return_struct_name.?);
}

test "parse function with typedef struct param" {
    const src =
        \\typedef struct { float x; float y; } Vec2;
        \\float length(Vec2 v);
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 1), result.functions.len);
    try std.testing.expectEqualStrings("Vec2", result.functions[0].params[0].struct_type_name.?);
}

test "parse struct and function together" {
    const src =
        \\struct vec2 { float x; float y; };
        \\void print_hello(int n);
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 1), result.structs.len);
    try std.testing.expectEqual(@as(usize, 1), result.functions.len);
    try std.testing.expectEqualStrings("vec2", result.structs[0].name);
    try std.testing.expectEqualStrings("print_hello", result.functions[0].name);
}

test "parse function with variadic params" {
    const src =
        \\int printf(const char *fmt, ...);
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 1), result.functions.len);
    try std.testing.expectEqualStrings("printf", result.functions[0].name);
    try std.testing.expectEqual(CType.i32, result.functions[0].return_type);
    // Only the non-variadic params are captured
    try std.testing.expectEqual(@as(usize, 1), result.functions[0].params.len);
    try std.testing.expectEqualStrings("fmt", result.functions[0].params[0].name);
    try std.testing.expectEqual(CType.ptr, result.functions[0].params[0].c_type);
}

test "parse function with __attribute__" {
    const src =
        \\int printf(const char * restrict, ...) __attribute__((__format__ (__printf__, 1, 2)));
        \\void exit(int status) __attribute__((__noreturn__));
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 2), result.functions.len);
    try std.testing.expectEqualStrings("printf", result.functions[0].name);
    try std.testing.expectEqualStrings("exit", result.functions[1].name);
}

test "parse function with __asm__" {
    const src =
        \\int getrlimit(int resource, void *rlim) __asm("_" "getrlimit");
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 1), result.functions.len);
    try std.testing.expectEqualStrings("getrlimit", result.functions[0].name);
}

test "parse skips function pointer declarations" {
    const src =
        \\void (*signal(int sig, void (*handler)(int)))(int);
        \\int normal_func(int a);
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    // signal is a function-pointer-returning function — should be skipped (tokens < 2)
    // normal_func should be parsed
    try std.testing.expectEqual(@as(usize, 1), result.functions.len);
    try std.testing.expectEqualStrings("normal_func", result.functions[0].name);
}

test "parse function with nameless params" {
    const src =
        \\int abs(int);
        \\long labs(long);
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 2), result.functions.len);
    try std.testing.expectEqualStrings("abs", result.functions[0].name);
    try std.testing.expectEqual(@as(usize, 1), result.functions[0].params.len);
    try std.testing.expectEqualStrings("_0", result.functions[0].params[0].name);
    try std.testing.expectEqual(CType.i32, result.functions[0].params[0].c_type);
}

test "parse function with restrict and qualifiers in params" {
    const src =
        \\int snprintf(char * restrict str, unsigned long size, const char * restrict fmt, ...);
    ;
    const result = try expectParseResult(src);
    defer freeResult(result);

    try std.testing.expectEqual(@as(usize, 1), result.functions.len);
    try std.testing.expectEqualStrings("snprintf", result.functions[0].name);
    // str, size, fmt (variadic ... is skipped)
    try std.testing.expectEqual(@as(usize, 3), result.functions[0].params.len);
}

test "formatHoneyType" {
    const alloc = std.testing.allocator;

    const t1 = try formatHoneyType(alloc, .i32, null, null, null);
    defer alloc.free(t1);
    try std.testing.expectEqualStrings("i32", t1);

    const t2 = try formatHoneyType(alloc, .i32, "vec2", null, null);
    defer alloc.free(t2);
    try std.testing.expectEqualStrings("vec2", t2);

    const t3 = try formatHoneyType(alloc, .ptr, null, .i32, null);
    defer alloc.free(t3);
    try std.testing.expectEqualStrings("*mut i32", t3);

    const t4 = try formatHoneyType(alloc, .ptr, null, null, "vec2");
    defer alloc.free(t4);
    try std.testing.expectEqualStrings("*mut vec2", t4);

    const t5 = try formatHoneyType(alloc, .ptr, null, null, null);
    defer alloc.free(t5);
    try std.testing.expectEqualStrings("*mut i8", t5);

    // char* → [:0]u8 (C string convention)
    const t6 = try formatHoneyType(alloc, .ptr, null, .i8, null);
    defer alloc.free(t6);
    try std.testing.expectEqualStrings("[:0]u8", t6);
}
