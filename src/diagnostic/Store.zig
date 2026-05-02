const std = @import("std");
const mem = std.mem;

const Source = @import("../source/Source.zig");

arr: std.MultiArrayList(Diagnostic),

const Self = @This();

pub const Ref = enum(u32) {
    none = std.math.maxInt(u32),
    _,
};

pub const Stage = enum {
    lexer,
    parser,
    lowering,
    sema,
    optimizer,
};

pub const Severity = enum {
    fatal,
    err,
    warning,
};

pub const Tag = enum {
    lexer_unrecognized_character,
    lexer_multiple_decimal_points,
    lexer_empty_hex_literal,
    lexer_empty_bin_literal,
    lexer_unterminated_string_literal,

    parser_expected_expression,
    parser_expected_declaration,
    parser_expected_token,

    lowering_unsupported_node,

    sema_type_mismatch,
    sema_duplicate_declaration,
    sema_declaration_cycle,
    sema_namespace_not_value,
    sema_undefined_name,
    sema_undefined_type,
    sema_type_not_value,
    sema_not_a_value,
    sema_return_outside_function,
    sema_unsupported_string_literal,
    sema_import_file_not_found,
    sema_import_cycle,
    sema_import_invalid_namespace_name,
    sema_unexpanded_import,
    sema_expected_namespace,

    optimizer_division_by_zero,
};

pub const Span = struct {
    start: Source.Offset,
    end: Source.Offset,
};

pub const Diagnostic = struct {
    stage: Stage,
    severity: Severity,
    tag: Tag,
    span: ?Span = null,
};

const TagInfo = struct {
    code: []const u8,
    message: []const u8,
};

const tag_info = std.EnumArray(Tag, TagInfo).init(.{
    .lexer_unrecognized_character = .{ .code = "L001", .message = "unrecognized character" },
    .lexer_multiple_decimal_points = .{ .code = "L002", .message = "multiple decimal points" },
    .lexer_empty_hex_literal = .{ .code = "L003", .message = "empty hexadecimal literal" },
    .lexer_empty_bin_literal = .{ .code = "L004", .message = "empty binary literal" },
    .lexer_unterminated_string_literal = .{ .code = "L005", .message = "unterminated string literal" },

    .parser_expected_expression = .{ .code = "P001", .message = "expected expression" },
    .parser_expected_declaration = .{ .code = "P002", .message = "expected declaration" },
    .parser_expected_token = .{ .code = "P003", .message = "expected token" },

    .lowering_unsupported_node = .{ .code = "H001", .message = "unsupported AST node in lowering" },

    .sema_type_mismatch = .{ .code = "S001", .message = "type mismatch" },
    .sema_duplicate_declaration = .{ .code = "S002", .message = "duplicate declaration" },
    .sema_declaration_cycle = .{ .code = "S003", .message = "declaration cycle" },
    .sema_namespace_not_value = .{ .code = "S004", .message = "namespace is not a value" },
    .sema_undefined_name = .{ .code = "S005", .message = "undefined name" },
    .sema_undefined_type = .{ .code = "S006", .message = "undefined type" },
    .sema_type_not_value = .{ .code = "S007", .message = "type is not a value" },
    .sema_not_a_value = .{ .code = "S008", .message = "expected a value" },
    .sema_return_outside_function = .{ .code = "S009", .message = "return outside function" },
    .sema_unsupported_string_literal = .{ .code = "S010", .message = "unsupported string literal" },
    .sema_import_file_not_found = .{ .code = "S011", .message = "import file not found" },
    .sema_import_cycle = .{ .code = "S012", .message = "import cycle" },
    .sema_import_invalid_namespace_name = .{ .code = "S013", .message = "invalid import namespace name" },
    .sema_unexpanded_import = .{ .code = "S014", .message = "import was not expanded" },
    .sema_expected_namespace = .{ .code = "S015", .message = "expected namespace" },

    .optimizer_division_by_zero = .{ .code = "O001", .message = "division by zero" },
});

pub fn init() Self {
    return .{ .arr = .{} };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.arr.deinit(alloc);
}

pub fn len(self: *const Self) usize {
    return self.arr.len;
}

pub fn get(self: *const Self, idx: usize) Diagnostic {
    return self.arr.get(idx);
}

pub fn add(self: *Self, alloc: mem.Allocator, diagnostic: Diagnostic) !Ref {
    const ref: Ref = @enumFromInt(@as(u32, @intCast(self.arr.len)));
    try self.arr.append(alloc, diagnostic);
    return ref;
}

pub fn hasErrors(self: *const Self) bool {
    for (self.arr.items(.severity)) |severity| {
        switch (severity) {
            .fatal, .err, .warning => return true,
        }
    }
    return false;
}

pub fn code(tag: Tag) []const u8 {
    return tag_info.get(tag).code;
}

pub fn message(tag: Tag) []const u8 {
    return tag_info.get(tag).message;
}

pub fn render(self: *const Self, alloc: mem.Allocator, src: *const Source) ![]const u8 {
    var aw: std.Io.Writer.Allocating = .init(alloc);
    const w = &aw.writer;

    const stages = self.arr.items(.stage);
    const severities = self.arr.items(.severity);
    const tags = self.arr.items(.tag);
    const spans = self.arr.items(.span);

    for (0..self.arr.len) |idx| {
        try w.print("[D{d:0>3}] {s} {s}/{s}", .{
            idx,
            @tagName(severities[idx]),
            @tagName(stages[idx]),
            code(tags[idx]),
        });

        if (spans[idx]) |span| {
            const lc = src.lineCol(span.start);
            try w.print(" at {d}:{d}", .{ lc.line, lc.col });
        }

        try w.print(": {s}\n", .{message(tags[idx])});
    }

    return aw.toOwnedSlice();
}
