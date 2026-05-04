const std = @import("std");
const mem = std.mem;

const Source = @import("../source/Source.zig");

warnings: Diagnostics,
errors: Diagnostics,
fatals: Diagnostics,

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
    source_id: Source.ID = .none,
    start: Source.Offset,
    end: Source.Offset,
};

pub const Diagnostic = struct {
    stage: Stage,
    severity: Severity,
    tag: Tag,
    span: ?Span = null,
};

const Diagnostics = std.MultiArrayList(DiagnosticData);

const DiagnosticData = struct {
    stage: Stage,
    tag: Tag,
    span: ?Span = null,
};

const TagInfo = struct {
    code: []const u8,
    message: []const u8,
    help: []const u8,
};

const tag_info = std.EnumArray(Tag, TagInfo).init(.{
    .lexer_unrecognized_character = .{ .code = "L001", .message = "unrecognized character", .help = "this byte is not part of Honey syntax" },
    .lexer_multiple_decimal_points = .{ .code = "L002", .message = "multiple decimal points", .help = "a floating-point literal may contain only one decimal point" },
    .lexer_empty_hex_literal = .{ .code = "L003", .message = "empty hexadecimal literal", .help = "add at least one hexadecimal digit after 0x" },
    .lexer_empty_bin_literal = .{ .code = "L004", .message = "empty binary literal", .help = "add at least one binary digit after 0b" },
    .lexer_unterminated_string_literal = .{ .code = "L005", .message = "unterminated string literal", .help = "close the string with a double quote before the line ends" },

    .parser_expected_expression = .{ .code = "P001", .message = "expected expression", .help = "insert an expression here" },
    .parser_expected_declaration = .{ .code = "P002", .message = "expected declaration", .help = "top-level items must be declarations or imports" },
    .parser_expected_token = .{ .code = "P003", .message = "expected token", .help = "the parser expected a different token here" },

    .lowering_unsupported_node = .{ .code = "H001", .message = "unsupported AST node in lowering", .help = "this syntax is parsed but cannot be lowered yet" },

    .sema_type_mismatch = .{ .code = "S001", .message = "type mismatch", .help = "make the value type match the expected type" },
    .sema_duplicate_declaration = .{ .code = "S002", .message = "duplicate declaration", .help = "rename one declaration or remove the duplicate" },
    .sema_declaration_cycle = .{ .code = "S003", .message = "declaration cycle", .help = "break the cycle between these declarations" },
    .sema_namespace_not_value = .{ .code = "S004", .message = "namespace is not a value", .help = "select a value from the namespace" },
    .sema_undefined_name = .{ .code = "S005", .message = "undefined name", .help = "declare this name before using it" },
    .sema_undefined_type = .{ .code = "S006", .message = "undefined type", .help = "use a built-in type or declare the type before using it" },
    .sema_type_not_value = .{ .code = "S007", .message = "type is not a value", .help = "use this name in a type position or refer to a value instead" },
    .sema_not_a_value = .{ .code = "S008", .message = "expected a value", .help = "replace this with an expression that produces a value" },
    .sema_return_outside_function = .{ .code = "S009", .message = "return outside function", .help = "move this return into a function body" },
    .sema_unsupported_string_literal = .{ .code = "S010", .message = "unsupported string literal", .help = "string literals are parsed but not supported semantically yet" },
    .sema_import_file_not_found = .{ .code = "S011", .message = "import file not found", .help = "check that the import path exists relative to the importing file" },
    .sema_import_cycle = .{ .code = "S012", .message = "import cycle", .help = "remove one import from the cycle" },
    .sema_import_invalid_namespace_name = .{ .code = "S013", .message = "invalid import namespace name", .help = "use an explicit alias that is a valid identifier" },
    .sema_unexpanded_import = .{ .code = "S014", .message = "import was not expanded", .help = "this import should have been resolved before semantic analysis" },
    .sema_expected_namespace = .{ .code = "S015", .message = "expected namespace", .help = "the left side of this qualified reference must be a namespace" },

    .optimizer_division_by_zero = .{ .code = "O001", .message = "division by zero", .help = "avoid dividing by a value known to be zero" },
});

pub fn init() Self {
    return .{
        .warnings = .{},
        .errors = .{},
        .fatals = .{},
    };
}

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    self.warnings.deinit(alloc);
    self.errors.deinit(alloc);
    self.fatals.deinit(alloc);
}

pub fn len(self: *const Self) usize {
    return self.warningCount() + self.errorCount() + self.fatalCount();
}

pub fn get(self: *const Self, idx: usize) Diagnostic {
    var remaining = idx;

    if (remaining < self.warnings.len)
        return expandDiagnostic(self.warnings.get(remaining), .warning);
    remaining -= self.warnings.len;

    if (remaining < self.errors.len)
        return expandDiagnostic(self.errors.get(remaining), .err);
    remaining -= self.errors.len;

    return expandDiagnostic(self.fatals.get(remaining), .fatal);
}

pub fn add(self: *Self, alloc: mem.Allocator, diagnostic: Diagnostic) !Ref {
    const data: DiagnosticData = .{
        .stage = diagnostic.stage,
        .tag = diagnostic.tag,
        .span = diagnostic.span,
    };

    return switch (diagnostic.severity) {
        .warning => blk: {
            const ref = makeRef(.warning, self.warnings.len);
            try self.warnings.append(alloc, data);
            break :blk ref;
        },
        .err => blk: {
            const ref = makeRef(.err, self.errors.len);
            try self.errors.append(alloc, data);
            break :blk ref;
        },
        .fatal => blk: {
            const ref = makeRef(.fatal, self.fatals.len);
            try self.fatals.append(alloc, data);
            break :blk ref;
        },
    };
}

pub fn hasErrors(self: *const Self) bool {
    return self.errorCount() > 0 or self.fatalCount() > 0;
}

pub fn hasProblems(self: *const Self) bool {
    return self.len() > 0;
}

pub fn hasWarnings(self: *const Self) bool {
    return self.warningCount() > 0;
}

pub fn errorCount(self: *const Self) usize {
    return self.errors.len;
}

pub fn fatalCount(self: *const Self) usize {
    return self.fatals.len;
}

pub fn warningCount(self: *const Self) usize {
    return self.warnings.len;
}

pub fn code(tag: Tag) []const u8 {
    return tag_info.get(tag).code;
}

pub fn message(tag: Tag) []const u8 {
    return tag_info.get(tag).message;
}

pub fn help(tag: Tag) []const u8 {
    return tag_info.get(tag).help;
}

pub fn render(self: *const Self, alloc: mem.Allocator, src: *const Source) ![]const u8 {
    const sources = [_]Source{src.*};
    return self.renderWithSources(alloc, &sources, src);
}

pub fn renderWithSources(self: *const Self, alloc: mem.Allocator, sources: []const Source, fallback_src: *const Source) ![]const u8 {
    var aw: std.Io.Writer.Allocating = .init(alloc);
    const w = &aw.writer;

    const source_resolver: SourceResolver = .{
        .sources = sources,
        .fallback = fallback_src,
    };

    var wrote_diagnostic = false;
    try writeDiagnostics(w, source_resolver, &self.warnings, .warning, &wrote_diagnostic);
    try writeDiagnostics(w, source_resolver, &self.errors, .err, &wrote_diagnostic);
    try writeDiagnostics(w, source_resolver, &self.fatals, .fatal, &wrote_diagnostic);

    try w.writeByte('\n');

    try writeSummary(w, self.fatalCount(), self.errorCount(), self.warningCount());

    return aw.toOwnedSlice();
}

const SourceResolver = struct {
    sources: []const Source,
    fallback: *const Source,

    fn get(self: SourceResolver, source_id: Source.ID) *const Source {
        if (source_id != .none) {
            const source_idx = source_id.toInt();
            if (source_idx < self.sources.len and self.sources[source_idx].id == source_id)
                return &self.sources[source_idx];

            for (self.sources) |*src| {
                if (src.id == source_id) return src;
            }
        }
        return self.fallback;
    }
};

fn writeDiagnostics(
    w: *std.Io.Writer,
    source_resolver: SourceResolver,
    diagnostics: *const Diagnostics,
    severity: Severity,
    wrote_diagnostic: *bool,
) !void {
    const tags = diagnostics.items(.tag);
    const spans = diagnostics.items(.span);

    for (0..diagnostics.len) |idx| {
        if (wrote_diagnostic.*) {
            try w.writeByte('\n');
        } else {
            wrote_diagnostic.* = true;
        }
        try writeDiagnostic(w, source_resolver, severity, tags[idx], spans[idx]);
    }
}

fn writeDiagnostic(
    w: *std.Io.Writer,
    source_resolver: SourceResolver,
    severity: Severity,
    tag: Tag,
    span: ?Span,
) !void {
    try w.print("{s}[{s}]: {s}\n", .{ severityName(severity), code(tag), message(tag) });

    if (span) |s| {
        const src = source_resolver.get(s.source_id);
        const lc = src.lineCol(s.start);
        const path = src.path orelse "<source>";
        try w.print("  --> {s}:{d}:{d}\n", .{ path, lc.line, lc.col });

        const bounds = lineBounds(src.contents, s.start);
        const trimmed_start = trimLineStart(src.contents, bounds.start, bounds.end);
        const line = src.contents[trimmed_start..bounds.end];
        const line_width = @max(digitCount(lc.line), 4);

        try printGutter(w, line_width, null);
        try w.writeByte('\n');

        try printGutter(w, line_width, lc.line);
        try w.print(" {s}\n", .{line});

        try printGutter(w, line_width, null);
        try w.writeByte(' ');

        const caret_indent = if (s.start >= trimmed_start) s.start - trimmed_start else 0;
        for (0..caret_indent) |_| try w.writeByte(' ');

        const max_end = @min(s.end, bounds.end);
        const caret_len = if (max_end > s.start) max_end - s.start else 1;
        for (0..caret_len) |_| try w.writeByte('^');

        try w.print(" {s}\n", .{help(tag)});
    } else {
        try w.print("      {s}\n", .{help(tag)});
    }
}

fn writeSummary(w: *std.Io.Writer, fatal_count: usize, err_count: usize, warn_count: usize) !void {
    if (fatal_count == 0 and err_count == 0 and warn_count == 0) return;

    try w.writeAll("Reported ");

    var wrote_count = false;

    if (fatal_count > 0) {
        try w.print("{d} fatal error{s}", .{ fatal_count, plural(fatal_count) });
        wrote_count = true;
    }

    if (err_count > 0) {
        if (wrote_count) try w.writeAll(", ");
        try w.print("{d} error{s}", .{ err_count, plural(err_count) });
        wrote_count = true;
    }

    if (warn_count > 0) {
        if (wrote_count) try w.writeAll(", ");
        try w.print("{d} warning{s}", .{ warn_count, plural(warn_count) });
    }

    try w.writeByte('\n');
}

fn makeRef(severity: Severity, idx: usize) Ref {
    const severity_bits: u32 = switch (severity) {
        .warning => 0,
        .err => 1,
        .fatal => 2,
    };
    const idx_bits: u32 = @intCast(idx);
    std.debug.assert(idx_bits <= 0x3fff_ffff);
    return @enumFromInt((severity_bits << 30) | idx_bits);
}

fn expandDiagnostic(data: DiagnosticData, severity: Severity) Diagnostic {
    return .{
        .stage = data.stage,
        .severity = severity,
        .tag = data.tag,
        .span = data.span,
    };
}

fn severityName(severity: Severity) []const u8 {
    return switch (severity) {
        .fatal => "fatal",
        .err => "error",
        .warning => "warning",
    };
}

fn lineBounds(contents: []const u8, offset: Source.Offset) struct { start: usize, end: usize } {
    const clamped: usize = @min(offset, contents.len);

    var start = clamped;
    while (start > 0 and contents[start - 1] != '\n') : (start -= 1) {}

    var end = clamped;
    while (end < contents.len and contents[end] != '\n') : (end += 1) {}

    return .{ .start = start, .end = end };
}

fn trimLineStart(contents: []const u8, start: usize, end: usize) usize {
    var trimmed = start;
    while (trimmed < end) : (trimmed += 1) {
        switch (contents[trimmed]) {
            ' ', '\t' => {},
            else => break,
        }
    }
    return trimmed;
}

fn printGutter(w: *std.Io.Writer, width: u32, line_num: ?u32) !void {
    if (line_num) |num| {
        const num_width = digitCount(num);
        for (0..(width - num_width)) |_| try w.writeByte(' ');
        try w.print("{d} |", .{num});
    } else {
        for (0..width) |_| try w.writeByte(' ');
        try w.writeAll(" |");
    }
}

fn digitCount(n: u32) u32 {
    if (n == 0) return 1;

    var count: u32 = 0;
    var num = n;
    while (num > 0) : (num /= 10) {
        count += 1;
    }
    return count;
}

fn plural(count: usize) []const u8 {
    return if (count == 1) "" else "s";
}

test "render expands span into source context" {
    const alloc = std.testing.allocator;

    var src = try Source.init.fromStr(alloc, "answer ::\n", Source.ID.fromInt(0));
    defer src.deinit(alloc);

    var diagnostics = init();
    defer diagnostics.deinit(alloc);

    _ = try diagnostics.add(alloc, .{
        .stage = .parser,
        .severity = .err,
        .tag = .parser_expected_expression,
        .span = .{ .source_id = src.id, .start = 8, .end = 8 },
    });

    const rendered = try diagnostics.render(alloc, &src);
    defer alloc.free(rendered);

    try std.testing.expect(mem.indexOf(u8, rendered, "error[P001]: expected expression") != null);
    try std.testing.expect(mem.indexOf(u8, rendered, "--> <source>:1:9") != null);
    try std.testing.expect(mem.indexOf(u8, rendered, "1 | answer ::") != null);
    try std.testing.expect(mem.indexOf(u8, rendered, "^ insert an expression here") != null);
    try std.testing.expect(mem.indexOf(u8, rendered, "Reported 1 error") != null);
}

test "render resolves spans against non-root sources" {
    const alloc = std.testing.allocator;

    var root_src = try Source.init.fromStr(alloc, "import \"lib.hon\"\n", Source.ID.fromInt(0));
    defer root_src.deinit(alloc);
    var lib_src = try Source.init.fromStr(alloc, "value :: missing\n", Source.ID.fromInt(1));
    defer lib_src.deinit(alloc);

    var diagnostics = init();
    defer diagnostics.deinit(alloc);

    _ = try diagnostics.add(alloc, .{
        .stage = .sema,
        .severity = .err,
        .tag = .sema_undefined_name,
        .span = .{ .source_id = lib_src.id, .start = 9, .end = 16 },
    });

    const sources = [_]Source{ root_src, lib_src };
    const rendered = try diagnostics.renderWithSources(alloc, &sources, &root_src);
    defer alloc.free(rendered);

    try std.testing.expect(mem.indexOf(u8, rendered, "--> <source>:1:10") != null);
    try std.testing.expect(mem.indexOf(u8, rendered, "1 | value :: missing") != null);
}

test "warnings count as problems but not hard errors" {
    const alloc = std.testing.allocator;

    var diagnostics = init();
    defer diagnostics.deinit(alloc);

    _ = try diagnostics.add(alloc, .{
        .stage = .optimizer,
        .severity = .warning,
        .tag = .optimizer_division_by_zero,
    });

    try std.testing.expect(!diagnostics.hasErrors());
    try std.testing.expect(diagnostics.hasProblems());
    try std.testing.expect(diagnostics.hasWarnings());
    try std.testing.expectEqual(@as(usize, 0), diagnostics.errorCount());
    try std.testing.expectEqual(@as(usize, 1), diagnostics.warningCount());
}

test "render places warnings before errors" {
    const alloc = std.testing.allocator;

    var src = try Source.init.fromStr(alloc, "x :: 1\n", Source.ID.fromInt(0));
    defer src.deinit(alloc);

    var diagnostics = init();
    defer diagnostics.deinit(alloc);

    _ = try diagnostics.add(alloc, .{
        .stage = .sema,
        .severity = .err,
        .tag = .sema_undefined_name,
    });
    _ = try diagnostics.add(alloc, .{
        .stage = .optimizer,
        .severity = .warning,
        .tag = .optimizer_division_by_zero,
    });

    const rendered = try diagnostics.render(alloc, &src);
    defer alloc.free(rendered);

    const warning_idx = mem.indexOf(u8, rendered, "warning[O001]") orelse return error.ExpectedWarning;
    const error_idx = mem.indexOf(u8, rendered, "error[S005]") orelse return error.ExpectedError;
    try std.testing.expect(warning_idx < error_idx);
}
