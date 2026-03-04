const std = @import("std");

const ansi = @import("../utils/ansi.zig");
const ErrorList = @import("error.zig").ErrorList;
const ParseError = @import("error.zig").ParseError;
const ParseErrorKind = @import("error.zig").ParseErrorKind;
const SourceCode = @import("../source/source.zig").SourceCode;
const SourceIndex = @import("../source/source.zig").SourceIndex;

const ErrorInfo = struct {
    code: []const u8,
    message: []const u8,
    help: []const u8,
};

/// Single source of truth for all error metadata.
/// Compile-time checked to ensure all error kinds are covered.
const error_info = std.EnumArray(ParseErrorKind, ErrorInfo).init(.{
    .unexpected_eof = .{
        .code = "P001",
        .message = "unexpected end of file",
        .help = "the file ended unexpectedly while parsing",
    },
    .unexpected_token = .{
        .code = "P002",
        .message = "unexpected token",
        .help = "this token was not expected here",
    },
    .unclosed_brace = .{
        .code = "P003",
        .message = "unclosed brace",
        .help = "missing closing '}'",
    },
    .unclosed_paren = .{
        .code = "P004",
        .message = "unclosed parenthesis",
        .help = "missing closing ')'",
    },
    .expected_identifier = .{
        .code = "P005",
        .message = "expected identifier",
        .help = "a name was expected here",
    },
    .expected_declaration = .{
        .code = "P006",
        .message = "expected declaration",
        .help = "expected a constant, variable, or function declaration",
    },
    .expected_type_annotation = .{
        .code = "P007",
        .message = "expected type annotation",
        .help = "a type name was expected after ':'",
    },
    .expected_expression = .{
        .code = "P008",
        .message = "expected expression",
        .help = "an expression was expected here",
    },
    .expected_operand = .{
        .code = "P009",
        .message = "expected operand",
        .help = "an operand was expected for this operator",
    },
    .expected_statement = .{
        .code = "P010",
        .message = "expected statement",
        .help = "a statement was expected here",
    },
    .expected_block = .{
        .code = "P011",
        .message = "expected block",
        .help = "a block '{ ... }' was expected",
    },
    .expected_colon = .{
        .code = "P012",
        .message = "expected ':'",
        .help = "a colon was expected here",
    },
    .expected_double_colon = .{
        .code = "P013",
        .message = "expected '::'",
        .help = "a double colon was expected for constant declaration",
    },
    .expected_equal = .{
        .code = "P014",
        .message = "expected '='",
        .help = "an equals sign was expected here",
    },
    .expected_left_paren = .{
        .code = "P015",
        .message = "expected '('",
        .help = "an opening parenthesis was expected",
    },
    .expected_right_paren = .{
        .code = "P016",
        .message = "expected ')'",
        .help = "a closing parenthesis was expected",
    },
    .expected_left_curly = .{
        .code = "P017",
        .message = "expected '{'",
        .help = "an opening brace was expected",
    },
    .expected_right_curly = .{
        .code = "P018",
        .message = "expected '}'",
        .help = "a closing brace was expected",
    },
    .expected_comma_or_close = .{
        .code = "P019",
        .message = "expected ',' or closing delimiter",
        .help = "expected comma to continue or delimiter to close",
    },
    .expected_string_literal = .{
        .code = "P020",
        .message = "expected string literal",
        .help = "a quoted string was expected (e.g. \"file.hon\")",
    },
});

fn getInfo(kind: ParseErrorKind) ErrorInfo {
    return error_info.get(kind);
}

/// Print errors to stderr with source context.
pub fn print(error_list: *const ErrorList, src: *const SourceCode, file_path: []const u8) void {
    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const writer = &stderr_writer.interface;

    for (error_list.errors.items) |err| {
        writeError(err, src, file_path, writer) catch {
            std.fs.File.stderr().writeAll("error: failed to write diagnostic\n") catch {};
        };
    }

    writer.flush() catch {};
}

fn writeError(
    err: ParseError,
    src: *const SourceCode,
    file_path: []const u8,
    writer: *std.io.Writer,
) !void {
    const info = getInfo(err.kind);

    // compute line and column
    var line: u32 = 1;
    var col: u32 = 1;
    var line_start: SourceIndex = 0;

    for (0..err.start) |i| {
        if (src.get(@intCast(i))) |c| {
            if (c == '\n') {
                line += 1;
                col = 1;
                line_start = @intCast(i + 1);
            } else {
                col += 1;
            }
        }
    }

    // find end of line containing err.start
    var line_end: SourceIndex = line_start;
    while (line_end < src.buffer.len) : (line_end += 1) {
        if (src.get(line_end)) |c| {
            if (c == '\n') break;
        }
    }

    // find first non-whitespace character on the line
    var trimmed_start: SourceIndex = line_start;
    while (trimmed_start < line_end) : (trimmed_start += 1) {
        if (src.get(trimmed_start)) |c| {
            if (c != ' ' and c != '\t') break;
        }
    }

    // compute line number width for consistent gutter
    const line_width = digitCount(line);

    // build message with token info if available
    var msg_buf: [128]u8 = undefined;
    const message = if (err.expected != null and err.found != null)
        std.fmt.bufPrint(&msg_buf, "{s} (expected {s}, found {s})", .{
            info.message,
            @tagName(err.expected.?),
            @tagName(err.found.?),
        }) catch info.message
    else if (err.found != null)
        std.fmt.bufPrint(&msg_buf, "{s} (found {s})", .{
            info.message,
            @tagName(err.found.?),
        }) catch info.message
    else
        info.message;

    // print diagnostic header
    try writer.print("error[{s}]: {s}\n", .{ info.code, message });
    try writer.print("  --> {s}:{d}:{d}\n", .{ file_path, line, col });

    // empty gutter line
    try printGutter(writer, line_width, null);
    try writer.print("\n", .{});

    // source line with line number (trimmed)
    try printGutter(writer, line_width, line);
    const source_line = src.getSlice(trimmed_start, line_end);
    try writer.print(" {s}\n", .{source_line});

    // pointer line
    try printGutter(writer, line_width, null);

    // adjust indent for trimmed whitespace
    const indent = if (err.start >= trimmed_start) err.start - trimmed_start else 0;
    const max_end = @min(err.end, line_end);
    const pointer_len = if (max_end > err.start) max_end - err.start else 1;

    try writer.writeByte(' ');
    for (0..indent) |_| try writer.writeByte(' ');
    { // red pointers
        _ = try writer.write(ansi.red());
        for (0..pointer_len) |_| try writer.writeByte('^');
        _ = try writer.write(ansi.reset());
    }
    try writer.print(" {s}{s}{s}\n\n", .{ ansi.red(), info.help, ansi.reset() });
}

fn printGutter(writer: *std.io.Writer, width: u32, line_num: ?u32) !void {
    if (line_num) |num| {
        // right-align the line number
        const num_width = digitCount(num);
        for (0..(width - num_width)) |_| try writer.writeByte(' ');
        try writer.print("{d} |", .{num});
    } else {
        // empty gutter
        for (0..width) |_| try writer.writeByte(' ');
        try writer.print(" |", .{});
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
