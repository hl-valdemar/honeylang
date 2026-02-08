const std = @import("std");

const ansi = @import("../utils/ansi.zig");
const ErrorList = @import("error.zig").ErrorList;
const LexerError = @import("error.zig").LexerError;
const LexerErrorKind = @import("error.zig").LexerErrorKind;
const SourceCode = @import("../source/source.zig").SourceCode;
const SourceIndex = @import("../source/source.zig").SourceIndex;

const ErrorInfo = struct {
    code: []const u8,
    message: []const u8,
    help: []const u8,
};

/// Single source of truth for all error metadata.
/// Compile-time checked to ensure all error kinds are covered.
const error_info = std.EnumArray(LexerErrorKind, ErrorInfo).init(.{
    .unexpected_character = .{
        .code = "L001",
        .message = "unexpected character",
        .help = "this character is not recognized",
    },
    .invalid_character = .{
        .code = "L002",
        .message = "invalid character",
        .help = "this character is not valid in this context",
    },
    .invalid_number_literal = .{
        .code = "L003",
        .message = "invalid number literal",
        .help = "number format is not valid",
    },
    .multiple_decimal_points = .{
        .code = "L004",
        .message = "multiple decimal points in number",
        .help = "a number can only have one decimal point",
    },
    .unterminated_string = .{
        .code = "L005",
        .message = "unterminated string literal",
        .help = "string was never closed with a matching quote",
    },
    .invalid_escape_sequence = .{
        .code = "L006",
        .message = "invalid escape sequence",
        .help = "this escape sequence is not recognized",
    },
    .unexpected_eof = .{
        .code = "L007",
        .message = "unexpected end of file",
        .help = "the file ended unexpectedly",
    },
});

fn getInfo(kind: LexerErrorKind) ErrorInfo {
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
    err: LexerError,
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

    // build message with character info if available
    var msg_buf: [128]u8 = undefined;
    const message = if (err.character) |ch|
        if (std.ascii.isPrint(ch))
            std.fmt.bufPrint(&msg_buf, "{s} '{c}' (0x{X:0>2})", .{ info.message, ch, ch }) catch info.message
        else
            std.fmt.bufPrint(&msg_buf, "{s} (0x{X:0>2})", .{ info.message, ch }) catch info.message
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
