const std = @import("std");

const ansi = @import("../utils/ansi.zig");
const errors = @import("error.zig");
const ErrorList = errors.ErrorList;
const ErrorInfo = errors.ErrorInfo;
const SourceCode = @import("../source/source.zig").SourceCode;
const SourceIndex = @import("../source/source.zig").SourceIndex;

/// Print all diagnostics (errors and warnings) to stderr.
pub fn print(error_list: *const ErrorList, src: *const SourceCode, file_path: []const u8) void {
    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const writer = &stderr_writer.interface;

    // print warnings
    for (error_list.warnings.items) |warn| {
        writeDiagnostic(warn, src, file_path, writer) catch {
            std.fs.File.stderr().writeAll("error: failed to write diagnostic\n") catch {};
        };
    }

    // print errors
    for (error_list.errors.items) |err| {
        writeDiagnostic(err, src, file_path, writer) catch {
            std.fs.File.stderr().writeAll("error: failed to write diagnostic\n") catch {};
        };
    }

    // print status report
    const err_count = error_list.errorCount();
    const warn_count = error_list.warningCount();

    if (err_count > 0 or warn_count > 0) {
        writer.print("Reported ", .{}) catch {};

        if (err_count > 0) {
            writer.print("{s}{d} error{s}{s}", .{ ansi.red(), err_count, if (err_count > 1) "s" else "", ansi.reset() }) catch {};
        }

        if (err_count > 0 and warn_count > 0) {
            writer.print(", ", .{}) catch {};
        }

        if (warn_count > 0) {
            writer.print("{s}{d} warning{s}{s}", .{ ansi.yellow(), warn_count, if (warn_count > 1) "s" else "", ansi.reset() }) catch {};
        }

        writer.print("\n\n", .{}) catch {};
    }

    writer.flush() catch {};
}

fn writeDiagnostic(
    err: errors.SemanticError,
    src: *const SourceCode,
    file_path: []const u8,
    writer: *std.io.Writer,
) !void {
    const info = err.kind.info();

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

    // compute line number width for consistent gutter (minimum 4 digits)
    const line_width = @max(digitCount(line), 4);

    // print diagnostic
    const severity_str: []const u8 = switch (info.severity) {
        .warning => "warning",
        .fatal => "fatal",
        else => "error",
    };
    const severity_col: []const u8 = if (info.severity == .warning) ansi.yellow() else ansi.red();
    try writer.print("{s}{s}{s}[{s}]: {s}\n", .{ severity_col, severity_str, ansi.reset(), info.code, info.message });
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
