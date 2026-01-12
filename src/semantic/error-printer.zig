const std = @import("std");

const ErrorList = @import("error.zig").ErrorList;
const ErrorKind = @import("error.zig").SemanticErrorKind;
const SourceCode = @import("../source/source.zig").SourceCode;
const SourceIndex = @import("../source/source.zig").SourceIndex;

const ErrorInfo = struct {
    code: []const u8,
    message: []const u8,
    help: []const u8,
};

/// Single source of truth for all error metadata.
/// Compile-time checked to ensure all error kinds are covered.
const error_info = std.EnumArray(ErrorKind, ErrorInfo).init(.{
    .unknown_type = .{
        .code = "S001",
        .message = "unknown type",
        .help = "type not found",
    },
    .duplicate_symbol = .{
        .code = "S002",
        .message = "duplicate symbol",
        .help = "symbol already defined",
    },
    .undefined_symbol = .{
        .code = "S003",
        .message = "undefined symbol",
        .help = "symbol not found in scope",
    },
    .type_mismatch = .{
        .code = "S004",
        .message = "type mismatch",
        .help = "types are not compatible",
    },
    .invalid_operand_type = .{
        .code = "S005",
        .message = "invalid operand type",
        .help = "operand has wrong type",
    },
    .cannot_negate_unsigned = .{
        .code = "S006",
        .message = "cannot negate unsigned integer",
        .help = "negation requires signed type",
    },
    .logical_op_requires_bool = .{
        .code = "S007",
        .message = "logical operation requires boolean operands",
        .help = "operands must be bool",
    },
    .arithmetic_op_requires_numeric = .{
        .code = "S008",
        .message = "arithmetic operation requires numeric operands",
        .help = "operands must be numeric",
    },
    .comparison_requires_compatible = .{
        .code = "S009",
        .message = "comparison requires compatible types",
        .help = "operands must have the same type",
    },
    .argument_count_mismatch = .{
        .code = "S010",
        .message = "argument count mismatch",
        .help = "wrong number of arguments",
    },
    .argument_type_mismatch = .{
        .code = "S011",
        .message = "argument type mismatch",
        .help = "argument has wrong type",
    },
    .return_type_mismatch = .{
        .code = "S012",
        .message = "return type mismatch",
        .help = "returned value doesn't match function signature",
    },
    .assignment_to_immutable = .{
        .code = "S013",
        .message = "cannot assign to immutable variable",
        .help = "use 'mut' to make variable mutable",
    },
    .condition_not_bool = .{
        .code = "S014",
        .message = "condition must be boolean",
        .help = "expected bool expression",
    },
    .not_callable = .{
        .code = "S015",
        .message = "expression is not callable",
        .help = "only functions can be called",
    },
});

fn getInfo(kind: ErrorKind) ErrorInfo {
    return error_info.get(kind);
}

/// Print errors to stderr.
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
    err: @import("error.zig").SemanticError,
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

    // print diagnostic
    try writer.print("error[{s}]: {s}\n", .{ info.code, info.message });
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
    for (0..pointer_len) |_| try writer.writeByte('^');
    try writer.print(" {s}\n\n", .{info.help});
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
