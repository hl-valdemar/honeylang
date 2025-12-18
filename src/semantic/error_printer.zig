const std = @import("std");

const ErrorList = @import("error.zig").ErrorList;
const ErrorKind = @import("error.zig").SemanticErrorKind;
const SourceCode = @import("../source/source.zig").SourceCode;
const SourceIndex = @import("../source/source.zig").SourceIndex;

/// Print errors to stderr with default buffer.
pub fn print(error_list: *const ErrorList, src: *const SourceCode, file_path: []const u8) void {
    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    write(error_list, src, file_path, stderr) catch {
        // fallback to unbuffered write on error
        std.fs.File.stderr().writeAll("error: failed to write diagnostic\n") catch {};
    };

    stderr.flush() catch {};
}

/// Write errors to any std.Io.Writer.
pub fn write(
    error_list: *const ErrorList,
    src: *const SourceCode,
    file_path: []const u8,
    writer: *std.Io.Writer,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    for (error_list.errors.items) |err| {
        try writeError(err, src, file_path, alloc, writer);
    }
}

fn writeError(
    err: @import("error.zig").SemanticError,
    src: *const SourceCode,
    file_path: []const u8,
    alloc: std.mem.Allocator,
    writer: *std.Io.Writer,
) !void {
    var context_start: SourceIndex = 0;

    // compute line and column
    var line: SourceIndex = 1;
    var col: SourceIndex = 0;
    for (0..err.start) |i| {
        col += 1;
        if (src.get(@intCast(i)) == '\n') {
            col = 0;
            line += 1;
            context_start = @intCast(i + 1);
        }
    }

    // scan to end of line (or EOF)
    var context_end: SourceIndex = err.end;
    while (context_end < src.buffer.len) : (context_end += 1) {
        if (src.get(context_end) == '\n') break;
    }

    // generate pointers
    const num_pointers = err.end - err.start;
    const pointers = try alloc.alloc(u8, num_pointers);
    @memset(pointers, '^');

    // generate indent
    const indent_width = err.start - context_start;
    const indent = try alloc.alloc(u8, indent_width);
    @memset(indent, ' ');

    // write diagnostic
    try writer.print("error{any}: {s}\n", .{ kindToCode(err.kind), fmtErrorKind(err.kind) });
    try writer.print("  --> {s}:{d}:{d}\n", .{ file_path, line, col });
    try writer.print("   |\n", .{});
    try writer.print(" {d} | {s}\n", .{ line, src.getSlice(context_start, context_end) });
    try writer.print("   | {s}{s} {s}\n\n", .{ indent, pointers, kindToHelperMsg(err.kind) });
}

const ErrorCode = enum {
    e001,
    e002,
    e003,
};

fn kindToCode(kind: ErrorKind) ErrorCode {
    return switch (kind) {
        .cannot_negate_unsigned => .e001,
        .arithmetic_op_requires_numeric => .e002,
        .type_mismatch => .e003,
        else => unreachable,
    };
}

fn fmtErrorKind(kind: ErrorKind) []const u8 {
    return switch (kind) {
        .cannot_negate_unsigned => "cannot negate unsigned integer",
        .arithmetic_op_requires_numeric => "arithmetic operation requires numeric type",
        .type_mismatch => "",
        else => "<[:unimplemented:]>",
    };
}

fn fmtErrorCode(err_code: ErrorCode) []const u8 {
    return switch (err_code) {
        .e001 => "E001",
        .e002 => "E002",
    };
}

fn kindToHelperMsg(kind: ErrorKind) []const u8 {
    return switch (kind) {
        .cannot_negate_unsigned => "negation requires signed type",
        .arithmetic_op_requires_numeric => "operands must be numeric",
        else => "<[:unimplemented:]>",
    };
}
