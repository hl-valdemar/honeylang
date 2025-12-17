const std = @import("std");

const ErrorList = @import("error.zig").ErrorList;
const ErrorKind = @import("error.zig").SemanticErrorKind;
const SourceCode = @import("../source/source.zig").SourceCode;
const SourceIndex = @import("../source/source.zig").SourceIndex;

pub fn print(error_list: *const ErrorList, src: *const SourceCode) void {
    for (error_list.errors.items) |err| {
        var context_start: SourceIndex = 0;
        var context_end: SourceIndex = 0;

        var line: SourceIndex = 1;
        for (0..err.start) |i| {
            if (src.get(@intCast(i)) == '\n') {
                line += 1;
                context_start = @intCast(i + 1);
            }
        }

        for (err.end..err.end + 3) |i| {
            if (src.get(@intCast(i)) == '\n') {
                context_end = @intCast(i);
                break;
            }
        }

        context_end = @min(context_end, err.end + 3);

        const num_pointers = err.end - err.start;
        var pointers: [20]u8 = [_]u8{0} ** 20;
        for (0..num_pointers) |i| {
            pointers[i] = '^';
        }

        const context_width = err.end - context_start;
        var indent: [50]u8 = [_]u8{0} ** 50;
        for (0..context_width - num_pointers) |i| {
            indent[i] = ' ';
        }

        std.debug.print("error[{s}]: {s}\n", .{ fmtErrorCode(kindToCode(err.kind)), fmtErrorKind(err.kind) });
        std.debug.print("   |\n", .{});
        std.debug.print(" {d} | {s}\n", .{ line, src.getSlice(context_start, context_end) });
        std.debug.print("   | {s}{s} {s}\n", .{ indent, pointers, kindToHelperMsg(err.kind) });
    }
}

const ErrorCode = enum {
    e001,
    e002,
};

fn kindToCode(kind: ErrorKind) ErrorCode {
    return switch (kind) {
        .cannot_negate_unsigned => .e001,
        .arithmetic_op_requires_numeric => .e002,
        else => unreachable,
    };
}

fn codeToKind(err_code: ErrorCode) ErrorKind {
    return switch (err_code) {
        .e001 => .cannot_negate_unsigned,
        .e002 => .arithmetic_op_requires_numeric,
    };
}

fn fmtErrorKind(kind: ErrorKind) []const u8 {
    return switch (kind) {
        .cannot_negate_unsigned => "cannot negate unsigned integer",
        .arithmetic_op_requires_numeric => "arithmetic operation requires numeric type",
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
        else => "<[:unimplemented:]>",
    };
}
