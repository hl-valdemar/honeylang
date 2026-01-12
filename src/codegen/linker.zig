const std = @import("std");
const mem = std.mem;
const fs = std.fs;

pub const LinkerError = error{
    AssemblerFailed,
    LinkerFailed,
    SdkPathNotFound,
    IoError,
    OutOfMemory,
};

pub const LinkerResult = struct {
    executable_path: []const u8,
};

/// Assembles and links the generated assembly into an executable.
/// NOTE: Currently only supports macOS ARM64.
pub fn link(
    allocator: mem.Allocator,
    assembly: []const u8,
    output_name: []const u8,
) LinkerError!LinkerResult {
    const output_dir = "zig-out";

    // ensure output directory exists
    fs.cwd().makeDir(output_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return LinkerError.IoError,
    };

    // write assembly to temp file
    const asm_path = output_dir ++ "/output.s";
    const asm_file = fs.cwd().createFile(asm_path, .{}) catch return LinkerError.IoError;
    asm_file.writeAll(assembly) catch return LinkerError.IoError;
    asm_file.close();

    // assemble: as -o output.o output.s
    const obj_path = output_dir ++ "/output.o";
    const as_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "as", "-o", obj_path, asm_path },
    }) catch return LinkerError.AssemblerFailed;

    if (as_result.term.Exited != 0) {
        std.debug.print("Assembler error:\n{s}\n", .{as_result.stderr});
        return LinkerError.AssemblerFailed;
    }

    // assemble runtime: as -o runtime.o runtime.s
    const runtime_path = "src/runtime/start-darwin-arm64.s";
    const runtime_obj_path = output_dir ++ "/runtime.o";
    const runtime_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "as", "-o", runtime_obj_path, runtime_path },
    }) catch return LinkerError.AssemblerFailed;

    if (runtime_result.term.Exited != 0) {
        std.debug.print("Runtime assembler error:\n{s}\n", .{runtime_result.stderr});
        return LinkerError.AssemblerFailed;
    }

    // build output path
    var exe_path_buf: [256]u8 = undefined;
    const exe_path = std.fmt.bufPrint(&exe_path_buf, "{s}/{s}", .{ output_dir, output_name }) catch return LinkerError.OutOfMemory;

    // system lib path
    const sdk_path_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "xcrun", "-sdk", "macosx", "--show-sdk-path" },
    }) catch return LinkerError.LinkerFailed;
    defer allocator.free(sdk_path_result.stdout);
    defer allocator.free(sdk_path_result.stderr);

    // check if xcrun succeeded
    if (sdk_path_result.term.Exited != 0) {
        std.debug.print("Failed to find SDK path: {s}\n", .{sdk_path_result.stderr});
        return error.SdkPathNotFound;
    }

    // trim the newline usually returned by xcrun
    const sdk_path = std.mem.trim(u8, sdk_path_result.stdout, "\n\r");

    // link: ld <runtime-ojb> <output-obj> -o <output-prog> -e _start -arch arm64 -lSystem -syslibroot <syslib>
    const ld_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "ld", runtime_obj_path, obj_path, "-o", exe_path, "-e", "_start", "-arch", "arm64", "-lSystem", "-syslibroot", sdk_path },
    }) catch return LinkerError.LinkerFailed;

    if (ld_result.term.Exited != 0) {
        std.debug.print("Linker error:\n{s}\n", .{ld_result.stderr});
        return LinkerError.LinkerFailed;
    }

    // return path (caller should dupe if needed)
    const result_path = allocator.dupe(u8, exe_path) catch return LinkerError.OutOfMemory;
    return .{ .executable_path = result_path };
}
