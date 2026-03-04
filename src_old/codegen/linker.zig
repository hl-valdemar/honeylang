const std = @import("std");
const mem = std.mem;
const fs = std.fs;

pub const LinkerError = error{
    LinkerFailed,
    IoError,
    OutOfMemory,
    ZigNotFound,
};

pub const LinkerResult = struct {
    executable_path: []const u8,
};

/// Compiles LLVM IR into an executable using zig cc.
/// Uses zig cc instead of clang for built-in cross-compilation support (bundled LLD + libc).
/// Optionally compiles and links C source files and libraries alongside the IR.
pub fn link(
    allocator: mem.Allocator,
    llvm_ir: []const u8,
    target_triple: []const u8,
    output_name: []const u8,
    c_source_files: []const []const u8,
    link_libs: []const []const u8,
    link_paths: []const []const u8,
) LinkerError!LinkerResult {
    const output_dir = "zig-out";

    // ensure output directory exists
    fs.cwd().makeDir(output_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return LinkerError.IoError,
    };

    // write LLVM IR to temp file
    const ir_path = output_dir ++ "/output.ll";
    const ir_file = fs.cwd().createFile(ir_path, .{}) catch return LinkerError.IoError;
    ir_file.writeAll(llvm_ir) catch return LinkerError.IoError;
    ir_file.close();

    // build output path
    var exe_path_buf: [256]u8 = undefined;
    const exe_path = std.fmt.bufPrint(&exe_path_buf, "{s}/{s}", .{ output_dir, output_name }) catch return LinkerError.OutOfMemory;

    // Build clang argv dynamically:
    // clang [c_files...] -x ir output.ll -o executable -target <triple>
    var argv = std.ArrayList([]const u8).initCapacity(allocator, 8) catch return LinkerError.OutOfMemory;
    defer argv.deinit(allocator);

    argv.appendSlice(allocator, &.{ "zig", "cc" }) catch return LinkerError.OutOfMemory;

    // Add C source files first (before -x ir which changes the input language)
    for (c_source_files) |c_path| {
        argv.append(allocator, c_path) catch return LinkerError.OutOfMemory;
    }

    // Add LLVM IR input (.ll extension is auto-recognized)
    argv.append(allocator, ir_path) catch return LinkerError.OutOfMemory;

    // Add output and target
    argv.appendSlice(allocator, &.{ "-o", exe_path, "-target", target_triple }) catch return LinkerError.OutOfMemory;

    // Add library search paths (-L flags)
    for (link_paths) |path| {
        const flag = std.fmt.allocPrint(allocator, "-L{s}", .{path}) catch return LinkerError.OutOfMemory;
        argv.append(allocator, flag) catch return LinkerError.OutOfMemory;
    }

    // Add link libraries (-l flags)
    for (link_libs) |lib| {
        const flag = std.fmt.allocPrint(allocator, "-l{s}", .{lib}) catch return LinkerError.OutOfMemory;
        argv.append(allocator, flag) catch return LinkerError.OutOfMemory;
    }

    const zig_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv.items,
    }) catch return LinkerError.ZigNotFound;
    defer allocator.free(zig_result.stdout);
    defer allocator.free(zig_result.stderr);

    if (zig_result.term.Exited != 0) {
        std.debug.print("Linker error:\n{s}\n", .{zig_result.stderr});
        return LinkerError.LinkerFailed;
    }

    // return path (caller should dupe if needed)
    const result_path = allocator.dupe(u8, exe_path) catch return LinkerError.OutOfMemory;
    return .{ .executable_path = result_path };
}
