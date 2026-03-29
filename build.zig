const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // library module - public API exposed via src/root.zig
    const mod = b.addModule("honeylang", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
    });

    const pretty = b.addModule("pretty", .{
        .root_source_file = b.path("lib/pretty.zig"),
        .target = target,
    });

    // CLI executable - separate root module that imports the library
    const exe = b.addExecutable(.{
        .name = "honey",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "honeylang", .module = mod },
                .{ .name = "pretty", .module = pretty },
            },
        }),
    });

    b.installArtifact(exe);

    // zig build run [-- args...]
    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // zig build test - runs tests for both the library and the executable
    const mod_tests = b.addTest(.{ .root_module = mod });
    const exe_tests = b.addTest(.{ .root_module = exe.root_module });

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&b.addRunArtifact(mod_tests).step);
    test_step.dependOn(&b.addRunArtifact(exe_tests).step);
}
