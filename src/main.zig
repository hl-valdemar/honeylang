const std = @import("std");
const mem = @import("std").mem;
const heap = @import("std").heap;
const builtin = @import("builtin");

const honey = @import("honeylang");

pub fn main() !void {
    honey.ansi.init();

    var gpa = heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    // get command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // parse arguments
    var file_path: ?[]const u8 = null;
    var target: ?honey.codegen.Target = null;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (mem.startsWith(u8, arg, "--target=")) {
            target = parseTarget(arg[9..]);
        } else if (!mem.startsWith(u8, arg, "-")) {
            file_path = arg;
        }
    }

    if (file_path == null) {
        std.debug.print("Usage: {s} [--target=<arch>-<os>] <file>\n", .{args[0]});
        std.debug.print("\nTargets:\n", .{});
        std.debug.print("  aarch64-darwin  ARM64 macOS\n", .{});
        std.debug.print("  aarch64-linux   ARM64 Linux\n", .{});
        std.debug.print("  x86_64-darwin   x86_64 macOS\n", .{});
        std.debug.print("  x86_64-linux    x86_64 Linux\n", .{});
        return error.MissingArgument;
    }

    // default to native target, auto-fallback to LLVM for unsupported architectures
    const final_target = target orelse getNativeTarget();

    switch (builtin.mode) {
        .Debug => try compileDebug(allocator, file_path.?, final_target),
        else => try compileRelease(allocator, file_path.?, final_target),
    }
}

fn parseTarget(target_str: []const u8) ?honey.codegen.Target {
    // parse "<arch>-<os>" format
    if (mem.indexOf(u8, target_str, "-")) |sep| {
        const arch_str = target_str[0..sep];
        const os_str = target_str[sep + 1 ..];

        const arch: honey.codegen.Arch = if (mem.eql(u8, arch_str, "aarch64"))
            .aarch64
        else if (mem.eql(u8, arch_str, "x86_64"))
            .x86_64
        else
            return null;

        const os: honey.codegen.Os = if (mem.eql(u8, os_str, "darwin"))
            .darwin
        else if (mem.eql(u8, os_str, "linux"))
            .linux
        else
            return null;

        return .{ .arch = arch, .os = os };
    }
    return null;
}

fn getNativeOs() honey.codegen.Os {
    return switch (builtin.os.tag) {
        .macos => .darwin,
        .linux => .linux,
        else => .darwin,
    };
}

fn getNativeTarget() honey.codegen.Target {
    const native_arch: honey.codegen.Arch = switch (builtin.cpu.arch) {
        .aarch64 => .aarch64,
        .x86_64 => .x86_64,
        else => @panic("unsupported architecture: " ++ @tagName(builtin.cpu.arch)),
    };

    return .{ .arch = native_arch, .os = getNativeOs() };
}

pub fn compileDebug(gpa: mem.Allocator, file_path: []const u8, target: honey.codegen.Target) !void {
    const ansi = honey.ansi;

    // 1. read source
    var source_arena = heap.ArenaAllocator.init(gpa);
    defer source_arena.deinit();
    const src = try honey.source.fromFile(source_arena.allocator(), file_path, 0);

    std.debug.print("\n{s}::[[ Source Code ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
    std.debug.print("{s}\n", .{src.buffer});

    // 2. scan source
    var token_arena = heap.ArenaAllocator.init(gpa);
    defer token_arena.deinit();
    const lexer_result = try honey.lexer.scan(token_arena.allocator(), &src);

    // print generated tokens
    std.debug.print("\n{s}::[[ Scanning ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
    std.debug.print("Generated {d} tokens:\n\n", .{lexer_result.tokens.items.len});
    honey.token_printer.print(&lexer_result.tokens, &src);

    // print lexer errors if any
    if (lexer_result.errors.hasErrors()) {
        std.debug.print("\n{s}Reported {d} lexer errors:{s}\n\n", .{ ansi.red(), lexer_result.errors.count(), ansi.reset() });
        honey.lexer.error_printer.print(&lexer_result.errors, &src, file_path);
    }

    // 3. parse tokens
    var ast_arena = std.heap.ArenaAllocator.init(gpa);
    defer ast_arena.deinit();
    const parse_result = try honey.parser.parse(ast_arena.allocator(), lexer_result.tokens, &src);

    // print generated parse tree
    std.debug.print("\n\n{s}::[[ Parsing ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
    std.debug.print("Parsed {} nodes:\n\n", .{parse_result.ast.nodeCount()});
    honey.ast_printer.print(&parse_result.ast, &lexer_result.tokens, &src);

    // print parse errors if any
    if (parse_result.errors.hasErrors()) {
        std.debug.print("\n{s}Reported {d} parse errors:{s}\n\n", .{ ansi.red(), parse_result.errors.count(), ansi.reset() });
        honey.parser.error_printer.print(&parse_result.errors, &src, file_path);
    }

    // 4. analyze parse tree
    var semantic_arena = std.heap.ArenaAllocator.init(gpa);
    defer semantic_arena.deinit();
    const sem_result = try honey.semantic.analyze(semantic_arena.allocator(), &parse_result.ast, &lexer_result.tokens, &src);

    // print generated symbol table
    std.debug.print("\n\n{s}::[[ Semantic Analysis ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
    std.debug.print("Collected {d} symbols:\n\n", .{sem_result.symbols.count()});
    honey.symbol_printer.print(&sem_result.symbols, &src);

    std.debug.print("\nGenerated type registry:\n\n", .{});
    honey.type_printer.print(&sem_result.types);

    // print semantic diagnostics if any
    if (sem_result.errors.hasErrors()) {
        std.debug.print("\n{s}Reported {d} errors:{s}\n\n", .{ ansi.red(), sem_result.errors.count(), ansi.reset() });
    }
    if (sem_result.errors.hasWarnings()) {
        std.debug.print("\n{s}Reported {d} warnings:{s}\n\n", .{ ansi.yellow(), sem_result.errors.warningCount(), ansi.reset() });
    }
    if (sem_result.errors.hasErrors() or sem_result.errors.hasWarnings()) {
        honey.semantic.error_printer.print(&sem_result.errors, &src, file_path);
    }

    // 5. comptime expression evaluation
    var comptime_arena = std.heap.ArenaAllocator.init(gpa);
    defer comptime_arena.deinit();
    const comptime_result = try honey.comptime_.evaluate(comptime_arena.allocator(), &parse_result.ast, &lexer_result.tokens, &src, &sem_result.symbols);

    // print generated symbol table
    std.debug.print("\n\n{s}::[[ Comptime Evaluation ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
    std.debug.print("Evaluated {d} expressions:\n\n", .{comptime_result.eval_literals.items.len});
    honey.comptime_printer.print(&comptime_result, &sem_result.symbols, &src);

    // 6. code emission
    var codegen_arena = std.heap.ArenaAllocator.init(gpa);
    defer codegen_arena.deinit();

    const codegen_result = try honey.codegen.generate(codegen_arena.allocator(), target, &comptime_result, &sem_result.symbols, &sem_result.node_types, &sem_result.skip_nodes, &parse_result.ast, &lexer_result.tokens, &src);

    // print generated MIR
    std.debug.print("\n\n{s}::[[ MIR Generation ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
    std.debug.print("Generated {d} functions:\n\n", .{codegen_result.mir.functions.items.len});
    honey.mir_printer.print(&codegen_result.mir);

    // print emitted LLVM IR
    std.debug.print("\n{s}::[[ Code Emission ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
    std.debug.print("Emitted LLVM IR (target: {s}-{s}):\n\n", .{ @tagName(target.arch), @tagName(target.os) });
    std.debug.print("{s}", .{codegen_result.output});

    // 7. link into executable
    const link_result = honey.codegen.linker.link(
        codegen_arena.allocator(),
        codegen_result.output,
        target.getLLVMTriple(),
        "program",
    );

    if (link_result) |result| {
        std.debug.print("\n{s}::[[ Linking ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
        std.debug.print("Created executable: {s}\n", .{result.executable_path});
    } else |err| {
        std.debug.print("\n{s}Linking failed: {s}{s}\n", .{ ansi.red(), @errorName(err), ansi.reset() });
        return;
    }
}

pub fn compileRelease(gpa: mem.Allocator, file_path: []const u8, target: honey.codegen.Target) !void {
    const ansi = honey.ansi;

    // 1. read source
    var source_arena = heap.ArenaAllocator.init(gpa);
    defer source_arena.deinit();
    const src = try honey.source.fromFile(source_arena.allocator(), file_path, 0);

    // 2. scan source
    var token_arena = heap.ArenaAllocator.init(gpa);
    defer token_arena.deinit();
    const lexer_result = try honey.lexer.scan(token_arena.allocator(), &src);

    // print lexer errors if any
    if (lexer_result.errors.hasErrors()) {
        std.debug.print("\n{s}Reported {d} lexer errors:{s}\n\n", .{ ansi.red(), lexer_result.errors.count(), ansi.reset() });
        honey.lexer.error_printer.print(&lexer_result.errors, &src, file_path);
    }

    // 3. parse tokens
    var ast_arena = std.heap.ArenaAllocator.init(gpa);
    defer ast_arena.deinit();
    const parse_result = try honey.parser.parse(ast_arena.allocator(), lexer_result.tokens, &src);

    // print parse errors if any
    if (parse_result.errors.hasErrors()) {
        std.debug.print("\n{s}Reported {d} parse errors:{s}\n\n", .{ ansi.red(), parse_result.errors.count(), ansi.reset() });
        honey.parser.error_printer.print(&parse_result.errors, &src, file_path);
    }

    // 4. analyze parse tree
    var semantic_arena = std.heap.ArenaAllocator.init(gpa);
    defer semantic_arena.deinit();
    const sem_result = try honey.semantic.analyze(semantic_arena.allocator(), &parse_result.ast, &lexer_result.tokens, &src);

    // print semantic diagnostics if any
    if (sem_result.errors.hasErrors()) {
        std.debug.print("\n{s}Reported {d} errors:{s}\n\n", .{ ansi.red(), sem_result.errors.count(), ansi.reset() });
    }
    if (sem_result.errors.hasWarnings()) {
        std.debug.print("\n{s}Reported {d} warnings:{s}\n\n", .{ ansi.yellow(), sem_result.errors.warningCount(), ansi.reset() });
    }
    if (sem_result.errors.hasErrors() or sem_result.errors.hasWarnings()) {
        honey.semantic.error_printer.print(&sem_result.errors, &src, file_path);
    }

    // 5. comptime expression evaluation
    var comptime_arena = std.heap.ArenaAllocator.init(gpa);
    defer comptime_arena.deinit();
    const comptime_result = try honey.comptime_.evaluate(comptime_arena.allocator(), &parse_result.ast, &lexer_result.tokens, &src, &sem_result.symbols);

    // 6. code emission
    var codegen_arena = std.heap.ArenaAllocator.init(gpa);
    defer codegen_arena.deinit();

    const codegen_result = try honey.codegen.generate(codegen_arena.allocator(), target, &comptime_result, &sem_result.symbols, &sem_result.node_types, &sem_result.skip_nodes, &parse_result.ast, &lexer_result.tokens, &src);

    // 7. link into executable
    const link_result = honey.codegen.linker.link(
        codegen_arena.allocator(),
        codegen_result.output,
        target.getLLVMTriple(),
        "program",
    );

    if (link_result) |result| {
        std.debug.print("Created executable: {s}\n", .{result.executable_path});
    } else |err| {
        std.debug.print("{s}Linking failed: {s}{s}\n", .{ ansi.red(), @errorName(err), ansi.reset() });
    }
}
