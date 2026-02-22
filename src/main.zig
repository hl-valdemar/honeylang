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
    var c_sources = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer c_sources.deinit(allocator);
    var link_libs = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer link_libs.deinit(allocator);
    var link_paths = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer link_paths.deinit(allocator);
    var include_paths = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    defer include_paths.deinit(allocator);

    var list_targets = false;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (mem.eql(u8, arg, "--target")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Error: --target requires a value (see --list-targets)\n", .{});
                return error.InvalidTarget;
            }
            target = parseTarget(args[i]);
            if (target == null) {
                std.debug.print("Unknown target: {s} (see --list-targets)\n", .{args[i]});
                return error.InvalidTarget;
            }
        } else if (mem.eql(u8, arg, "--list-targets")) {
            list_targets = true;
        } else if (mem.eql(u8, arg, "--c-source")) {
            i += 1;
            while (i < args.len and !mem.startsWith(u8, args[i], "--")) : (i += 1) {
                try c_sources.append(allocator, args[i]);
            }
            // Back up so the outer loop's increment processes the next --flag
            if (i < args.len and mem.startsWith(u8, args[i], "--")) i -= 1;
        } else if (mem.eql(u8, arg, "--link-lib")) {
            i += 1;
            while (i < args.len and !mem.startsWith(u8, args[i], "--")) : (i += 1) {
                try link_libs.append(allocator, args[i]);
            }
            if (i < args.len and mem.startsWith(u8, args[i], "--")) i -= 1;
        } else if (mem.eql(u8, arg, "--link-path")) {
            i += 1;
            while (i < args.len and !mem.startsWith(u8, args[i], "--")) : (i += 1) {
                try link_paths.append(allocator, args[i]);
            }
            if (i < args.len and mem.startsWith(u8, args[i], "--")) i -= 1;
        } else if (mem.eql(u8, arg, "--include-path")) {
            i += 1;
            while (i < args.len and !mem.startsWith(u8, args[i], "--")) : (i += 1) {
                try include_paths.append(allocator, args[i]);
            }
            if (i < args.len and mem.startsWith(u8, args[i], "--")) i -= 1;
        } else if (!mem.startsWith(u8, arg, "-")) {
            file_path = arg;
        }
    }

    if (list_targets) {
        printTargets();
        return;
    }

    if (file_path == null) {
        std.debug.print("Usage: {s} [options] <file>\n\n", .{args[0]});
        std.debug.print("Options:\n", .{});
        std.debug.print("  --target <target>       Set compilation target (see --list-targets)\n", .{});
        std.debug.print("  --list-targets          List all supported compilation targets\n", .{});
        std.debug.print("  --c-source <files...>   C source files to compile and link\n", .{});
        std.debug.print("  --link-lib <libs...>    Libraries to link (-l)\n", .{});
        std.debug.print("  --link-path <dirs...>   Library search paths (-L)\n", .{});
        std.debug.print("  --include-path <dirs..> Additional C header search paths\n", .{});
        return error.MissingArgument;
    }

    // check that the source file exists
    std.fs.cwd().access(file_path.?, .{}) catch {
        std.debug.print("Error: file not found: {s}\n", .{file_path.?});
        std.process.exit(1);
    };

    // default to native target, auto-fallback to LLVM for unsupported architectures
    const final_target = target orelse getNativeTarget() orelse {
        std.debug.print("Could not detect native target. Specify one with --target (see --list-targets)\n", .{});
        return error.InvalidTarget;
    };

    switch (builtin.mode) {
        .Debug => try compileDebug(allocator, file_path.?, final_target, c_sources.items, link_libs.items, link_paths.items, include_paths.items),
        else => try compileRelease(allocator, file_path.?, final_target, c_sources.items, link_libs.items, link_paths.items, include_paths.items),
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
        else if (mem.eql(u8, arch_str, "arm"))
            .arm
        else if (mem.eql(u8, arch_str, "x86"))
            .x86
        else
            return null;

        const os: honey.codegen.Os = if (mem.eql(u8, os_str, "macos"))
            .macos
        else if (mem.eql(u8, os_str, "linux"))
            .linux
        else
            return null;

        // 32-bit Darwin is not supported
        if ((arch == .arm or arch == .x86) and os == .macos) return null;

        return .{ .arch = arch, .os = os };
    }
    return null;
}

fn printTargets() void {
    std.debug.print(
        \\Supported targets:
        \\
        \\arm:
        \\  arm-linux         ARM 32-bit Linux (armv7 hard-float)
        \\
        \\arm64:
        \\  aarch64-macos     ARM 64-bit macOS
        \\  aarch64-linux     ARM 64-bit Linux
        \\
        \\x86:
        \\  x86-linux         x86 32-bit Linux
        \\
        \\x86_64:
        \\  x86_64-macos      x86 64-bit macOS
        \\  x86_64-linux      x86 64-bit Linux
        \\
    , .{});
}

fn getNativeTarget() ?honey.codegen.Target {
    const os: honey.codegen.Os = switch (builtin.os.tag) {
        .macos => .macos,
        .linux => .linux,
        else => return null,
    };
    const arch: honey.codegen.Arch = switch (builtin.cpu.arch) {
        .aarch64 => .aarch64,
        .x86_64 => .x86_64,
        else => return null,
    };
    return .{ .arch = arch, .os = os };
}

pub fn compileDebug(gpa: mem.Allocator, file_path: []const u8, target: honey.codegen.Target, c_sources: []const []const u8, link_libs: []const []const u8, link_paths: []const []const u8, include_paths: []const []const u8) !void {
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
        honey.parser.error_printer.print(&parse_result.errors, &src, file_path);
    }

    // 3.5 resolve imports
    var import_arena = std.heap.ArenaAllocator.init(gpa);
    defer import_arena.deinit();
    const resolved_imports = try honey.imports.resolveImports(
        import_arena.allocator(),
        &parse_result.ast,
        &lexer_result.tokens,
        &src,
        file_path,
        include_paths,
    );

    if (resolved_imports.count() > 0) {
        std.debug.print("\n{s}::[[ Import Resolution ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
        std.debug.print("Resolved {d} import(s)\n", .{resolved_imports.count()});
        for (resolved_imports.imports.items) |imp| {
            std.debug.print("  - {s} (namespace: {s})\n", .{ imp.file_path, imp.namespace_name });
        }
    }

    // 4. analyze parse tree
    var semantic_arena = std.heap.ArenaAllocator.init(gpa);
    defer semantic_arena.deinit();
    var sem_result = try honey.semantic.analyze(
        semantic_arena.allocator(),
        &parse_result.ast,
        &lexer_result.tokens,
        &src,
        &resolved_imports,
        target.ptrSize(),
    );

    // check for missing entry point
    if (sem_result.symbols.lookup("main") == null) {
        try sem_result.errors.add(.{ .kind = .missing_entry_point, .start = 0, .end = 0 });
    }

    // print generated symbol table
    std.debug.print("\n\n{s}::[[ Semantic Analysis ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
    std.debug.print("Collected {d} symbols:\n\n", .{sem_result.symbols.count()});
    honey.symbol_printer.print(&sem_result.symbols, &src);

    std.debug.print("\nGenerated type registry:\n\n", .{});
    honey.type_printer.print(&sem_result.types);

    // print semantic diagnostics if any
    if (sem_result.errors.hasErrors() or sem_result.errors.hasWarnings()) {
        honey.semantic.error_printer.print(&sem_result.errors, &src, file_path, &resolved_imports);
    }

    // 5. comptime expression evaluation
    var comptime_arena = std.heap.ArenaAllocator.init(gpa);
    defer comptime_arena.deinit();
    const comptime_result = try honey.comptime_.evaluate(
        comptime_arena.allocator(),
        &parse_result.ast,
        &lexer_result.tokens,
        &src,
        &sem_result.symbols,
        &resolved_imports,
    );

    // print generated symbol table
    std.debug.print("\n\n{s}::[[ Comptime Evaluation ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
    std.debug.print("Evaluated {d} expressions:\n\n", .{comptime_result.eval_literals.items.len});
    honey.comptime_printer.print(&comptime_result, &sem_result.symbols, &src);

    // 6. code emission
    var codegen_arena = std.heap.ArenaAllocator.init(gpa);
    defer codegen_arena.deinit();

    const codegen_result = try honey.codegen.generate(
        codegen_arena.allocator(),
        target,
        &comptime_result,
        &sem_result.symbols,
        &sem_result.types,
        &sem_result.node_types,
        &sem_result.import_node_types,
        &sem_result.skip_nodes,
        &parse_result.ast,
        &lexer_result.tokens,
        &src,
        &resolved_imports,
    );

    // print generated MIR
    std.debug.print("\n\n{s}::[[ MIR Generation ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
    std.debug.print("Generated {d} functions:\n\n", .{codegen_result.mir.functions.items.len});
    honey.mir_printer.print(&codegen_result.mir);

    // print emitted LLVM IR
    std.debug.print("\n{s}::[[ Code Emission ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
    std.debug.print("Emitted LLVM IR (target: {s}-{s}):\n\n", .{ @tagName(target.arch), @tagName(target.os) });
    std.debug.print("{s}", .{codegen_result.output});

    // 7. link into executable
    if (c_sources.len > 0) {
        std.debug.print("\n{s}::[[ C Sources ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
        for (c_sources) |c_path| {
            std.debug.print("  - {s}\n", .{c_path});
        }
    }

    const link_result = honey.codegen.linker.link(
        codegen_arena.allocator(),
        codegen_result.output,
        target.getZigTriple(),
        "program",
        c_sources,
        link_libs,
        link_paths,
    );

    if (link_result) |result| {
        std.debug.print("\n{s}::[[ Linking ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
        std.debug.print("Created executable: {s}\n", .{result.executable_path});
    } else |err| {
        std.debug.print("\n{s}Linking failed: {s}{s}\n", .{ ansi.red(), @errorName(err), ansi.reset() });
        return;
    }
}

pub fn compileRelease(gpa: mem.Allocator, file_path: []const u8, target: honey.codegen.Target, c_sources: []const []const u8, link_libs: []const []const u8, link_paths: []const []const u8, include_paths: []const []const u8) !void {
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
        honey.lexer.error_printer.print(&lexer_result.errors, &src, file_path);
    }

    // 3. parse tokens
    var ast_arena = std.heap.ArenaAllocator.init(gpa);
    defer ast_arena.deinit();
    const parse_result = try honey.parser.parse(ast_arena.allocator(), lexer_result.tokens, &src);

    // print parse errors if any
    if (parse_result.errors.hasErrors()) {
        honey.parser.error_printer.print(&parse_result.errors, &src, file_path);
    }

    // 3.5 resolve imports
    var import_arena = std.heap.ArenaAllocator.init(gpa);
    defer import_arena.deinit();
    const resolved_imports = try honey.imports.resolveImports(
        import_arena.allocator(),
        &parse_result.ast,
        &lexer_result.tokens,
        &src,
        file_path,
        include_paths,
    );

    // 4. analyze parse tree
    var semantic_arena = std.heap.ArenaAllocator.init(gpa);
    defer semantic_arena.deinit();
    var sem_result = try honey.semantic.analyze(
        semantic_arena.allocator(),
        &parse_result.ast,
        &lexer_result.tokens,
        &src,
        &resolved_imports,
        target.ptrSize(),
    );

    // check for missing entry point
    if (sem_result.symbols.lookup("main") == null) {
        try sem_result.errors.add(.{ .kind = .missing_entry_point, .start = 0, .end = 0 });
    }

    // print semantic diagnostics if any
    if (sem_result.errors.hasErrors() or sem_result.errors.hasWarnings()) {
        honey.semantic.error_printer.print(&sem_result.errors, &src, file_path, &resolved_imports);
    }

    // 5. comptime expression evaluation
    var comptime_arena = std.heap.ArenaAllocator.init(gpa);
    defer comptime_arena.deinit();
    const comptime_result = try honey.comptime_.evaluate(
        comptime_arena.allocator(),
        &parse_result.ast,
        &lexer_result.tokens,
        &src,
        &sem_result.symbols,
        &resolved_imports,
    );

    // 6. code emission
    var codegen_arena = std.heap.ArenaAllocator.init(gpa);
    defer codegen_arena.deinit();

    const codegen_result = try honey.codegen.generate(
        codegen_arena.allocator(),
        target,
        &comptime_result,
        &sem_result.symbols,
        &sem_result.types,
        &sem_result.node_types,
        &sem_result.import_node_types,
        &sem_result.skip_nodes,
        &parse_result.ast,
        &lexer_result.tokens,
        &src,
        &resolved_imports,
    );

    // 7. link into executable
    const link_result = honey.codegen.linker.link(
        codegen_arena.allocator(),
        codegen_result.output,
        target.getZigTriple(),
        "program",
        c_sources,
        link_libs,
        link_paths,
    );

    if (link_result) |result| {
        std.debug.print("Created executable: {s}\n", .{result.executable_path});
    } else |err| {
        std.debug.print("{s}Linking failed: {s}{s}\n", .{
            ansi.red(),
            @errorName(err),
            ansi.reset(),
        });
    }
}
