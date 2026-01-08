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

    if (args.len < 2) {
        std.debug.print("Usage: {s} <file>\n", .{args[0]});
        return error.MissingArgument;
    }

    const file_path = args[1];

    switch (builtin.mode) {
        .Debug => try compileDebug(allocator, file_path),
        else => try compileRelease(allocator, file_path),
    }
}

pub fn compileDebug(gpa: mem.Allocator, file_path: []const u8) !void {
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
    const parse_result = try honey.parser.parse(ast_arena.allocator(), lexer_result.tokens);

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

    // print semantic errors if any
    if (sem_result.errors.hasErrors()) {
        std.debug.print("\n{s}Reported {d} errors:{s}\n\n", .{ ansi.red(), sem_result.errors.count(), ansi.reset() });
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

    const target = .arm64;
    const codegen_result = try honey.codegen.generate(codegen_arena.allocator(), target, &parse_result.ast, &lexer_result.tokens, &src);

    // print emitted code
    std.debug.print("\n\n{s}::[[ Code Emission ]]::{s}\n\n", .{ ansi.magenta(), ansi.reset() });
    std.debug.print("Emitted {s} assembly:\n\n", .{@tagName(target)});
    std.debug.print("{s}", .{codegen_result.assembly});
}

pub fn compileRelease(gpa: mem.Allocator, file_path: []const u8) !void {
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
    const parse_result = try honey.parser.parse(ast_arena.allocator(), lexer_result.tokens);

    // print parse errors if any
    if (parse_result.errors.hasErrors()) {
        std.debug.print("\n{s}Reported {d} parse errors:{s}\n\n", .{ ansi.red(), parse_result.errors.count(), ansi.reset() });
        honey.parser.error_printer.print(&parse_result.errors, &src, file_path);
    }

    // 4. analyze parse tree
    var semantic_arena = std.heap.ArenaAllocator.init(gpa);
    defer semantic_arena.deinit();
    const sem_result = try honey.semantic.analyze(semantic_arena.allocator(), &parse_result.ast, &lexer_result.tokens, &src);

    // print semantic errors if any
    if (sem_result.errors.hasErrors()) {
        std.debug.print("\n{s}Reported {d} errors:{s}\n\n", .{ ansi.red(), sem_result.errors.count(), ansi.reset() });
        honey.semantic.error_printer.print(&sem_result.errors, &src, file_path);
    }

    // 5. comptime expression evaluation
    var comptime_arena = std.heap.ArenaAllocator.init(gpa);
    defer comptime_arena.deinit();
    const comptime_result = try honey.comptime_.evaluate(comptime_arena.allocator(), &parse_result.ast, &lexer_result.tokens, &src, &sem_result.symbols);
    _ = comptime_result;
}
