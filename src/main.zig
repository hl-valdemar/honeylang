const std = @import("std");
const mem = @import("std").mem;
const heap = @import("std").heap;
const builtin = @import("builtin");

const honey = @import("honeylang");

pub fn main() !void {
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
    // 1. read source
    var source_arena = heap.ArenaAllocator.init(gpa);
    defer source_arena.deinit();

    const src = try honey.source.fromFile(source_arena.allocator(), file_path, 0);

    std.debug.print("\n::[[ Source Code ]]::\n\n", .{});
    std.debug.print("{s}\n", .{src.buffer});

    // 2. scan source
    var token_arena = heap.ArenaAllocator.init(gpa);
    defer token_arena.deinit();

    const tokens = try honey.lexer.scan(token_arena.allocator(), &src);

    // print generated tokens
    std.debug.print("\n::[[ Scanning ]]::\n\n", .{});
    std.debug.print("Generated {d} tokens:\n\n", .{tokens.items.len});
    honey.token_printer.print(&tokens, &src);

    // 3. parse tokens
    var ast_arena = std.heap.ArenaAllocator.init(gpa);
    defer ast_arena.deinit();

    const ast = try honey.parser.parse(ast_arena.allocator(), tokens);

    // print generated parse tree
    std.debug.print("\n\n::[[ Parsing ]]::\n\n", .{});
    std.debug.print("Parsed {} nodes:\n\n", .{ast.nodeCount()});
    honey.ast_printer.print(&ast, &tokens, &src);

    // 4. analyze parse tree
    var semantic_arena = std.heap.ArenaAllocator.init(gpa);
    defer semantic_arena.deinit();

    const sem_result = try honey.semantic.analyze(semantic_arena.allocator(), &ast, &tokens, &src);

    // print generated symbol table
    std.debug.print("\n\n::[[ Semantic Analysis ]]::\n\n", .{});
    std.debug.print("Collected {d} symbols:\n\n", .{sem_result.symbols.count()});
    honey.symbol_printer.print(&sem_result.symbols, &src);
}

pub fn compileRelease(gpa: mem.Allocator, file_path: []const u8) !void {
    // 1. read source
    var source_arena = heap.ArenaAllocator.init(gpa);
    defer source_arena.deinit();

    const src = try honey.source.fromFile(source_arena.allocator(), file_path, 0);

    // 2. scan source
    var token_arena = heap.ArenaAllocator.init(gpa);
    defer token_arena.deinit();

    const tokens = try honey.lexer.scan(token_arena.allocator(), &src);
    _ = tokens;
}
