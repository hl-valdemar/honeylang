const std = @import("std");
const mem = @import("std").mem;
const heap = @import("std").heap;
const builtin = @import("builtin");

const honey = @import("honeylang");

pub fn main() !void {
    var gpa = heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();

    const file_path = "examples/func_decls.hon";

    switch (builtin.mode) {
        .Debug => try compileDebug(gpa.allocator(), file_path),
        else => try compileRelease(gpa.allocator(), file_path),
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
    for (tokens.items) |token| {
        if (token.len > 0) {
            std.debug.print("{} = \"{s}\"\n", .{ token.kind, src.getSlice(token.start, token.start + token.len) });
        } else {
            std.debug.print("{}\n", .{token.kind});
        }
    }

    // 3. parse tokens
    var ast_arena = std.heap.ArenaAllocator.init(gpa);
    defer ast_arena.deinit();

    const ast = try honey.parser.parse(ast_arena.allocator(), tokens);

    std.debug.print("\n\n::[[ Parsing ]]::\n\n", .{});
    std.debug.print("Parsed {} nodes:\n\n", .{ast.nodeCount()});
    honey.ast_printer.print(&ast, &tokens, &src);

    // TODO: analyze parse tree
    // var semantic_arena = std.heap.ArenaAllocator.init(gpa);
    // defer semantic_arena.deinit();
    //
    // try honey.semantic.analyze(semantic_arena.allocator(), ast);
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
