const std = @import("std");
const honey = @import("honeylang");

const source = honey.source;
const lexer = honey.lexer;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const file_path = "examples/func_decls.hon";

    // 1. read the source
    const src = try source.fromFile(allocator, file_path, 0);
    defer src.deinit(allocator);

    std.debug.print("\n::[[ Source Code ]]::\n\n", .{});
    std.debug.print("{s}\n", .{src.buffer});

    // 2. scan the source
    std.debug.print("\n::[[ Scanning ]]::\n\n", .{});

    var tokens = try lexer.scan(allocator, &src);
    defer tokens.deinit(allocator);

    // print generated tokens
    std.debug.print("Generated {d} tokens:\n\n", .{tokens.items.len});
    for (tokens.items) |token| {
        if (token.len > 0) {
            std.debug.print("kind: {}, value: {s}\n", .{ token.kind, src.getSlice(token.start, token.start + token.len) });
        } else {
            std.debug.print("kind: {}\n", .{token.kind});
        }
    }

    // TODO:
    // 3. parse the tokens
    // 4. analyze the parse tree
}
