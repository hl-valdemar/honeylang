const std = @import("std");
const honey = @import("honeylang");

const source = honey.source;
const lexer = honey.lexer;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const file_path = "examples/const_decls.hon";

    // read the source
    const src = try source.fromFile(allocator, file_path);
    defer src.deinit(allocator);

    // print the source
    std.debug.print("\n::[[ Source Code ]]::\n\n", .{});
    std.debug.print("{s}\n", .{src.buffer});

    // scan the source
    var tokens = try lexer.scan(allocator, &src);
    defer tokens.deinit(allocator);

    var i: usize = 0;
    while (i < tokens.len) : (i += 1) {
        const token = tokens.get(i) orelse break;
        std.debug.print("kind: {}, value: {s}\n", .{token.kind, src.getSliceFromRange(token.src_range)});
    }

    // TODO:
    // 3. parse the tokens
    // 4. analyze the parse tree
}
