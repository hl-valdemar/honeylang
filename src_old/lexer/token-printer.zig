const std = @import("std");

const TokenList = @import("token.zig").TokenList;
const TokenKind = @import("token.zig").Kind;
const SourceCode = @import("../source/source.zig").SourceCode;

pub fn print(tokens: *const TokenList, src: *const SourceCode) void {
    const count = tokens.items.len;

    if (count == 0) {
        std.debug.print("(no tokens)\n", .{});
        return;
    }

    // print header
    std.debug.print("{s:<5} {s:<16} {s:<8} {s:<5} {s}\n", .{
        "idx",
        "kind",
        "start",
        "len",
        "value",
    });
    std.debug.print("{s:-<5} {s:-<16} {s:-<8} {s:-<5} {s:-<16}\n", .{
        "",
        "",
        "",
        "",
        "",
    });

    // print each token
    for (tokens.items, 0..) |token, i| {
        const kind_str = @tagName(token.kind);
        const value = if (token.len > 0)
            src.getSlice(token.start, token.start + token.len)
        else
            "";

        // format value with quotes for readability (except empty)
        if (value.len > 0) {
            std.debug.print("{d:<5} {s:<16} {d:<8} {d:<5} \"{s}\"\n", .{
                i,
                kind_str,
                token.start,
                token.len,
                value,
            });
        } else {
            std.debug.print("{d:<5} {s:<16} {d:<8} {d:<5}\n", .{
                i,
                kind_str,
                token.start,
                token.len,
            });
        }
    }
}

/// Compact output - just kind and value on each line.
pub fn printCompact(tokens: *const TokenList, src: *const SourceCode) void {
    for (tokens.items) |token| {
        const kind_str = @tagName(token.kind);

        if (token.len > 0) {
            const value = src.getSlice(token.start, token.start + token.len);
            std.debug.print("{s} \"{s}\"\n", .{ kind_str, value });
        } else {
            std.debug.print("{s}\n", .{kind_str});
        }
    }
}
