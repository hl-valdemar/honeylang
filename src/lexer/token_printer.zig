const std = @import("std");

const TokenList = @import("token.zig").TokenList;
const SourceCode = @import("../source/source.zig").SourceCode;

pub fn print(tokens: *const TokenList, src: *const SourceCode) void {
    for (tokens.items) |token| {
        if (token.len > 0) {
            std.debug.print("{} = \"{s}\"\n", .{ token.kind, src.getSlice(token.start, token.start + token.len) });
        } else {
            std.debug.print("{}\n", .{token.kind});
        }
    }
}
