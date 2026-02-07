pub const source = @import("source/source.zig");
pub const lexer = @import("lexer/lexer.zig");
pub const parser = @import("parser/parser.zig");
pub const semantic = @import("semantic/semantic.zig");
pub const comptime_ = @import("comptime/comptime.zig");
pub const codegen = @import("codegen/codegen.zig");

pub const ansi = @import("utils/ansi.zig");
pub const token_printer = @import("lexer/token-printer.zig");
pub const ast_printer = @import("parser/ast-printer.zig");
pub const symbol_printer = @import("semantic/symbol-printer.zig");
pub const type_printer = @import("semantic/type-printer.zig");
pub const comptime_printer = @import("comptime/comptime-printer.zig");
pub const mir_printer = @import("codegen/mir-printer.zig");

test {
    _ = @import("integration_tests.zig");
}
