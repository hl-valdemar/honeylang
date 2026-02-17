const std = @import("std");
const honey = @import("../root.zig");

const LexerErrorKind = @import("../lexer/error.zig").LexerErrorKind;
const ParseErrorKind = @import("../parser/error.zig").ParseErrorKind;
const SemanticErrorKind = @import("../semantic/error.zig").SemanticErrorKind;

pub const Phase = enum { lexer, parser, semantic, codegen };

pub const CompileResult = struct {
    arena: std.heap.ArenaAllocator,
    lex: honey.lexer.LexerResult,
    parse: ?honey.parser.ParseResult,
    sem: ?honey.semantic.SemanticResult,
    codegen: ?honey.codegen.CodeGenResult,

    pub fn deinit(self: *CompileResult) void {
        self.arena.deinit();
    }

    pub fn expectNoErrors(self: *const CompileResult) !void {
        try std.testing.expect(!self.lex.errors.hasErrors());
        if (self.parse) |p| try std.testing.expect(!p.errors.hasErrors());
        if (self.sem) |s| try std.testing.expect(!s.errors.hasErrors());
    }

    pub fn expectLLVMContains(self: *const CompileResult, expected: []const u8) !void {
        const cg = self.codegen orelse return error.TestExpectedEqual;
        if (std.mem.indexOf(u8, cg.output, expected) == null) {
            std.debug.print("\nExpected LLVM IR to contain: {s}\n", .{expected});
            std.debug.print("Actual LLVM IR:\n{s}\n", .{cg.output});
            return error.TestExpectedEqual;
        }
    }

    pub fn expectLexerError(self: *const CompileResult, expected: LexerErrorKind) !void {
        for (self.lex.errors.errors.items) |err| {
            if (err.kind == expected) return;
        }
        return error.TestExpectedEqual;
    }

    pub fn expectParseError(self: *const CompileResult, expected: ParseErrorKind) !void {
        const p = self.parse orelse return error.TestExpectedEqual;
        for (p.errors.errors.items) |err| {
            if (err.kind == expected) return;
        }
        return error.TestExpectedEqual;
    }

    pub fn expectSemanticError(self: *const CompileResult, expected: SemanticErrorKind) !void {
        const s = self.sem orelse return error.TestExpectedEqual;
        for (s.errors.errors.items) |err| {
            if (err.kind == expected) return;
        }
        return error.TestExpectedEqual;
    }
};

pub fn compileTo(phase: Phase, src_input: []const u8) !CompileResult {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    errdefer arena.deinit();

    const allocator = arena.allocator();
    const src = honey.source.fromStr(src_input, 0);

    // lexer
    const lex = try honey.lexer.scan(allocator, &src);
    if (phase == .lexer) return .{ .arena = arena, .lex = lex, .parse = null, .sem = null, .codegen = null };

    // parser
    const parse = try honey.parser.parse(allocator, lex.tokens, &src);
    if (phase == .parser) return .{ .arena = arena, .lex = lex, .parse = parse, .sem = null, .codegen = null };

    // semantic (no file-based imports in integration tests)
    var sem = try honey.semantic.analyze(allocator, &parse.ast, &lex.tokens, &src, null);
    if (phase == .semantic) return .{ .arena = arena, .lex = lex, .parse = parse, .sem = sem, .codegen = null };

    // comptime
    const comptime_result = try honey.comptime_.evaluate(allocator, &parse.ast, &lex.tokens, &src, &sem.symbols, null);

    // codegen
    const target: honey.codegen.Target = .{ .arch = .aarch64, .os = .darwin };
    const codegen_result = try honey.codegen.generate(allocator, target, &comptime_result, &sem.symbols, &sem.types, &sem.node_types, &sem.import_node_types, &sem.skip_nodes, &parse.ast, &lex.tokens, &src, null);
    return .{ .arena = arena, .lex = lex, .parse = parse, .sem = sem, .codegen = codegen_result };
}
