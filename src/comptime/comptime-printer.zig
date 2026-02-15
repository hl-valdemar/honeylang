const std = @import("std");

const ComptimeResult = @import("comptime.zig").ComptimeResult;
const EvalState = @import("comptime.zig").EvalState;

const SymbolTable = @import("../semantic/symbols.zig").SymbolTable;
const SymbolIndex = @import("../semantic/symbols.zig").SymbolIndex;
const SymbolKind = @import("../semantic/symbols.zig").SymbolKind;

const SourceCode = @import("../source/source.zig").SourceCode;

pub fn print(
    result: *const ComptimeResult,
    symbols: *const SymbolTable,
    src: *const SourceCode,
) void {
    const count = symbols.count();

    if (count == 0) {
        std.debug.print("(no symbols)\n", .{});
        return;
    }

    // print header
    std.debug.print("{s:<5} {s:<16} {s:<12} {s:<12} {s}\n", .{
        "idx",
        "name",
        "kind",
        "eval_state",
        "value",
    });
    std.debug.print("{s:-<5} {s:-<16} {s:-<12} {s:-<12} {s:-<20}\n", .{
        "",
        "",
        "",
        "",
        "",
    });

    // print each symbol's evaluation result
    for (0..count) |i| {
        const idx: SymbolIndex = @intCast(i);
        printSymbol(result, symbols, src, idx);
    }

    // print errors if any
    if (result.errors.hasErrors()) {
        std.debug.print("\nErrors:\n", .{});
        for (result.errors.errors.items) |err| {
            const name = symbols.getName(err.symbol_idx, src);
            std.debug.print("  {s}: {s}\n", .{
                name,
                @tagName(err.kind),
            });
        }
    }
}

fn printSymbol(
    result: *const ComptimeResult,
    symbols: *const SymbolTable,
    src: *const SourceCode,
    idx: SymbolIndex,
) void {
    const name = symbols.getName(idx, src);
    const kind = symbols.getKind(idx);
    const eval_state = result.eval_states.items[idx];

    const kind_str = switch (kind) {
        .constant => "const",
        .variable => "var",
        .function => "func",
        .@"type" => "type",
        .namespace => "namespace",
    };

    const state_str = switch (eval_state) {
        .unevaluated => "unevaluated",
        .evaluating => "evaluating",
        .evaluated => "evaluated",
    };

    const value_str = if (result.getEvalLiteral(idx)) |lit|
        lit
    else
        "-";

    std.debug.print("{d:<5} {s:<16} {s:<12} {s:<12} {s}\n", .{
        idx,
        name,
        kind_str,
        state_str,
        value_str,
    });
}

/// Verbose output with more details
pub fn printVerbose(
    result: *const ComptimeResult,
    symbols: *const SymbolTable,
    src: *const SourceCode,
) void {
    const count = symbols.count();

    if (count == 0) {
        std.debug.print("(no symbols)\n", .{});
        return;
    }

    for (0..count) |i| {
        const idx: SymbolIndex = @intCast(i);
        printSymbolVerbose(result, symbols, src, idx);
        std.debug.print("\n", .{});
    }

    if (result.errors.hasErrors()) {
        std.debug.print("Errors ({d}):\n", .{result.errors.errors.items.len});
        for (result.errors.errors.items) |err| {
            const name = symbols.getName(err.symbol_idx, src);
            std.debug.print("  symbol[{d}] {s}: {s}\n", .{
                err.symbol_idx,
                name,
                @tagName(err.kind),
            });
        }
    }
}

fn printSymbolVerbose(
    result: *const ComptimeResult,
    symbols: *const SymbolTable,
    src: *const SourceCode,
    idx: SymbolIndex,
) void {
    const name = symbols.getName(idx, src);
    const kind = symbols.getKind(idx);
    const eval_state = result.eval_states.items[idx];
    const type_id = symbols.getResolvedType(idx);

    std.debug.print("symbol[{d}]:\n", .{idx});
    std.debug.print("  name:       {s}\n", .{name});
    std.debug.print("  kind:       {s}\n", .{@tagName(kind)});
    std.debug.print("  type:       {s}\n", .{@tagName(type_id)});
    std.debug.print("  eval_state: {s}\n", .{@tagName(eval_state)});

    if (result.getEvalLiteral(idx)) |lit| {
        std.debug.print("  value:      {s}\n", .{lit});
    } else {
        std.debug.print("  value:      (not evaluated)\n", .{});
    }
}
