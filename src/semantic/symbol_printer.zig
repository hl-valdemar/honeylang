const std = @import("std");

const SymbolTable = @import("symbols.zig").SymbolTable;
const SymbolIndex = @import("symbols.zig").SymbolIndex;
const SymbolKind = @import("symbols.zig").SymbolKind;
const TypeState = @import("types.zig").TypeState;
const TypeId = @import("types.zig").TypeId;

const SourceCode = @import("../source/source.zig").SourceCode;

pub fn print(symbols: *const SymbolTable, src: *const SourceCode) void {
    const count = symbols.count();

    if (count == 0) {
        std.debug.print("(no symbols)\n", .{});
        return;
    }

    // print header
    std.debug.print("{s:<4} {s:<16} {s:<10} {s:<12} {s:<8}\n", .{
        "idx",
        "name",
        "kind",
        "type",
        "value",
    });
    std.debug.print("{s:-<4} {s:-<16} {s:-<10} {s:-<12} {s:-<8}\n", .{
        "",
        "",
        "",
        "",
        "",
    });

    // print each symbol
    for (0..count) |i| {
        const idx: SymbolIndex = @intCast(i);
        printSymbol(symbols, src, idx);
    }
}

fn printSymbol(symbols: *const SymbolTable, src: *const SourceCode, idx: SymbolIndex) void {
    const name = symbols.getName(idx, src);
    const kind = symbols.getKind(idx);
    const type_state = symbols.getTypeState(idx);
    const value_node = symbols.getValueNode(idx);

    const kind_str = switch (kind) {
        .constant => "const",
        .function => "func",
    };

    const type_str = switch (type_state) {
        .pending => "<pending>",
        .resolved => @tagName(symbols.getTypeId(idx)),
    };

    std.debug.print("{d:<4} {s:<16} {s:<10} {s:<12} node({d})\n", .{
        idx,
        name,
        kind_str,
        type_str,
        value_node,
    });
}

/// Verbose output with more details.
pub fn printVerbose(symbols: *const SymbolTable, src: *const SourceCode) void {
    const count = symbols.count();

    if (count == 0) {
        std.debug.print("(no symbols)\n", .{});
        return;
    }

    for (0..count) |i| {
        const idx: SymbolIndex = @intCast(i);
        printSymbolVerbose(symbols, src, idx);
        std.debug.print("\n", .{});
    }
}

fn printSymbolVerbose(symbols: *const SymbolTable, src: *const SourceCode, idx: SymbolIndex) void {
    const name = symbols.getName(idx, src);
    const kind = symbols.getKind(idx);
    const type_state = symbols.getTypeState(idx);
    const value_node = symbols.getValueNode(idx);
    const name_start = symbols.name_starts.items[idx];

    std.debug.print("symbol[{d}]:\n", .{idx});
    std.debug.print("  name:       {s}\n", .{name});
    std.debug.print("  kind:       {s}\n", .{@tagName(kind)});
    std.debug.print("  type_state: {s}\n", .{@tagName(type_state)});

    if (type_state == .resolved) {
        const resolved = symbols.getTypeId(idx);
        std.debug.print("  type:       {s}\n", .{@tagName(resolved)});
    }

    std.debug.print("  value_node: {d}\n", .{value_node});
    std.debug.print("  location:   {d}..{d}\n", .{
        name_start,
        name_start + symbols.name_lens.items[idx],
    });
}
