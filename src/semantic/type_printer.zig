const std = @import("std");

const TypeId = @import("types.zig").TypeId;
const TypeRegistry = @import("types.zig").TypeRegistry;
const FunctionTypeIndex = @import("types.zig").FunctionTypeIndex;

pub fn print(registry: *const TypeRegistry) void {
    const count = registry.functionTypeCount();

    if (count == 0) {
        std.debug.print("(no function types)\n", .{});
        return;
    }

    // print header
    std.debug.print("{s:<5} {s:<8} {s}\n", .{ "idx", "params", "signature" });
    std.debug.print("{s:-<5} {s:-<8} {s:-<32}\n", .{ "", "", "" });

    // print each function type
    for (registry.function_types.items, 0..) |ft, i| {
        var buf: [128]u8 = undefined;
        var stream = std.io.fixedBufferStream(&buf);
        const writer = stream.writer();

        writer.writeAll("func(") catch {};
        for (ft.param_types, 0..) |param, j| {
            if (j > 0) writer.writeAll(", ") catch {};
            writer.writeAll(formatTypeId(param)) catch {};
        }
        writer.writeAll(") ") catch {};
        writer.writeAll(formatTypeId(ft.return_type)) catch {};

        std.debug.print("{d:<5} {d:<8} {s}\n", .{ i, ft.param_types.len, stream.getWritten() });
    }
}

pub fn printVerbose(registry: *const TypeRegistry) void {
    const count = registry.functionTypeCount();

    if (count == 0) {
        std.debug.print("(no function types)\n", .{});
        return;
    }

    for (registry.function_types.items, 0..) |ft, i| {
        std.debug.print("function_type[{d}]:\n", .{i});

        std.debug.print("  params ({d}):\n", .{ft.param_types.len});
        for (ft.param_types, 0..) |param, j| {
            std.debug.print("    [{d}] {s}\n", .{ j, formatTypeId(param) });
        }

        std.debug.print("  return: {s}\n", .{formatTypeId(ft.return_type)});
        std.debug.print("\n", .{});
    }
}

fn formatTypeId(type_id: TypeId) []const u8 {
    return switch (type_id) {
        .unresolved => "<unresolved>",
        .primitive => |p| @tagName(p),
        .function => |idx| blk: {
            const S = struct {
                var buf: [16]u8 = undefined;
            };
            break :blk std.fmt.bufPrint(&S.buf, "func#{d}", .{idx}) catch "func#?";
        },
    };
}
