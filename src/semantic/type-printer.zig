const std = @import("std");

const TypeId = @import("types.zig").TypeId;
const TypeRegistry = @import("types.zig").TypeRegistry;
const FunctionTypeIndex = @import("types.zig").FunctionTypeIndex;
const CallingConvention = @import("../parser/ast.zig").CallingConvention;

pub fn print(registry: *const TypeRegistry) void {
    const func_count = registry.functionTypeCount();
    const struct_count = registry.structTypeCount();

    if (func_count == 0 and struct_count == 0) {
        std.debug.print("(no types)\n", .{});
        return;
    }

    // print header
    std.debug.print("{s:<5} {s:<8} {s:<8} {s}\n", .{ "idx", "cc", "params", "signature" });
    std.debug.print("{s:-<5} {s:-<8} {s:-<8} {s:-<32}\n", .{ "", "", "", "" });

    // print each function type
    for (registry.function_types.items, 0..) |ft, i| {
        var buf: [128]u8 = undefined;
        var stream = std.io.fixedBufferStream(&buf);
        const writer = stream.writer();

        // format calling convention prefix
        const cc_str = switch (ft.calling_conv) {
            .honey => "",
            .c => "c ",
            .fortran => "fortran ",
            .cobol => "cobol ",
        };

        writer.print("{s}func(", .{cc_str}) catch {};
        for (ft.param_types, 0..) |param, j| {
            if (j > 0) writer.writeAll(", ") catch {};
            writer.writeAll(formatTypeId(param)) catch {};
        }
        writer.writeAll(") ") catch {};
        writer.writeAll(formatTypeId(ft.return_type)) catch {};

        const conv_name = @tagName(ft.calling_conv);
        std.debug.print("{d:<5} {s:<8} {d:<8} {s}\n", .{ i, conv_name, ft.param_types.len, stream.getWritten() });
    }

    // print struct types
    if (struct_count > 0) {
        std.debug.print("\n{s:<5} {s:<8} {s:<8} {s:<8} {s}\n", .{ "idx", "cc", "fields", "size", "name" });
        std.debug.print("{s:-<5} {s:-<8} {s:-<8} {s:-<8} {s:-<16}\n", .{ "", "", "", "", "" });

        for (registry.struct_types.items, 0..) |st, i| {
            const conv_name = @tagName(st.calling_conv);
            std.debug.print("{d:<5} {s:<8} {d:<8} {d:<8} {s}\n", .{ i, conv_name, st.fields.len, st.size, st.name });
        }
    }
}

pub fn printVerbose(registry: *const TypeRegistry) void {
    const func_count = registry.functionTypeCount();
    const struct_count = registry.structTypeCount();

    if (func_count == 0 and struct_count == 0) {
        std.debug.print("(no types)\n", .{});
        return;
    }

    for (registry.function_types.items, 0..) |ft, i| {
        std.debug.print("function_type[{d}]:\n", .{i});

        std.debug.print("  convention: {s}\n", .{@tagName(ft.calling_conv)});

        std.debug.print("  params ({d}):\n", .{ft.param_types.len});
        for (ft.param_types, 0..) |param, j| {
            std.debug.print("    [{d}] {s}\n", .{ j, formatTypeId(param) });
        }

        std.debug.print("  return: {s}\n", .{formatTypeId(ft.return_type)});
        std.debug.print("\n", .{});
    }

    for (registry.pointer_types.items, 0..) |pt, i| {
        const kind_str = if (pt.is_many_item) "mptr" else "ptr";
        const mut_str = if (pt.is_mutable) "mut " else "";
        std.debug.print("{s}[{d}]: {s}{s}\n", .{ kind_str, i, mut_str, formatTypeId(pt.pointee) });
    }

    for (registry.struct_types.items, 0..) |st, i| {
        std.debug.print("struct_type[{d}]:\n", .{i});
        std.debug.print("  name:      {s}\n", .{st.name});
        std.debug.print("  convention: {s}\n", .{@tagName(st.calling_conv)});
        std.debug.print("  size:      {d} bytes\n", .{st.size});
        std.debug.print("  alignment: {d}\n", .{st.alignment});

        std.debug.print("  fields ({d}):\n", .{st.fields.len});
        for (st.fields, 0..) |field, j| {
            std.debug.print("    [{d}] {s}: {s} (offset {d})\n", .{ j, field.name, formatTypeId(field.type_id), field.offset });
        }
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
        .struct_type => |idx| blk: {
            const S = struct {
                var buf: [16]u8 = undefined;
            };
            break :blk std.fmt.bufPrint(&S.buf, "struct#{d}", .{idx}) catch "struct#?";
        },
        .pointer => |idx| blk: {
            const S = struct {
                var buf: [16]u8 = undefined;
            };
            break :blk std.fmt.bufPrint(&S.buf, "ptr#{d}", .{idx}) catch "ptr#?";
        },
    };
}
