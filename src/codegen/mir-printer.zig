const std = @import("std");

const mir = @import("mir.zig");
const MIRModule = mir.MIRModule;
const MIRFunction = mir.MIRFunction;
const MInst = mir.MInst;
const Width = mir.Width;
const BinOp = mir.BinOp;
const CmpOp = mir.CmpOp;
const VReg = mir.VReg;

pub fn print(module: *const MIRModule) void {
    const func_count = module.functions.items.len;

    if (func_count == 0) {
        std.debug.print("(no functions)\n", .{});
        return;
    }

    // print summary header
    std.debug.print("{s:<5} {s:<20} {s:<8} {s:<8} {s}\n", .{
        "idx",
        "name",
        "cc",
        "instrs",
        "vregs",
    });
    std.debug.print("{s:-<5} {s:-<20} {s:-<8} {s:-<8} {s:-<8}\n", .{
        "",
        "",
        "",
        "",
        "",
    });

    // print function summary
    for (module.functions.items, 0..) |func, i| {
        const cc_str: []const u8 = @tagName(func.call_conv);
        std.debug.print("{d:<5} {s:<20} {s:<8} {d:<8} {d}\n", .{
            i,
            func.name,
            cc_str,
            func.instructions.items.len,
            func.next_vreg,
        });
    }

    // print detailed instructions for each function
    std.debug.print("\n", .{});
    for (module.functions.items, 0..) |func, i| {
        printFunction(&func, i);
    }
}

fn printFunction(func: *const MIRFunction, idx: usize) void {
    const cc_str: []const u8 = @tagName(func.call_conv);
    std.debug.print("func[{d}] {s} (cc: {s}, vregs: {d}):\n", .{
        idx,
        func.name,
        cc_str,
        func.next_vreg,
    });

    if (func.instructions.items.len == 0) {
        std.debug.print("  (no instructions)\n\n", .{});
        return;
    }

    for (func.instructions.items, 0..) |inst, i| {
        std.debug.print("  {d:>3}: ", .{i});
        printInstruction(&inst);
        std.debug.print("\n", .{});
    }
    std.debug.print("\n", .{});
}

fn printInstruction(inst: *const MInst) void {
    switch (inst.*) {
        .mov_imm => |m| {
            std.debug.print("mov.{s} v{d}, #{d}", .{
                widthStr(m.width),
                m.dst,
                m.value,
            });
        },
        .mov_reg => |m| {
            std.debug.print("mov.{s} v{d}, v{d}", .{
                widthStr(m.width),
                m.dst,
                m.src,
            });
        },
        .binop => |b| {
            std.debug.print("{s}.{s} v{d}, v{d}, v{d}", .{
                binopStr(b.op),
                widthStr(b.width),
                b.dst,
                b.lhs,
                b.rhs,
            });
        },
        .cmp => |c| {
            std.debug.print("cmp.{s}.{s} v{d}, v{d}, v{d}", .{
                cmpopStr(c.op),
                widthStr(c.width),
                c.dst,
                c.lhs,
                c.rhs,
            });
        },
        .ret => |r| {
            if (r.value) |v| {
                std.debug.print("ret.{s} v{d}", .{ widthStr(r.width), v });
            } else {
                std.debug.print("ret", .{});
            }
        },
        .prologue => {
            std.debug.print("prologue", .{});
        },
        .epilogue => {
            std.debug.print("epilogue", .{});
        },
        .load_global => |g| {
            std.debug.print("load.{s} v{d}, global[{d}]", .{
                widthStr(g.width),
                g.dst,
                g.global_idx,
            });
        },
        .store_global => |g| {
            std.debug.print("store.{s} global[{d}], v{d}", .{
                widthStr(g.width),
                g.global_idx,
                g.src,
            });
        },
        .load_local => |l| {
            std.debug.print("load.{s} v{d}, [fp{d}]", .{
                widthStr(l.width),
                l.dst,
                l.offset,
            });
        },
        .store_local => |l| {
            std.debug.print("store.{s} [fp{d}], v{d}", .{
                widthStr(l.width),
                l.offset,
                l.src,
            });
        },
        .store_arg => |s| {
            std.debug.print("store_arg.{s} [fp{d}], arg{d}", .{
                widthStr(s.width),
                s.offset,
                s.arg_idx,
            });
        },
        .call => |c| {
            if (c.dst) |dst| {
                std.debug.print("call.{s} v{d} = {s}(", .{ widthStr(c.width), dst, c.func_name });
            } else {
                std.debug.print("call void {s}(", .{c.func_name});
            }
            for (c.args, 0..) |arg, i| {
                if (i > 0) std.debug.print(", ", .{});
                std.debug.print("v{d}", .{arg});
            }
            std.debug.print(")", .{});
        },
        .label => |id| {
            std.debug.print("L{d}:", .{id});
        },
        .br_cond => |b| {
            std.debug.print("br_cond v{d}, L{d}, L{d}", .{ b.cond, b.true_label, b.false_label });
        },
        .br => |target| {
            std.debug.print("br L{d}", .{target});
        },
        .load_field => |f| {
            std.debug.print("load_field.{s} v{d}, v{d}, struct[{d}].field[{d}]", .{
                widthStr(f.width),
                f.dst,
                f.base,
                f.struct_idx,
                f.field_idx,
            });
        },
        .alloca_struct => |a| {
            std.debug.print("alloca_struct v{d}, struct[{d}]", .{
                a.dst,
                a.struct_idx,
            });
        },
        .store_field => |f| {
            std.debug.print("store_field.{s} v{d}, struct[{d}].field[{d}], v{d}", .{
                widthStr(f.width),
                f.base,
                f.struct_idx,
                f.field_idx,
                f.value,
            });
        },
        .copy_struct => |c| {
            std.debug.print("copy_struct v{d}, v{d}, struct[{d}]", .{
                c.dst,
                c.src,
                c.struct_idx,
            });
        },
    }
}

fn widthStr(w: Width) []const u8 {
    return switch (w) {
        .w32 => "w32",
        .w64 => "w64",
        .ptr => "ptr",
    };
}

fn binopStr(op: BinOp) []const u8 {
    return switch (op) {
        .add => "add",
        .sub => "sub",
        .mul => "mul",
        .div_s => "sdiv",
        .div_u => "udiv",
        .mod_s => "smod",
        .mod_u => "umod",
        .bit_and => "and",
        .bit_or => "or",
        .bit_xor => "xor",
        .shl => "shl",
        .shr_s => "ashr",
        .shr_u => "lshr",
    };
}

fn cmpopStr(op: CmpOp) []const u8 {
    return switch (op) {
        .eq => "eq",
        .ne => "ne",
        .lt_s => "slt",
        .lt_u => "ult",
        .le_s => "sle",
        .le_u => "ule",
        .gt_s => "sgt",
        .gt_u => "ugt",
        .ge_s => "sge",
        .ge_u => "uge",
    };
}
