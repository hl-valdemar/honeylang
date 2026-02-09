const std = @import("std");
const mem = std.mem;

const CallingConvention = @import("../parser/ast.zig").CallingConvention;

pub const PrimitiveType = enum(u8) {
    void,
    bool,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    f16,
    f32,
    f64,

    pub fn isInteger(self: PrimitiveType) bool {
        return switch (self) {
            .u8, .u16, .u32, .u64, .i8, .i16, .i32, .i64 => true,
            else => false,
        };
    }

    pub fn isFloat(self: PrimitiveType) bool {
        return switch (self) {
            .f16, .f32, .f64 => true,
            else => false,
        };
    }

    pub fn isNumeric(self: PrimitiveType) bool {
        return self.isInteger() or self.isFloat();
    }

    pub fn isSignedInteger(self: PrimitiveType) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64 => true,
            else => false,
        };
    }
};

pub const FunctionTypeIndex = u32;
pub const StructTypeIndex = u32;

pub const TypeId = union(enum) {
    /// Type could not be determined, will trap at runtime
    unresolved,

    /// Primitive types (void, bool, integers, floats)
    primitive: PrimitiveType,

    /// Function type, index into TypeRegistry.function_types
    function: FunctionTypeIndex,

    /// Struct type, index into TypeRegistry.struct_types
    struct_type: StructTypeIndex,

    pub const @"void": TypeId = .{ .primitive = .void };
    pub const @"bool": TypeId = .{ .primitive = .bool };
    pub const @"u8": TypeId = .{ .primitive = .u8 };
    pub const @"u16": TypeId = .{ .primitive = .u16 };
    pub const @"u32": TypeId = .{ .primitive = .u32 };
    pub const @"u64": TypeId = .{ .primitive = .u64 };
    pub const @"i8": TypeId = .{ .primitive = .i8 };
    pub const @"i16": TypeId = .{ .primitive = .i16 };
    pub const @"i32": TypeId = .{ .primitive = .i32 };
    pub const @"i64": TypeId = .{ .primitive = .i64 };
    pub const @"f16": TypeId = .{ .primitive = .f16 };
    pub const @"f32": TypeId = .{ .primitive = .f32 };
    pub const @"f64": TypeId = .{ .primitive = .f64 };

    pub fn isUnresolved(self: TypeId) bool {
        return self == .unresolved;
    }

    pub fn isVoid(self: TypeId) bool {
        return switch (self) {
            .primitive => |p| p == .void,
            else => false,
        };
    }

    pub fn isBool(self: TypeId) bool {
        return switch (self) {
            .primitive => |p| p == .bool,
            else => false,
        };
    }

    pub fn isInteger(self: TypeId) bool {
        return switch (self) {
            .primitive => |p| p.isInteger(),
            else => false,
        };
    }

    pub fn isFloat(self: TypeId) bool {
        return switch (self) {
            .primitive => |p| p.isFloat(),
            else => false,
        };
    }

    pub fn isNumeric(self: TypeId) bool {
        return switch (self) {
            .primitive => |p| p.isNumeric(),
            else => false,
        };
    }

    pub fn isSignedInteger(self: TypeId) bool {
        return switch (self) {
            .primitive => |p| p.isSignedInteger(),
            else => false,
        };
    }

    pub fn isFunction(self: TypeId) bool {
        return self == .function;
    }

    pub fn isStruct(self: TypeId) bool {
        return self == .struct_type;
    }

    pub fn eql(self: TypeId, other: TypeId) bool {
        return std.meta.eql(self, other);
    }
};

pub const FunctionType = struct {
    param_types: []const TypeId,
    return_type: TypeId,
    calling_conv: CallingConvention,
};

pub const StructField = struct {
    name: []const u8,
    type_id: TypeId,
    offset: u32, // byte offset (C layout)
};

pub const StructType = struct {
    name: []const u8,
    fields: []const StructField,
    size: u32, // total size in bytes
    alignment: u32, // required alignment
    calling_conv: CallingConvention,
    finalized: bool = false,
};

/// Type Registry (Storage for Composite Types)
pub const TypeRegistry = struct {
    allocator: mem.Allocator,
    function_types: std.ArrayList(FunctionType),
    struct_types: std.ArrayList(StructType),
    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: mem.Allocator) !TypeRegistry {
        return .{
            .allocator = allocator,
            .function_types = try std.ArrayList(FunctionType).initCapacity(allocator, 16),
            .struct_types = try std.ArrayList(StructType).initCapacity(allocator, 4),
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *TypeRegistry) void {
        self.function_types.deinit(self.allocator);
        self.struct_types.deinit(self.allocator);
        self.arena.deinit();
    }

    /// Register a function type and return a TypeId for it.
    /// The param_types slice is copied into the registry's arena.
    pub fn addFunctionType(
        self: *TypeRegistry,
        params: []const TypeId,
        ret: TypeId,
        calling_conv: CallingConvention,
    ) !TypeId {
        // copy params into arena
        const arena_alloc = self.arena.allocator();
        const params_copy = try arena_alloc.dupe(TypeId, params);

        const idx: FunctionTypeIndex = @intCast(self.function_types.items.len);
        try self.function_types.append(self.allocator, .{
            .param_types = params_copy,
            .return_type = ret,
            .calling_conv = calling_conv,
        });

        return .{ .function = idx };
    }

    /// Get the function type for a given index.
    pub fn getFunctionType(self: *const TypeRegistry, type_id: TypeId) ?FunctionType {
        return switch (type_id) {
            .function => |idx| self.function_types.items[idx],
            else => null,
        };
    }

    /// Get return type of a function type.
    pub fn getReturnType(self: *const TypeRegistry, type_id: TypeId) ?TypeId {
        if (self.getFunctionType(type_id)) |func_type| {
            return func_type.return_type;
        }
        return null;
    }

    /// Get parameter types of a function type.
    pub fn getParamTypes(self: *const TypeRegistry, type_id: TypeId) ?[]const TypeId {
        if (self.getFunctionType(type_id)) |func_type| {
            return func_type.param_types;
        }
        return null;
    }

    pub fn functionTypeCount(self: *const TypeRegistry) usize {
        return self.function_types.items.len;
    }

    /// Register a struct type and return a TypeId for it.
    pub fn addStructType(
        self: *TypeRegistry,
        name: []const u8,
        field_names: []const []const u8,
        field_types: []const TypeId,
        calling_conv: CallingConvention,
    ) !TypeId {
        const layout = try self.computeStructLayout(field_names, field_types, calling_conv);

        const idx: StructTypeIndex = @intCast(self.struct_types.items.len);
        try self.struct_types.append(self.allocator, .{
            .name = name,
            .fields = layout.fields,
            .size = layout.size,
            .alignment = layout.alignment,
            .calling_conv = calling_conv,
            .finalized = true,
        });

        return .{ .struct_type = idx };
    }

    /// Get the struct type for a given TypeId.
    pub fn getStructType(self: *const TypeRegistry, type_id: TypeId) ?StructType {
        return switch (type_id) {
            .struct_type => |idx| self.struct_types.items[idx],
            else => null,
        };
    }

    /// Reserve a slot for a struct type (forward declaration).
    /// Returns the TypeId that will refer to this struct once finalized.
    pub fn reserveStructType(self: *TypeRegistry, name: []const u8) !TypeId {
        const idx: StructTypeIndex = @intCast(self.struct_types.items.len);
        try self.struct_types.append(self.allocator, .{
            .name = name,
            .fields = &.{},
            .size = 0,
            .alignment = 1,
            .calling_conv = .c,
        });
        return .{ .struct_type = idx };
    }

    /// Finalize a previously reserved struct type with field data and layout.
    pub fn finalizeStructType(
        self: *TypeRegistry,
        type_id: TypeId,
        field_names: []const []const u8,
        field_types: []const TypeId,
        calling_conv: CallingConvention,
    ) !void {
        const idx = type_id.struct_type;
        const layout = try self.computeStructLayout(field_names, field_types, calling_conv);

        self.struct_types.items[idx] = .{
            .name = self.struct_types.items[idx].name,
            .fields = layout.fields,
            .size = layout.size,
            .alignment = layout.alignment,
            .calling_conv = calling_conv,
            .finalized = true,
        };
    }

    const StructLayout = struct {
        fields: []StructField,
        size: u32,
        alignment: u32,
    };

    fn computeStructLayout(
        self: *TypeRegistry,
        field_names: []const []const u8,
        field_types: []const TypeId,
        calling_conv: CallingConvention,
    ) !StructLayout {
        // all conventions currently use C layout
        // TODO: implement native honey struct layout
        return switch (calling_conv) {
            .honey, .c, .cobol, .fortran => try self.computeCLayout(field_names, field_types),
        };
    }

    fn computeCLayout(
        self: *TypeRegistry,
        field_names: []const []const u8,
        field_types: []const TypeId,
    ) !StructLayout {
        const arena_alloc = self.arena.allocator();

        var offset: u32 = 0;
        var max_align: u32 = 1;
        var fields = try arena_alloc.alloc(StructField, field_names.len);

        for (field_names, field_types, 0..) |fname, ftype, i| {
            const field_align = alignmentOf(ftype, self);
            const field_size = sizeOf(ftype, self);
            offset = alignUp(offset, field_align);

            fields[i] = .{
                .name = fname,
                .type_id = ftype,
                .offset = offset,
            };

            offset += field_size;
            if (field_align > max_align) max_align = field_align;
        }

        return .{
            .fields = fields,
            .size = alignUp(offset, max_align),
            .alignment = max_align,
        };
    }

    pub fn structTypeCount(self: *const TypeRegistry) usize {
        return self.struct_types.items.len;
    }
};

/// Size in bytes of a type (for C layout computation).
pub fn sizeOf(type_id: TypeId, types: *const TypeRegistry) u32 {
    return switch (type_id) {
        .primitive => |p| switch (p) {
            .void => 0,
            .bool, .u8, .i8 => 1,
            .u16, .i16, .f16 => 2,
            .u32, .i32, .f32 => 4,
            .u64, .i64, .f64 => 8,
        },
        .unresolved => 4, // fallback
        .function => 8, // pointer-sized
        .struct_type => |idx| types.struct_types.items[idx].size,
    };
}

/// Alignment in bytes of a type (for C layout computation).
pub fn alignmentOf(type_id: TypeId, types: *const TypeRegistry) u32 {
    return switch (type_id) {
        .primitive => |p| switch (p) {
            .void => 1,
            .bool, .u8, .i8 => 1,
            .u16, .i16, .f16 => 2,
            .u32, .i32, .f32 => 4,
            .u64, .i64, .f64 => 8,
        },
        .unresolved => 4,
        .function => 8,
        .struct_type => |idx| types.struct_types.items[idx].alignment,
    };
}

fn alignUp(offset: u32, alignment: u32) u32 {
    return (offset + alignment - 1) & ~(alignment - 1);
}

pub const TypeState = enum(u8) {
    /// Awaiting inference from context.
    pending,

    /// Type is known (use getResolved() on symbol table).
    resolved,
};
