const std = @import("std");
const mem = @import("std").mem;

const TypeState = @import("types.zig").TypeState;
const TypeId = @import("types.zig").TypeId;

const NodeIndex = @import("../parser/ast.zig").NodeIndex;

const SourceCode = @import("../source/source.zig").SourceCode;
const SourceIndex = @import("../source/source.zig").SourceIndex;

pub const LocalSymbol = struct {
    type_id: TypeId,
    is_mutable: bool,
    node_idx: NodeIndex = 0,
    referenced: bool = false,
};

pub const Scope = struct {
    locals: std.StringHashMap(LocalSymbol),

    pub fn init(allocator: mem.Allocator) Scope {
        return .{ .locals = std.StringHashMap(LocalSymbol).init(allocator) };
    }

    pub fn deinit(self: *Scope) void {
        self.locals.deinit();
    }
};

pub const SymbolIndex = u32;

pub const SymbolKind = enum(u8) {
    constant, // comptime constant (`X :: expr`, can be typed)
    variable, // runtime global (`x := expr` or `mut x := expr`, can be typed)
    function,
};

pub const SymbolTable = struct {
    allocator: mem.Allocator,

    // parallel arrays (all same length)
    kinds: std.ArrayList(SymbolKind),
    type_states: std.ArrayList(TypeState),
    type_ids: std.ArrayList(TypeId),
    value_nodes: std.ArrayList(NodeIndex),
    mutabilities: std.ArrayList(bool),
    referenced: std.ArrayList(bool),

    // name location in source code
    name_starts: std.ArrayList(SourceIndex),
    name_lengths: std.ArrayList(u16),

    // name lookup
    name_map: std.StringHashMap(SymbolIndex),

    pub fn init(allocator: mem.Allocator) !SymbolTable {
        const capacity = 100;
        return .{
            .allocator = allocator,
            .kinds = try std.ArrayList(SymbolKind).initCapacity(allocator, capacity),
            .type_states = try std.ArrayList(TypeState).initCapacity(allocator, capacity),
            .type_ids = try std.ArrayList(TypeId).initCapacity(allocator, capacity),
            .value_nodes = try std.ArrayList(NodeIndex).initCapacity(allocator, capacity),
            .mutabilities = try std.ArrayList(bool).initCapacity(allocator, capacity),
            .referenced = try std.ArrayList(bool).initCapacity(allocator, capacity),
            .name_starts = try std.ArrayList(SourceIndex).initCapacity(allocator, capacity),
            .name_lengths = try std.ArrayList(u16).initCapacity(allocator, capacity),
            .name_map = std.StringHashMap(SymbolIndex).init(allocator),
        };
    }

    pub fn deinit(self: *SymbolTable) void {
        self.kinds.deinit(self.allocator);
        self.type_states.deinit(self.allocator);
        self.type_ids.deinit(self.allocator);
        self.value_nodes.deinit(self.allocator);
        self.mutabilities.deinit(self.allocator);
        self.referenced.deinit(self.allocator);
        self.name_starts.deinit(self.allocator);
        self.name_lengths.deinit(self.allocator);
        self.name_map.deinit();
    }

    pub fn register(
        self: *SymbolTable,
        name: []const u8,
        name_start: SourceIndex,
        kind: SymbolKind,
        type_state: TypeState,
        type_id: TypeId, // .unresolved if pending
        value_node: NodeIndex,
        is_mutable: bool,
    ) !?SymbolIndex {
        if (self.name_map.contains(name)) {
            return null;
        }

        const idx: SymbolIndex = @intCast(self.kinds.items.len);

        try self.kinds.append(self.allocator, kind);
        try self.type_states.append(self.allocator, type_state);
        try self.type_ids.append(self.allocator, type_id);
        try self.value_nodes.append(self.allocator, value_node);
        try self.mutabilities.append(self.allocator, is_mutable);
        try self.referenced.append(self.allocator, false);
        try self.name_starts.append(self.allocator, name_start);
        try self.name_lengths.append(self.allocator, @intCast(name.len));
        try self.name_map.put(name, idx);

        return idx;
    }

    pub fn lookup(self: *const SymbolTable, name: []const u8) ?SymbolIndex {
        return self.name_map.get(name);
    }

    pub fn count(self: *const SymbolTable) usize {
        return self.kinds.items.len;
    }

    pub fn getKind(self: *const SymbolTable, idx: SymbolIndex) SymbolKind {
        return self.kinds.items[idx];
    }

    pub fn getTypeState(self: *const SymbolTable, idx: SymbolIndex) TypeState {
        return self.type_states.items[idx];
    }

    pub fn getTypeId(self: *const SymbolTable, idx: SymbolIndex) TypeId {
        return self.type_ids.items[idx];
    }

    pub fn getValueNode(self: *const SymbolTable, idx: SymbolIndex) NodeIndex {
        return self.value_nodes.items[idx];
    }

    pub fn isMutable(self: *const SymbolTable, idx: SymbolIndex) bool {
        return self.mutabilities.items[idx];
    }

    pub fn getName(self: *const SymbolTable, idx: SymbolIndex, src: *const SourceCode) []const u8 {
        const start = self.name_starts.items[idx];
        const len = self.name_lengths.items[idx];
        return src.getSlice(start, start + len);
    }

    pub fn resolve(self: *SymbolTable, idx: SymbolIndex, type_id: TypeId) void {
        self.type_ids.items[idx] = type_id;
        self.type_states.items[idx] = .resolved;
    }

    pub fn markReferenced(self: *SymbolTable, idx: SymbolIndex) void {
        self.referenced.items[idx] = true;
    }

    pub fn isReferenced(self: *const SymbolTable, idx: SymbolIndex) bool {
        return self.referenced.items[idx];
    }
};
