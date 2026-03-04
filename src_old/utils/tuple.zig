const std = @import("std");

/// Cached field names for common indices (0-7).
const cached = [_][]const u8{ "0", "1", "2", "3", "4", "5", "6", "7" };

/// Get the positional field name for a tuple index.
/// Returns a cached string for indices 0-7, allocates for larger indices.
pub fn fieldName(allocator: std.mem.Allocator, index: usize) ![]const u8 {
    if (index < cached.len) return cached[index];
    return try std.fmt.allocPrint(allocator, "{d}", .{index});
}
