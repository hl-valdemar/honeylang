const std = @import("std");
const fs = @import("std").fs;
const mem = @import("std").mem;

pub fn fromStr(src: []const u8, id: Id) SourceCode {
    return .{ .buffer = src, .id = id };
}

pub fn fromFile(allocator: mem.Allocator, file_path: []const u8, id: Id) !SourceCode {
    // open file as read only
    const file = try fs.cwd().openFile(file_path, .{ .mode = .read_only });
    defer file.close();

    // get size of file and  allocate a buffer
    const file_size = try file.getEndPos();
    const buffer = try allocator.alloc(u8, file_size);

    // read file into buffer
    _ = try file.readAll(buffer);

    return .{ .file_path = file_path, .buffer = buffer, .id = id };
}

pub const Id = u16; // allows for more than 65k files
pub const SourceIndex = u32; // allows for more than 4m characters (in each file)

pub const SourceCode = struct {
    file_path: ?[]const u8 = null,
    buffer: []const u8,
    id: Id,

    pub fn get(self: *const SourceCode, idx: SourceIndex) ?u8 {
        if (idx < self.buffer.len) return self.buffer[idx] else return null;
    }

    pub fn getSlice(self: *const SourceCode, start: SourceIndex, end: SourceIndex) []const u8 {
        return self.buffer[start..end];
    }
};
