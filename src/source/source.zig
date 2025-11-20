const fs = @import("std").fs;
const mem = @import("std").mem;

pub fn fromStr(src: []const u8) SourceCode {
    return .{ .buffer = src };
}

pub fn fromFile(allocator: mem.Allocator, file_path: []const u8) !SourceCode {
    // open file as read only
    const file = try fs.cwd().openFile(file_path, .{ .mode = .read_only });
    defer file.close();

    // get size of file and  allocate a buffer
    const file_size = try file.getEndPos();
    const buffer = try allocator.alloc(u8, file_size);

    // read file into buffer
    _ = try file.readAll(buffer);

    return .{ .buffer = buffer };
}

pub const Idx = usize;

pub const Range = struct {
    start: Idx,
    end: Idx,

    pub fn from(start: Idx, end: Idx) Range {
        return .{ .start = start, .end = end };
    }
};

pub const SourceCode = struct {
    buffer: []u8,

    pub fn deinit(self: *const SourceCode, allocator: mem.Allocator) void {
        allocator.free(self.buffer);
    }

    pub fn get(self: *const SourceCode, idx: Idx) ?u8 {
        if (idx < self.buffer.len) return self.buffer[idx] else return null;
    }

    pub fn getSlice(self: *const SourceCode, start: Idx, end: Idx) []u8 {
        return self.buffer[start..end];
    }

    pub fn getSliceFromRange(self: *const SourceCode, range: Range) []u8 {
        return self.buffer[range.start..range.end];
    }
};
