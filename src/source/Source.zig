const std = @import("std");
const mem = std.mem;
const fs = std.fs;

id: ID,
path: ?[]const u8,
contents: []const u8,

const Self = @This();

pub const ID = u16;
pub const Index = @import("../root.zig").BaseIndex;

pub const LineCol = struct {
    line: u32,
    col: u32,
};

pub const init = struct {
    pub fn fromStr(alloc: mem.Allocator, str: []const u8) !Self {
        return .{
            .id = ID_Generator.getNextID(),
            .path = null,
            .contents = try alloc.dupe(u8, str),
        };
    }

    pub fn fromFile(alloc: mem.Allocator, path: []const u8) !Self {
        const file = try fs.cwd().openFile(path, .{ .mode = .read_only });
        defer file.close();

        const file_size = try file.getEndPos();
        const contents = try alloc.alloc(u8, file_size);

        _ = try file.readAll(contents);

        return Self{
            .id = ID_Generator.getNextID(),
            .path = path,
            .contents = contents,
        };
    }
};

pub fn deinit(self: *Self, alloc: mem.Allocator) void {
    alloc.free(self.contents);
}

pub fn lineCol(self: *const Self, offset: u32) LineCol {
    var line: u32 = 1;
    var col: u32 = 1;
    for (0..offset) |i| {
        if (i >= self.contents.len)
            break;

        if (self.contents[i] == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    return LineCol{ .line = line, .col = col };
}

const ID_Generator = struct {
    var nextID: ID = 0;

    fn getNextID() ID {
        defer nextID += 1;
        return nextID;
    }
};
