const std = @import("std");
const mem = std.mem;
const fs = std.fs;

const Self = @This();

const Source = @import("Source.zig");

var next_id: Source.ID = 0;

pub const init = struct {
    pub fn fromStr(alloc: mem.Allocator, str: []const u8) !Source {
        next_id += 1;
        return .{
            .id = next_id - 1,
            .path = null,
            .contents = try alloc.dupe(u8, str),
        };
    }

    pub fn fromFile(alloc: mem.Allocator, path: []const u8) !Source {
        const file = try fs.cwd().openFile(path, .{ .mode = .read_only });
        defer file.close();

        const file_size = try file.getEndPos();
        const contents = try alloc.alloc(u8, file_size);

        _ = try file.readAll(contents);

        next_id += 1;
        return .{
            .id = next_id - 1,
            .path = path,
            .contents = contents,
        };
    }
};
