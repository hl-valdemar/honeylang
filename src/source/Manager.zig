const std = @import("std");
const mem = std.mem;

const Self = @This();

const Source = @import("Source.zig");

var next_id: u16 = 0;

pub const init = struct {
    pub fn fromStr(alloc: mem.Allocator, str: []const u8) !Source {
        const source_id = nextSourceId();
        return .{
            .id = source_id,
            .path = null,
            .contents = try alloc.dupe(u8, str),
        };
    }

    pub fn fromFile(alloc: mem.Allocator, io: std.Io, path: []const u8) !Source {
        const contents = try std.Io.Dir.cwd().readFileAlloc(io, path, alloc, .limited(std.math.maxInt(usize)));

        const source_id = nextSourceId();
        return .{
            .id = source_id,
            .path = path,
            .contents = contents,
        };
    }
};

fn nextSourceId() Source.ID {
    const source_id = Source.ID.fromInt(next_id);
    next_id += 1;
    return source_id;
}
