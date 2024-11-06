const std = @import("std");

pub const Flag = struct {
    name: []const u8,
    value: []const u8,
};

pub fn parse(alloc: std.mem.Allocator) ![]Flag {
    var result = try alloc.alloc(Flag, 1);

    // Parse args into string array (error union needs 'try')
    const args = try std.process.argsAlloc(alloc);

    var vals = try alloc.alloc([]const u8, 100);

    var i: u8 = 0;
    for (args) |arg| {
        vals[i] = arg;
        std.debug.print("Arg: {s}\n", .{arg});
        i += 1;
    }

    for (0..5) |ii| {
        if (std.mem.eql(u8, vals[ii], "")) {
            break;
        }

        if (std.mem.eql(u8, vals[ii], "--file")) {
            result[0].name = vals[ii];
            result[0].value = vals[ii + 1];
        }
    }

    return result;
}
