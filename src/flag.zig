const std = @import("std");

test {
    std.testing.refAllDecls(@This());
}

pub const Flag = struct {
    name: []const u8,
    value: []const u8,
};

pub const Result = struct {
    flags: []Flag,

    pub fn getFlag(self: *Result, name: []const u8) ?[]const u8 {
        for (self.flags) |f| {
            if (std.mem.eql(u8, f.name, name)) {
                return f.value;
            }
        }

        return null;
    }

    test "getFlag" {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const alloc = gpa.allocator();

        var flags = try alloc.alloc(Flag, 2);

        flags[0] = Flag{ .name = "--file", .value = "test.php" };
        flags[1] = Flag{ .name = "--output", .value = "output.php" };

        var result = Result{ .flags = flags };

        const file = result.getFlag("--file");
        const output = result.getFlag("--output");

        if (file) |f| {
            try std.testing.expectEqual(f, "test.php");
        }

        if (output) |o| {
            try std.testing.expectEqual(o, "output.php");
        }
    }
};

pub fn parse(alloc: std.mem.Allocator) !Result {
    var flags = try alloc.alloc(Flag, 1);

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
            flags[0].name = vals[ii];
            flags[0].value = vals[ii + 1];
        }
    }

    return Result{ .flags = flags };
}
