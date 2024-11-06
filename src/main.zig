const std = @import("std");
const repl = @import("repl.zig");
const flag = @import("flag.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Welcome to Ziph. The PHP Compiler written in Zig!\n", .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    const flags = try flag.parse(alloc);

    for (flags) |f| {
        try stdout.print("Flag: {s} Value: {s}\n", .{ f.name, f.value });
    }

    // try repl.start();
}
