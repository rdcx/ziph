const std = @import("std");
const repl = @import("repl.zig");
const flag = @import("flag.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Welcome to Ziph. The PHP Compiler written in Zig!\n", .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var flags = try flag.parse(alloc);

    const file = flags.getFlag("--file");

    if (file) |f| {
        try stdout.print("File: {s}\n", .{f});
    }

    // try repl.start();
}
