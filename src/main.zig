const std = @import("std");
const repl = @import("repl.zig");
const flag = @import("flag.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Welcome to Ziph. The PHP Compiler written in Zig!\n", .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var flags = try flag.parse(alloc);

    const file = flags.getFlag("--file");

    if (file) |f| {
        try stdout.print("File: {s}\n", .{f});

        const file_content = try std.fs.cwd().readFileAlloc(gpa.allocator(), f, std.math.maxInt(usize));
        std.debug.print("{s}\n", .{file_content});

        var l = lexer.new(file_content);

        while (true) {
            const tok = l.nextToken();
            if (tok == token.TokenTag.eof) {
                break;
            }

            try stdout.print("{}\n", .{tok});

            if (tok == token.TokenTag.illegal) {
                break;
            }
        }

        return;
    }

    try repl.start();
}
