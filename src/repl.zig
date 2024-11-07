const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const token = @import("token.zig");
const lexer = @import("lexer.zig");

pub fn start() !void {
    const alloc = std.heap.page_allocator;

    const memory = try alloc.alloc(u8, 100);
    defer alloc.free(memory);

    while (true) {
        const in = try stdin.readUntilDelimiterAlloc(alloc, '\n', 100);

        var l = lexer.new(in);

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
    }
}
