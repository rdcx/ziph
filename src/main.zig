const std = @import("std");
const repl = @import("repl.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Welcome to Ziph. The PHP Compiler written in Zig!\n", .{});

    try repl.start();

    // const input = "<?php echo 10; ?>";

    // var l = lexer.New(input);

    // while (true) {
    //     const tok = l.nextToken();
    //     if (tok.token_type == token.TokenType.EOF) {
    //         break;
    //     }

    //     try stdout.print("Token: {}\n", .{tok.token_type});
    // }
}
