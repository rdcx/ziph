const std = @import("std");
const repl = @import("repl.zig");
const flag = @import("flag.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const Parser = @import("parser.zig").Parser;
const Evaluator = @import("eval.zig").Evaluator;
const Env = @import("env.zig").Environment;
const String = @import("string.zig").String;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var flags = try flag.parse(alloc);

    const file = flags.getFlag("--file");

    if (file) |f| {
        var env = Env.new(alloc);
        var evaluator = Evaluator.new(alloc);

        const file_content = try std.fs.cwd().readFileAlloc(gpa.allocator(), f, std.math.maxInt(usize));

        var l = lexer.new(file_content);

        var parser = Parser.new(&l, alloc);

        var program = parser.parseProgram() catch |err| {
            try stdout.print("Error parsing program: {}\n", .{err});
            return;
        };

        const object = evaluator.evalProgram(&program, &env) catch |err| {
            try stdout.print("Error evaluating program: {}\n", .{err});
            return;
        };

        switch (object.*) {
            .error_ => try stdout.print("Error: {s}\n", .{object.error_.message}),

            else => {
                var objPrintBuf = String.init(alloc);
                try object.toString(&objPrintBuf);
                try stdout.print("{s}\n", .{objPrintBuf.str()});
            },
        }

        return;
    }

    try stdout.print("Welcome to Ziph. The PHP Compiler written in Zig!\n", .{});

    try repl.start();
}
