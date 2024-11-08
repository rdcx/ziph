const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const Env = @import("env.zig").Environment;
const Evaluator = @import("eval.zig").Evaluator;
const Parser = @import("parser.zig").Parser;
const String = @import("string.zig").String;

pub fn start() !void {
    const alloc = std.heap.page_allocator;

    const memory = try alloc.alloc(u8, 100);
    defer alloc.free(memory);

    var rootAllocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer rootAllocator.deinit();
    const allocator = rootAllocator.allocator();

    var env = Env.new(allocator);
    var evaluator = Evaluator.new(allocator);

    while (true) {
        const in = try stdin.readUntilDelimiterAlloc(alloc, '\n', 100);

        var lex = lexer.new(in);
        var parser = Parser.new(&lex, allocator);

        var program = parser.parseProgram() catch |err| {
            try stdout.print("Error parsing program: {}\n", .{err});
            continue;
        };

        const object = evaluator.evalProgram(&program, &env) catch |err| {
            try stdout.print("Error evaluating program: {}\n", .{err});
            continue;
        };

        switch (object.*) {
            .error_ => try stdout.print("Error: {}\n", .{object}),
            else => {
                var objPrintBuf = String.init(allocator);
                try object.toString(&objPrintBuf);
                try stdout.print("{s}\n", .{objPrintBuf.str()});
            },
        }
    }
}
