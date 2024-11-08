const std = @import("std");

test {
    std.testing.refAllDecls(@This());
}

const string = @import("string.zig");

const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const object = @import("object.zig");
const builtin = @import("builtin.zig");
const Env = @import("env.zig").Environment;
const EvalError = @import("eval_error.zig").EvaluatorError;
const util = @import("eval_util.zig");

pub const Evaluator = struct {
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn new(alloc: std.mem.Allocator) Evaluator {
        return Evaluator{
            .allocator = alloc,
        };
    }

    fn evalNode(self: *Self, node: ast.Node) !void {
        return switch (node) {
            .program => |*program| try self.evalProgram(program),
            .statement => |*statement| try self.evalStatement(statement),
            .expression => |*expression| try self.evalExpression(expression),
        };
    }

    pub fn evalProgram(self: *Self, program: *ast.Program, env: *Env) EvalError!*object.Object {
        var result: *object.Object = &builtin.NULL_OBJECT;

        var i: usize = 0;
        while (i < program.statements.items.len) : (i += 1) {
            const evaled = try self.evalStatement(&program.statements.items[i], env);

            switch (evaled) {
                else => result = evaled,
            }
        }

        return result;
    }

    fn evalVariable(self: *Self, variable: *const ast.Variable, env: *Env) EvalError!*object.Object {
        const value = env.*.get(variable.value);
        if (value) |val| {
            return val;
        }

        return util.newError(self.allocator, "variable not found: '${s}'", .{variable.*.value});
    }

    fn evalIdentifier(self: *Self, identifier: *const ast.Identifier, env: *Env) EvalError!*object.Object {
        const value = env.*.get(identifier.*.value);
        if (value) |val| {
            return val;
        }

        return util.newError(self.allocator, "identifier not found: '{s}'", .{identifier.*.value});
    }

    fn evalStatement(self: *Self, statement: *ast.Statement, env: *Env) EvalError!*object.Object {
        switch (statement.*) {
            .expressionStatement => |expressionStatement| return self.evalExpression(expressionStatement.expression, env),
            .assignment => |assignment| {
                const value = try self.evalExpression(assignment.value, env);
                switch (value.*) {
                    .error_ => return value,
                    else => {},
                }

                try env.*.insert(assignment.name.value, value);
                return &builtin.NULL_OBJECT;
            },
        }
    }

    fn evalExpression(self: *Self, expression: *ast.Expression, env: *Env) EvalError!*object.Object {
        switch (expression.*) {
            .variable => |variable| return self.evalVariable(&variable, env),
            .identifier => |identifier| return try self.evalIdentifier(&identifier, env),
            .integer => |integer| return try util.newInteger(self.*.allocator, integer.value),
            else => @panic("Bug: unsupported"),
        }
    }
};
