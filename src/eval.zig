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
            .assignment => |assignment| return self.evalAssignment(&assignment, env),
            .variable => |variable| return self.evalVariable(&variable, env),
            .identifier => |identifier| return try self.evalIdentifier(&identifier, env),
            .integer => |integer| return try util.newInteger(self.*.allocator, integer.value),
            .float => |float| return try util.newFloat(self.*.allocator, float.value),

            .infixExpression => |infixExpression| {
                const left = try self.evalExpression(infixExpression.left, env);
                switch (left.*) {
                    .error_ => return left,
                    else => {},
                }

                const right = try self.evalExpression(infixExpression.right, env);
                switch (right.*) {
                    .error_ => return right,
                    else => {},
                }

                return try self.evalInfixExpression(&infixExpression.operator, left, right);
            },
            // else => @panic("Bug: unsupported"),
        }
    }

    fn evalAssignment(self: *Self, assignment: *const ast.Assignment, env: *Env) EvalError!*object.Object {
        const value = try self.evalExpression(assignment.value, env);
        switch (value.*) {
            .error_ => return value,
            else => {},
        }

        try env.*.insert(assignment.name.value, value);
        return value;
    }

    fn evalInfixExpression(self: *Self, operator: *const ast.Operator, left: *object.Object, right: *object.Object) EvalError!*object.Object {
        switch (left.*) {
            .integer => |leftInteger| {
                switch (right.*) {
                    .integer => |rightInteger| return try self.evalIntegerInfixExpression(operator, &leftInteger, &rightInteger),
                    .float => |rightFloat| {
                        const convObje = try util.newFloatFromInteger(
                            self.allocator,
                            leftInteger.value,
                        );
                        return try self.evalFloatInfixExpression(
                            operator,
                            &rightFloat,
                            &convObje.float,
                        );
                    },
                    else => return util.newError(
                        self.allocator,
                        "type mismatch: {s} {s} {s}",
                        .{ left.typeName(), operator.toString(), right.typeName() },
                    ),
                }
            },

            .float => |leftFloat| {
                switch (right.*) {
                    .float => |rightFloat| return try self.evalFloatInfixExpression(operator, &leftFloat, &rightFloat),
                    .integer => |rightInteger| {
                        const convObj = try util.newFloatFromInteger(
                            self.allocator,
                            rightInteger.value,
                        );
                        return try self.evalFloatInfixExpression(
                            operator,
                            &leftFloat,
                            &convObj.float,
                        );
                    },
                    else => return util.newError(
                        self.allocator,
                        "type mismatch: {s} {s} {s}",
                        .{ left.typeName(), operator.toString(), right.typeName() },
                    ),
                }
            },

            else => {
                switch (operator.*) {
                    // .equal => return nativeBoolToBooleanObject(self.compareObject(left, right)),
                    // .notEqual => return nativeBoolToBooleanObject(!self.compareObject(left, right)),
                    else => {
                        if (std.mem.eql(u8, @tagName(left.*), @tagName(right.*))) {
                            return try util.newError(
                                self.allocator,
                                "unknown operator: {s} {s} {s}",
                                .{ left.typeName(), operator.toString(), right.typeName() },
                            );
                        } else {
                            return try util.newError(
                                self.allocator,
                                "type mismatch: {s} {s} {s}",
                                .{ left.typeName(), operator.toString(), right.typeName() },
                            );
                        }
                    },
                }
            },
        }
    }

    fn evalIntegerInfixExpression(self: *Self, operator: *const ast.Operator, left: *const object.Integer, right: *const object.Integer) EvalError!*object.Object {
        switch (operator.*) {
            .plus => return util.newInteger(self.allocator, left.*.value + right.*.value),
            .minus => return util.newInteger(self.allocator, left.*.value - right.*.value),
            .asterisk => return util.newInteger(self.allocator, left.*.value * right.*.value),
            .slash => return util.newInteger(self.allocator, @divFloor(left.*.value, right.*.value)),
            .lt => return nativeBoolToBooleanObject(left.*.value < right.*.value),
            .gt => return nativeBoolToBooleanObject(left.*.value > right.*.value),
            .lte => return nativeBoolToBooleanObject(left.*.value <= right.*.value),
            .gte => return nativeBoolToBooleanObject(left.*.value >= right.*.value),
            .equal => return nativeBoolToBooleanObject(left.*.value == right.*.value),
            .notEqual => return nativeBoolToBooleanObject(left.*.value != right.*.value),
            else => return util.newError(
                self.allocator,
                "unknown operator: Integer {s} Integer",
                .{operator.toString()},
            ),
        }
    }

    fn evalFloatInfixExpression(self: *Self, operator: *const ast.Operator, left: *const object.Float, right: *const object.Float) EvalError!*object.Object {
        switch (operator.*) {
            .plus => return util.newFloat(self.allocator, left.*.value + right.*.value),
            .minus => return util.newFloat(self.allocator, left.*.value - right.*.value),
            .asterisk => return util.newFloat(self.allocator, left.*.value * right.*.value),
            .slash => return util.newFloat(self.allocator, left.*.value / right.*.value),
            .lt => return nativeBoolToBooleanObject(left.*.value < right.*.value),
            .gt => return nativeBoolToBooleanObject(left.*.value > right.*.value),
            .lte => return nativeBoolToBooleanObject(left.*.value <= right.*.value),
            .gte => return nativeBoolToBooleanObject(left.*.value >= right.*.value),
            .equal => return nativeBoolToBooleanObject(left.*.value == right.*.value),
            .notEqual => return nativeBoolToBooleanObject(left.*.value != right.*.value),
            else => return util.newError(
                self.allocator,
                "unknown operator: Float {s} Float",
                .{operator.toString()},
            ),
        }
    }

    fn nativeBoolToBooleanObject(native: bool) *object.Object {
        if (native) {
            return &builtin.TRUE_OBJECT;
        } else {
            return &builtin.FALSE_OBJECT;
        }
    }
};
