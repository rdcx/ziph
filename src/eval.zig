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
            .return_ => |return_| {
                const value = try self.evalExpression(return_.value, env);
                switch (value.*) {
                    .error_ => return value,
                    else => {},
                }

                const objectPtr = self.allocator.create(object.Object) catch return EvalError.MemoryAllocation;
                objectPtr.* = object.Object{
                    .return_ = object.Return{
                        .value = value,
                    },
                };
                return objectPtr;
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
            .string_sq_literal => |stringLiteral| return try util.newString(self.allocator, stringLiteral.value),
            .string_dq_literal => |stringLiteral| return try util.newString(self.allocator, stringLiteral.value),
            .function => |*function| return try self.evalFunction(function, env),
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
            .call => |*call| {
                const function = try self.evalExpression(call.callee, env);
                switch (function.*) {
                    .error_ => return function,
                    else => {},
                }

                var args = std.ArrayList(*object.Object).init(self.allocator);
                var i: usize = 0;
                while (i < call.arguments.items.len) : (i += 1) {
                    const evaled = try self.evalExpression(&call.arguments.items[i], env);
                    switch (evaled.*) {
                        .error_ => return evaled,
                        else => {},
                    }

                    args.append(evaled) catch return EvalError.MemoryAllocation;
                }
                return try self.applyFunction(function, args);
            },
            .if_ => |*if_| return try self.evalIfExpression(if_, env),
            .boolean => |boolean| return nativeBoolToBooleanObject(boolean.value),
            // else => @panic("Bug: unsupported"),
        }
    }

    fn evalIfExpression(self: *Self, ifExpression: *ast.If, env: *Env) EvalError!*object.Object {
        const condition = try self.evalExpression(ifExpression.*.condition, env);
        switch (condition.*) {
            .error_ => return condition,
            else => {},
        }

        if (isTruthy(condition)) {
            return try self.evalBlock(&ifExpression.*.thenBranch, env);
        } else if (ifExpression.*.elseBranch) |*elseBranch| {
            return try self.evalBlock(elseBranch, env);
        } else {
            return &builtin.NULL_OBJECT;
        }
    }

    fn isTruthy(obj: *object.Object) bool {
        switch (obj.*) {
            .boolean => |boolean| return boolean.value,
            .null_ => return false,
            else => return true,
        }
    }

    fn evalFunction(self: *Self, function: *ast.Function, env: *Env) EvalError!*object.Object {
        const fun = try util.newFunction(self.allocator, function.parameters, &function.body, env);
        // add function to env
        try env.*.insert(function.name.value, fun);

        return fun;
    }

    fn applyFunction(self: *Self, function: *object.Object, arguments: std.ArrayList(*object.Object)) !*object.Object {
        switch (function.*) {
            .function => |*func| {
                if (func.parameters.items.len != arguments.items.len) {
                    return util.newError(
                        self.allocator,
                        "wrong number of arguments: want={}, got={}",
                        .{ func.parameters.items.len, arguments.items.len },
                    );
                }

                const extendedEnv = try self.extendFunctionEnv(func, arguments);
                const evaluated = try self.evalBlock(func.body, extendedEnv);
                return unwrapReturnValue(evaluated);
            },
            // .builtinFunction => |builtinFunction| return try builtinFunction.call(self.allocator, arguments),
            else => return util.newError(self.allocator, "not a function: {s}", .{function.typeName()}),
        }
    }

    fn extendFunctionEnv(self: Self, function: *object.Function, arguments: std.ArrayList(*object.Object)) EvalError!*Env {
        const envPtr = self.allocator.create(Env) catch return EvalError.MemoryAllocation;
        envPtr.* = Env.newEnclose(self.allocator, function.*.env);
        var i: usize = 0;
        while (i < function.parameters.items.len) : (i += 1) {
            try envPtr.*.insert(function.parameters.items[i].value, arguments.items[i]);
        }

        return envPtr;
    }

    fn unwrapReturnValue(obj: *object.Object) *object.Object {
        switch (obj.*) {
            .return_ => |returnValue| return returnValue.value,
            else => return obj,
        }
    }

    fn evalBlock(self: *Self, block: *ast.Block, env: *Env) EvalError!*object.Object {
        var result: *object.Object = &builtin.NULL_OBJECT;
        var i: usize = 0;
        while (i < block.statements.items.len) : (i += 1) {
            const evaled = try self.evalStatement(&block.statements.items[i], env);
            switch (evaled.*) {
                .return_ => return evaled,
                .error_ => return evaled,
                else => result = evaled,
            }
        }

        return result;
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

            .string => |leftString| {
                switch (right.*) {
                    .string => |rightString| {
                        if (operator.* == ast.Operator.equal or operator.* == ast.Operator.identical) {
                            return nativeBoolToBooleanObject(std.mem.eql(u8, leftString.value, rightString.value));
                        } else if (operator.* == ast.Operator.notEqual or operator.* == ast.Operator.notIdentical) {
                            return nativeBoolToBooleanObject(!std.mem.eql(u8, leftString.value, rightString.value));
                        } else {
                            return util.newError(
                                self.allocator,
                                "unknown operator: String {s} String",
                                .{operator.toString()},
                            );
                        }
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
                    .equal => return nativeBoolToBooleanObject(self.compareObject(left, right)),
                    .notEqual => return nativeBoolToBooleanObject(!self.compareObject(left, right)),
                    .identical => return nativeBoolToBooleanObject(left == right),
                    .notIdentical => return nativeBoolToBooleanObject(left != right),
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
            .identical => return nativeBoolToBooleanObject(left.*.value == right.*.value),
            .notIdentical => return nativeBoolToBooleanObject(left.*.value != right.*.value),
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
            .identical => return nativeBoolToBooleanObject(left.*.value == right.*.value),
            .notIdentical => return nativeBoolToBooleanObject(left.*.value != right.*.value),
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

    fn compareObject(_: Self, obj1: *object.Object, obj2: *object.Object) bool {
        switch (obj1.*) {
            .null_ => {
                switch (obj2.*) {
                    .null_ => return true,
                    else => return false,
                }
            },
            .integer => |integer1| {
                switch (obj2.*) {
                    .integer => |integer2| return integer1.value == integer2.value,
                    else => return false,
                }
            },
            .boolean => |boolean1| {
                switch (obj2.*) {
                    .boolean => |boolean2| return boolean1.value == boolean2.value,
                    else => return false,
                }
            },
            .string => |string1| {
                switch (obj2.*) {
                    .string => |string2| return std.mem.eql(u8, string1.value, string2.value),
                    else => return false,
                }
            },
            .function => return @intFromPtr(obj1) == @intFromPtr(obj2),
            // .builtinFunction => return @intFromPtr(obj1) == @intFromPtr(obj2),
            else => @panic("Unsupported comparison."),
        }
    }
};
