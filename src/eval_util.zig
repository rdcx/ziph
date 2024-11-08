const std = @import("std");

const ast = @import("ast.zig");
const object = @import("object.zig");
const Environment = @import("env.zig").Environment;
const EvaluatorError = @import("eval_error.zig").EvaluatorError;

pub fn newError(allocator: std.mem.Allocator, comptime fmt: []const u8, args: anytype) !*object.Object {
    const message = std.fmt.allocPrint(allocator, fmt, args) catch return EvaluatorError.MemoryAllocation;
    const errorPtr = allocator.create(object.Object) catch return EvaluatorError.MemoryAllocation;
    errorPtr.* = object.Object{ .error_ = object.Error{ .message = message } };
    return errorPtr;
}

pub fn newInteger(allocator: std.mem.Allocator, value: i64) !*object.Object {
    const integerPtr = allocator.create(object.Object) catch return EvaluatorError.MemoryAllocation;
    integerPtr.* = object.Object{
        .integer = object.Integer{ .value = value },
    };
    return integerPtr;
}

pub fn newFloat(allocator: std.mem.Allocator, value: f64) !*object.Object {
    const floatPtr = allocator.create(object.Object) catch return EvaluatorError.MemoryAllocation;
    floatPtr.* = object.Object{
        .float = object.Float{ .value = value },
    };
    return floatPtr;
}

pub fn newFloatFromInteger(allocator: std.mem.Allocator, value: i64) !*object.Object {
    return newFloat(allocator, @floatFromInt(value));
}

pub fn newIntegerFromFloat(allocator: std.mem.Allocator, value: f64) !*object.Object {
    return newInteger(allocator, @round(value));
}
