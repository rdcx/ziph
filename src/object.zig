const std = @import("std");
const string = @import("string.zig");
const ast = @import("ast.zig");
const Env = @import("env.zig").Environment;

pub const Object = union(enum) {
    null_: Null,
    error_: Error,
    integer: Integer,
    boolean: Boolean,
    float: Float,
    string: String,
    function: Function,
    return_: Return,

    pub fn toString(self: *Object, buf: *string.String) string.String.Error!void {
        switch (self.*) {
            .null_ => |null_| try null_.toString(buf),
            .error_ => |error_| try error_.toString(buf),
            .integer => |integer| try integer.toString(buf),
            .float => |float| try float.toString(buf),
            .boolean => |boolean| try boolean.toString(buf),
            .string => |str| try str.toString(buf),
            .function => |*function| try function.toString(buf),
            .return_ => |ret| try ret.value.toString(buf),
        }
    }

    pub fn typeName(self: Object) []const u8 {
        return switch (self) {
            .null_ => "Null",
            .integer => "Integer",
            .float => "Float",
            .error_ => "Error",
            .boolean => "Boolean",
            .string => "String",
            .function => "Function",
            .return_ => "Return",
        };
    }
};

pub const Error = struct {
    message: []const u8,

    pub fn toString(self: Error, buf: *string.String) string.String.Error!void {
        try buf.concat("Error: ");
        try buf.concat(self.message);
    }
};

pub const String = struct {
    value: []const u8,

    pub fn toString(self: String, buf: *string.String) string.String.Error!void {
        try buf.concat(self.value);
    }
};

pub const Integer = struct {
    value: i64,

    pub fn new(value: i64) Integer {
        return .{
            .value = value,
        };
    }

    pub fn toString(self: Integer, buf: *string.String) string.String.Error!void {
        const intString = try std.fmt.allocPrint(buf.allocator, "{}", .{self.value});
        try buf.concat(intString);
    }
};

pub const Null = struct {
    pub fn toString(_: Null, buf: *string.String) string.String.Error!void {
        try buf.concat("null");
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn toString(self: Boolean, buf: *string.String) string.String.Error!void {
        if (self.value) {
            try buf.concat("true");
        } else {
            try buf.concat("false");
        }
    }
};

pub const Float = struct {
    value: f64,

    pub fn toString(self: Float, buf: *string.String) string.String.Error!void {
        const floatString = try std.fmt.allocPrint(buf.allocator, "{d:.9}", .{self.value});
        try buf.concat(floatString);
    }
};

pub const Function = struct {
    parameters: std.ArrayList(ast.Variable),
    body: *ast.Block,
    env: *Env,

    pub fn toString(self: *Function, buf: *string.String) string.String.Error!void {
        try buf.concat("function");
        try buf.concat("(");
        var i: usize = 0;
        while (i < self.parameters.items.len) : (i += 1) {
            try buf.concat("$");
            try buf.concat(self.parameters.items[i].value);
            if (i != self.parameters.items.len - 1) {
                try buf.concat(", ");
            }
        }
        try buf.concat(") ");
    }
};

pub const Return = struct {
    value: *Object,
};
