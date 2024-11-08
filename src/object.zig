const std = @import("std");
const string = @import("string.zig");

pub const Object = union(enum) {
    null_: Null,
    error_: Error,
    integer: Integer,

    pub fn toString(self: *Object, buf: *string.String) string.String.Error!void {
        switch (self.*) {
            .null_ => |null_| try null_.toString(buf),
            .error_ => |error_| try error_.toString(buf),
            .integer => |integer| try integer.toString(buf),
        }
    }

    pub fn typeName(self: Object) []const u8 {
        return switch (self) {
            .null_ => "Null",
            .integer => "Integer",
            .error_ => "Error",
        };
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

pub const Error = struct {
    message: []const u8,

    pub fn toString(self: Error, buf: *string.String) string.String.Error!void {
        try buf.concat("Error: ");
        try buf.concat(self.message);
    }
};

pub const Null = struct {
    pub fn toString(_: Null, buf: *string.String) string.String.Error!void {
        try buf.concat("null");
    }
};
