const std = @import("std");
const token = @import("token.zig");
const String = @import("string.zig").String;

// Test initialization
test {
    std.testing.refAllDecls(@This());
}

pub const Node = union(enum) {
    program: Program,
    statement: Statement,
    expression: Expression,

    pub fn toString(self: *Node, buf: *String) !void {
        return switch (self.*) {
            .program => |*program| try program.*.toString(buf),
            .statement => |*statement| try statement.*.toString(buf),
            .expression => |*expression| try expression.*.toString(buf),
        };
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn toString(self: *Program, buf: *String) !void {
        var i: usize = 0;
        while (i < self.statements.items.len) : (i += 1) {
            try self.statements.items[i].toString(buf);
        }
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integer: Integer,
    infixExpression: InfixExpression,

    pub fn toString(self: *Expression, buf: *String) String.Error!void {
        switch (self.*) {
            .integer => |integer| try integer.toString(buf),
        }
    }
};

pub const InfixExpression = struct {
    left: *Expression,
    operator: Operator,
    right: *Expression,

    pub fn toString(self: InfixExpression, buf: *String) String.Error!void {
        try buf.concat("(");
        try self.left.toString(buf);
        try buf.concat(" ");
        try buf.concat(self.operator.toString());
        try buf.concat(" ");
        try self.right.toString(buf);
        try buf.concat(")");
    }
};

pub const Statement = union(enum) {
    variable: Variable,

    pub fn toString(self: *Statement, buf: *String) !void {
        return switch (self.*) {
            .variable => |*variable| try variable.toString(buf),
        };
    }
};

pub const Variable = struct {
    name: Identifier,
    value: *Expression,

    pub fn toString(self: *Variable, buf: *String) !void {
        try buf.concat(self.name.value);
        try buf.concat(" = ");
        try self.toString(buf);
    }
};

pub const Identifier = struct {
    value: []const u8,

    pub fn toString(self: *Identifier, buf: *String) !void {
        try buf.concat(self.value);
    }
};

pub const Integer = struct {
    value: i64,

    pub fn toString(self: Integer, buf: *String) String.Error!void {
        const intString = try std.fmt.allocPrint(buf.allocator, "{}", .{self.value});
        try buf.concat(intString);
    }
};

pub const Operator = enum {
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    equal,
    notEqual,
    lt,
    gt,

    pub fn toString(self: Operator) []const u8 {
        return switch (self) {
            .assign => "=",
            .plus => "+",
            .minus => "-",
            .bang => "!",
            .asterisk => "*",
            .slash => "/",
            .equal => "==",
            .notEqual => "!=",
            .lt => "<",
            .gt => ">",
        };
    }
};
