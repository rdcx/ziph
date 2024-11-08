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
    assignment: Assignment,
    variable: Variable,
    identifier: Identifier,
    integer: Integer,
    float: Float,
    string_sq_literal: StringLiteral,
    string_dq_literal: StringLiteral,
    infixExpression: InfixExpression,

    pub fn toString(self: *Expression, buf: *String) String.Error!void {
        switch (self.*) {
            .integer => |integer| try integer.toString(buf),
            .assignment => |*assignment| try assignment.toString(buf),
            .variable => |*variable| try variable.toString(buf),
            .identifier => |*identifier| try identifier.toString(buf),
            .infixExpression => |infixExpression| try infixExpression.toString(buf),
            .float => |float| try float.toString(buf),
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
    assignment: Assignment,
    expressionStatement: ExpressionStatement,

    pub fn toString(self: *Statement, buf: *String) !void {
        return switch (self.*) {
            .expressionStatement => |expressionStatement| try expressionStatement.toString(buf),
            .variable => |*variable| try variable.toString(buf),
        };
    }
};

pub const ExpressionStatement = struct {
    expression: *Expression,

    pub fn toString(self: ExpressionStatement, buf: *String) String.Error!void {
        try self.expression.toString(buf);
    }
};

pub const Assignment = struct {
    name: Variable,
    value: *Expression,

    pub fn toString(self: *Assignment, buf: *String) !void {
        try self.name.toString(buf);
        try buf.concat(" = ");
        try self.value.toString(buf);
    }
};

pub const Variable = struct {
    value: []const u8,

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

pub const StringLiteral = struct {
    value: []const u8,

    pub fn toString(self: StringLiteral, buf: *String) String.Error!void {
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

pub const Float = struct {
    value: f64,

    pub fn toString(self: Float, buf: *String) String.Error!void {
        const floatString = try std.fmt.allocPrint(buf.allocator, "{}", .{self.value});
        try buf.concat(floatString);
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
    lte,
    gte,

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
            .lte => "<=",
            .gte => ">=",
        };
    }
};
