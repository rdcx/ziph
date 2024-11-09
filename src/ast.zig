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
    call: Call,
    function: Function,
    if_: If,

    pub fn toString(self: *Expression, buf: *String) String.Error!void {
        switch (self.*) {
            .integer => |integer| try integer.toString(buf),
            .assignment => |*assignment| try assignment.toString(buf),
            .variable => |*variable| try variable.toString(buf),
            .identifier => |*identifier| try identifier.toString(buf),
            .infixExpression => |infixExpression| try infixExpression.toString(buf),
            .float => |float| try float.toString(buf),
            .call => |call| try call.toString(buf),
            .if_ => |if_| try if_.toString(buf),
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
    expressionStatement: ExpressionStatement,
    return_: Return,
    if_: If,

    pub fn toString(self: *Statement, buf: *String) !void {
        return switch (self.*) {
            .expressionStatement => |expressionStatement| try expressionStatement.toString(buf),
            .return_ => |return_| try return_.toString(buf),
        };
    }
};

pub const ExpressionStatement = struct {
    expression: *Expression,

    pub fn toString(self: ExpressionStatement, buf: *String) String.Error!void {
        try self.expression.toString(buf);
    }
};

pub const If = struct {
    condition: *Expression,
    thenBranch: Block,
    elseBranch: ?Block,

    pub fn toString(self: *If, buf: *String) String.Error!void {
        try buf.concat("if ");
        try self.condition.toString(buf);
        try buf.concat(" ");
        try self.thenBranch.toString(buf);
        if (self.elseBranch) |*elseBranch| {
            try buf.concat(" else ");
            try elseBranch.toString(buf);
        }
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

pub const Function = struct {
    name: Identifier,
    parameters: std.ArrayList(Variable),
    body: Block,

    pub fn toString(self: *Function, buf: *String) !void {
        try buf.concat("fn ");
        try buf.concat(self.name);
        try buf.concat("(");
        var i: usize = 0;
        while (i < self.parameters.items.len) : (i += 1) {
            try self.parameters.items[i].toString(buf);
            if (i + 1 < self.parameters.items.len) {
                try buf.concat(", ");
            }
        }
        try buf.concat(") ");
        try self.body.toString(buf);
    }
};

pub const Call = struct {
    callee: *Expression,
    arguments: std.ArrayList(Expression),

    pub fn toString(self: *Call, buf: *String) String.Error!void {
        try self.callee.toString(buf);
        try buf.concat("(");
        const len = self.arguments.items.len;
        var i: usize = 0;
        while (i < len) : (i += 1) {
            try self.arguments.items[i].toString(buf);
            if (i != len - 1) {
                try buf.concat(", ");
            }
        }
        try buf.concat(")");
    }
};

pub const Block = struct {
    statements: std.ArrayList(Statement),

    pub fn toString(self: *Block, buf: *String) !void {
        try buf.concat("{\n");
        var i: usize = 0;
        while (i < self.statements.items.len) : (i += 1) {
            try self.statements.items[i].toString(buf);
            try buf.concat("\n");
        }
        try buf.concat("}");
    }
};

pub const Return = struct {
    value: *Expression,

    pub fn toString(self: Return, buf: *String) String.Error!void {
        try buf.concat("return ");
        try self.value.toString(buf);
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
