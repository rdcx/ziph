const std = @import("std");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const token = @import("token.zig");
const string = @import("string.zig");

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

test {
    std.testing.refAllDecls(@This());
}

pub const ParserError = error{
    InvalidProgram,
    MemoryAllocation,
    InvalidPrefix,
    ExpectPeek,
    InvalidInfix,
    InvalidInteger,
    InvalidIntegerLiteral,
    ExpectOperator,
};

const Priority = enum(u4) {
    lowest = 0,
    equals = 1,
    lessgreater = 2,
    sum = 3,
    product = 4,
    prefix = 5,
    call = 6,
    index = 7,

    fn lessThan(self: Priority, other: Priority) bool {
        return @intFromEnum(self) < @intFromEnum(other);
    }

    fn fromToken(tok: token.TokenTag) Priority {
        return switch (tok) {
            .equal => .equals,
            .not_equal => .equals,
            .plus => .sum,
            .minus => .sum,
            .left_paren => .call,
            .left_bracket => .index,
            else => .lowest,
        };
    }
};

fn getOperatorFromToken(tok: token.TokenTag) !ast.Operator {
    return switch (tok) {
        .assign => .assign,
        .plus => .plus,
        .minus => .minus,
        .bang => .bang,
        .equal => .equal,
        .not_equal => .notEqual,
        else => ParserError.ExpectOperator,
    };
}

pub const Parser = struct {
    lex: *lexer.Lexer,
    curToken: token.Token,
    peekToken: token.Token,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn new(lex: *lexer.Lexer, alloc: std.mem.Allocator) Parser {
        const curToken = lex.nextToken();
        const peekToken = lex.nextToken();

        return Parser{
            .lex = lex,
            .curToken = curToken,
            .peekToken = peekToken,
            .allocator = alloc,
        };
    }

    pub fn nextToken(self: *Parser) void {
        self.curToken = self.peekToken;
        self.peekToken = self.lex.nextToken();
    }

    fn currentTokenIs(self: Self, tok: token.TokenTag) bool {
        return self.curToken == tok;
    }

    fn peekTokenIs(self: Self, tok: token.TokenTag) bool {
        return self.peekToken == tok;
    }

    fn expectPeek(self: *Self, tok: token.TokenTag) ParserError!void {
        if (self.peekTokenIs(tok)) {
            self.nextToken();
        } else {
            std.debug.print("expected next token to be {}, got {}\n", .{ tok, self.peekToken });
            return ParserError.ExpectPeek;
        }
    }

    pub fn parseProgram(self: *Self) ParserError!ast.Program {
        var statements = std.ArrayList(ast.Statement).init(self.allocator);

        while (self.curToken != token.Token.eof) {
            const statement = try self.parseStatement();
            statements.append(statement) catch return ParserError.InvalidProgram;
            self.nextToken();
        }

        return ast.Program{ .statements = statements };
    }

    fn parseStatement(self: *Self) ParserError!ast.Statement {
        return switch (self.curToken) {
            .variable => {
                if (!self.peekTokenIs(.assign)) {
                    return ast.Statement{ .expressionStatement = try self.parseExpressionStatement() };
                }
                return ast.Statement{ .variable = try self.parseVariableStatement() };
            },
            else => ast.Statement{ .expressionStatement = try self.parseExpressionStatement() },
        };
    }

    fn parseExpressionStatement(self: *Self) ParserError!ast.ExpressionStatement {
        const expression = try self.parseExpression(.lowest);
        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        const expressionPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        expressionPtr.* = expression;
        return ast.ExpressionStatement{ .expression = expressionPtr };
    }

    fn parseVariableStatement(self: *Self) ParserError!ast.Variable {
        const name =
            switch (self.curToken) {
            .variable => |variable| ast.Identifier{ .value = variable },
            else => unreachable,
        };

        try self.expectPeek(.assign);
        self.nextToken();

        const expression = try self.parseExpression(.lowest);
        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        // switch (expression) {
        //     .function => |*function| {
        //         function.*.name = name.value;
        //     },
        //     else => {},
        // }

        const expressionPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        expressionPtr.* = expression;
        return ast.Variable{ .name = name, .value = expressionPtr };
    }

    fn parseExpression(self: *Self, precedende: Priority) ParserError!ast.Expression {
        var leftExpression = try self.parseExpressionByPrefixToken(self.curToken);

        while (!self.peekTokenIs(.semicolon) and precedende.lessThan(Priority.fromToken(self.peekToken))) {
            const leftExpressionPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
            leftExpressionPtr.* = leftExpression;

            leftExpression = try self.parseInfixExpressionByToken(self.peekToken, leftExpressionPtr);
        }

        return leftExpression;
    }

    fn parseInfixExpressionByToken(self: *Self, tok: token.TokenTag, left: *ast.Expression) ParserError!ast.Expression {
        self.nextToken();
        return switch (tok) {
            .plus, .minus, .equal, .not_equal => ast.Expression{ .infixExpression = try self.parseInfixExpression(left) },
            else => ParserError.InvalidInfix,
        };
    }

    fn parseInfixExpression(self: *Self, left: *ast.Expression) ParserError!ast.InfixExpression {
        const operator = try getOperatorFromToken(self.curToken);
        const priority = Priority.fromToken(self.curToken);

        self.nextToken();

        const right = try self.parseExpression(priority);
        const rightPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        rightPtr.* = right;

        return ast.InfixExpression{ .operator = operator, .left = left, .right = rightPtr };
    }

    fn parseExpressionByPrefixToken(self: *Self, tok: token.TokenTag) ParserError!ast.Expression {
        return switch (tok) {
            .variable => ast.Expression{ .identifier = try self.parseIdentifier() },
            .integer_literal => ast.Expression{ .integer = try self.parseInteger() },
            else => ParserError.InvalidPrefix,
        };
    }

    fn parseIdentifier(self: Self) ParserError!ast.Identifier {
        return switch (self.curToken) {
            .variable => |variable| ast.Identifier{ .value = variable },
            else => ParserError.InvalidProgram,
        };
    }

    fn parseInteger(self: Self) ParserError!ast.Integer {
        return switch (self.curToken) {
            .integer_literal => |value| ast.Integer{ .value = std.fmt.parseInt(i64, value, 10) catch return ParserError.InvalidInteger },
            else => ParserError.InvalidIntegerLiteral,
        };
    }
};

test "Parser.new" {
    var lex = lexer.new(":;");
    const parser = Parser.new(&lex, std.testing.allocator);
    try expectEqual(token.TokenTag.colon, parser.curToken);
    try expectEqual(token.TokenTag.semicolon, parser.peekToken);
}

fn parseProgramForTesting(actualInput: []const u8, expecting: fn (*const ast.Program) anyerror!void) !void {
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();

    var lex = lexer.new(actualInput);
    var parser = Parser.new(&lex, allocator.allocator());
    const program = try parser.parseProgram();

    try expecting(&program);
}

fn expectOneStatementInProgram(
    expected: *const ast.Statement,
    program: *const ast.Program,
) !void {
    try expectEqual(@as(usize, 1), program.*.statements.items.len);
    try expectStatement(expected, &program.*.statements.items[0]);
}

fn expectIdentifier(expected: *const ast.Identifier, actual: *const ast.Identifier) !void {
    try expectEqualStrings(expected.*.value, actual.*.value);
}

fn expectInteger(expected: *const ast.Integer, actual: *const ast.Integer) !void {
    try expectEqual(expected.*.value, actual.*.value);
}

fn expectVariableStatement(expected: *const ast.Variable, actual: *const ast.Variable) !void {
    try expectIdentifier(&expected.*.name, &actual.*.name);
    try expectExpression(expected.*.value, actual.*.value);
}

fn expectVariableStatementByStatement(expected: *const ast.Variable, actual: *const ast.Statement) !void {
    switch (actual.*) {
        .variable => |variable| try expectVariableStatement(expected, &variable),
        else => {
            std.debug.print("expected .variable, found {}\n", .{actual});
            return error.TestExpectedVariableStatementByStatement;
        },
    }
}

fn expectExpression(expected: *const ast.Expression, actual: *const ast.Expression) !void {
    switch (expected.*) {
        .integer => {
            switch (actual.*) {
                .integer => |integer| try expectInteger(&expected.*.integer, &integer),
                else => {
                    std.debug.print("expected .integer, found {}\n", .{actual});
                    return error.TestExpectedExpression;
                },
            }
        },
        else => {
            std.debug.print("unsupported {}\n", .{expected});
            return error.TestExpectedExpression;
        },
    }
}

fn expectStatement(expected: *const ast.Statement, actual: *const ast.Statement) !void {
    switch (expected.*) {
        .variable => |variable| try expectVariableStatementByStatement(&variable, actual),
        .expressionStatement => |expressionStatement| try expectExpressionStatementByStatement(&expressionStatement, actual),
        // else => {
        //     std.debug.print("unsupported {}\n", .{expected});
        //     return error.TestExpectedStatement;
        // },
    }
}

fn expectExpressionStatement(expected: *const ast.ExpressionStatement, actual: *const ast.ExpressionStatement) !void {
    try expectExpression(expected.*.expression, actual.*.expression);
}

fn expectExpressionStatementByStatement(expected: *const ast.ExpressionStatement, actual: *const ast.Statement) !void {
    switch (actual.*) {
        .expressionStatement => |expressionStatement| try expectExpressionStatement(expected, &expressionStatement),
        else => {
            std.debug.print("expected .return_, found {}\n", .{actual});
            return error.TestExpectedExpressionStatementByStatement;
        },
    }
}

test "variable statements" {
    {
        try parseProgramForTesting("$x = 5;", struct {
            fn function(program: *const ast.Program) !void {
                var integer = ast.Expression{ .integer = ast.Integer{ .value = 5 } };
                try expectOneStatementInProgram(&ast.Statement{
                    .variable = ast.Variable{
                        .name = ast.Identifier{ .value = "x" },
                        .value = &integer,
                    },
                }, program);
            }
        }.function);
    }
}

test "expression statements" {
    {
        try parseProgramForTesting("5;", struct {
            fn function(program: *const ast.Program) !void {
                var integer = ast.Expression{ .integer = ast.Integer{ .value = 5 } };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &integer },
                }, program);
            }
        }.function);
    }
}
