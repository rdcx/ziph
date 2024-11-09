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
    InvalidFloat,
    InvalidIntegerLiteral,
    ExpectArgument,
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
            .lt => .lessgreater,
            .gt => .lessgreater,
            .lte => .lessgreater,
            .gte => .lessgreater,
            .plus => .sum,
            .minus => .sum,
            .asterisk => .product,
            .slash => .product,
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
        .asterisk => .asterisk,
        .slash => .slash,
        .gt => .gt,
        .lt => .lt,
        .gte => .gte,
        .lte => .lte,
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
            .open_tag => {
                self.nextToken();
                return try self.parseStatement();
            },
            .close_tag => {
                self.nextToken();
                return try self.parseStatement();
            },
            // .variable => {
            //     if (self.peekTokenIs(.assign)) {
            //         return ast.Statement{ .assignment = try self.parseAssignmentStatement() };
            //     }
            //     return ast.Statement{ .expressionStatement = try self.parseExpressionStatement() };
            // },
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

    fn parseBlock(self: *Self) ParserError!ast.Block {
        var statements = std.ArrayList(ast.Statement).init(self.allocator);

        while (!self.currentTokenIs(.right_brace)) {
            const statement = try self.parseStatement();
            statements.append(statement) catch return ParserError.MemoryAllocation;
            self.nextToken();
        }

        return ast.Block{ .statements = statements };
    }

    fn parseAssignmentStatement(self: *Self) ParserError!ast.Assignment {
        const name =
            switch (self.curToken) {
            .variable => |variable| ast.Variable{ .value = variable },
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
        return ast.Assignment{ .name = name, .value = expressionPtr };
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
            .plus, .minus, .asterisk, .slash, .gt, .lt, .gte, .lte, .equal, .not_equal => ast.Expression{ .infixExpression = try self.parseInfixExpression(left) },
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
            .variable => {
                if (self.peekTokenIs(.assign)) {
                    return ast.Expression{ .assignment = try self.parseAssignmentExpression() };
                }
                return ast.Expression{ .variable = ast.Variable{ .value = try self.parseVariable() } };
            },
            .string_sq_literal => ast.Expression{ .string_sq_literal = try self.parseString() },
            .string_dq_literal => ast.Expression{ .string_dq_literal = try self.parseString() },
            .integer_literal => ast.Expression{ .integer = try self.parseInteger() },
            .float_literal => ast.Expression{ .float = try self.parseFloat() },
            .left_paren => try self.parseGroupedExpression(),
            .function => ast.Expression{ .function = try self.parseFunctionLiteral() },
            else => {
                std.debug.print("unsupported {}\n", .{tok});
                return ParserError.InvalidPrefix;
            },
        };
    }

    fn parseFunctionLiteral(self: *Self) ParserError!ast.Function {
        try self.expectPeek(.ident);
        const name = ast.Identifier{ .value = self.curToken.ident };

        try self.expectPeek(.left_paren);
        self.nextToken();

        var parameters = std.ArrayList(ast.Variable).init(self.allocator);
        while (!self.currentTokenIs(.right_paren)) {
            // TODO: allow type annotations
            // TODO: allow default values

            if (!self.currentTokenIs(.variable)) {
                return ParserError.ExpectArgument;
            }
            parameters.append(ast.Variable{ .value = try self.parseVariable() }) catch return ParserError.MemoryAllocation;
            self.nextToken();

            if (self.currentTokenIs(.comma)) {
                self.nextToken();
            }
        }

        try self.expectPeek(.left_brace);
        self.nextToken();

        return ast.Function{
            .name = name,
            .parameters = parameters,
            .body = try self.parseBlock(),
        };
    }

    fn parseString(self: Self) ParserError!ast.StringLiteral {
        return switch (self.curToken) {
            .string_sq_literal => |value| ast.StringLiteral{ .value = value },
            .string_dq_literal => |value| ast.StringLiteral{ .value = value },
            else => unreachable,
        };
    }

    fn parseGroupedExpression(self: *Self) ParserError!ast.Expression {
        self.nextToken();
        const expression = try self.parseExpression(.lowest);
        try self.expectPeek(.right_paren);
        return expression;
    }

    fn parseAssignmentExpression(self: *Self) ParserError!ast.Assignment {
        const name = ast.Variable{ .value = try self.parseVariable() };
        try self.expectPeek(.assign);
        self.nextToken();

        const value = try self.parseExpression(.lowest);
        const valuePtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        valuePtr.* = value;

        return ast.Assignment{ .name = name, .value = valuePtr };
    }

    fn parseVariable(self: Self) ParserError![]const u8 {
        return switch (self.curToken) {
            .variable => |value| value,
            else => unreachable,
        };
    }

    fn parseInteger(self: Self) ParserError!ast.Integer {
        return switch (self.curToken) {
            .integer_literal => |value| ast.Integer{ .value = std.fmt.parseInt(i64, value, 10) catch return ParserError.InvalidInteger },
            else => ParserError.InvalidIntegerLiteral,
        };
    }

    fn parseFloat(self: Self) ParserError!ast.Float {
        return switch (self.curToken) {
            .float_literal => |value| ast.Float{ .value = std.fmt.parseFloat(f64, value) catch return ParserError.InvalidFloat },
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

fn expectFloat(expected: *const ast.Float, actual: *const ast.Float) !void {
    try expectEqual(expected.*.value, actual.*.value);
}

fn expectVariable(expected: *const ast.Variable, actual: *const ast.Variable) !void {
    try expectEqualStrings(expected.*.value, actual.*.value);
}

fn expectAssignmentStatement(expected: *const ast.Assignment, actual: *const ast.Assignment) !void {
    try expectVariable(&expected.*.name, &actual.*.name);
    try expectExpression(expected.*.value, actual.*.value);
}

fn expectAssignmentStatementByStatement(expected: *const ast.Assignment, actual: *const ast.Statement) !void {
    switch (actual.*) {
        .assignment => |assignment| try expectAssignmentStatement(expected, &assignment),
        else => {
            std.debug.print("expected .assignment, found {}\n", .{actual});
            return error.TestExpectedAssignmentStatementByStatement;
        },
    }
}

fn expectInfixExpression(expected: *const ast.InfixExpression, actual: *const ast.InfixExpression) anyerror!void {
    try expectExpression(expected.*.left, actual.*.left);
    try expectEqual(expected.*.operator, actual.*.operator);
    try expectExpression(expected.*.right, actual.*.right);
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
        .float => {
            switch (actual.*) {
                .float => |float| try expectFloat(&expected.*.float, &float),
                else => {
                    std.debug.print("expected .float, found {}\n", .{actual});
                    return error.TestExpectedExpression;
                },
            }
        },
        .infixExpression => {
            switch (actual.*) {
                .infixExpression => |infixExpression| try expectInfixExpression(&expected.*.infixExpression, &infixExpression),
                else => {
                    std.debug.print("expected .infixExpression, found {}\n", .{actual});
                    return error.TestExpectedExpression;
                },
            }
        },
        .string_sq_literal => {
            switch (actual.*) {
                .string_dq_literal => |lit| try expectEqualStrings(expected.*.string_sq_literal.value, lit.value),
                else => {
                    std.debug.print("expected .string, found {}\n", .{actual});
                    return error.TestExpectedExpression;
                },
            }
        },

        .string_dq_literal => {
            switch (actual.*) {
                .string_dq_literal => |lit| try expectEqualStrings(expected.*.string_dq_literal.value, lit.value),
                else => {
                    std.debug.print("expected .string, found {}\n", .{actual});
                    return error.TestExpectedExpression;
                },
            }
        },

        .function => {
            switch (actual.*) {
                .function => |function| try expectFunction(&expected.*.function, &function),
                else => {
                    std.debug.print("expected .function, found {}\n", .{actual});
                    return error.TestExpectedExpression;
                },
            }
        },

        .assignment => {
            switch (actual.*) {
                .assignment => |assignment| try expectAssignment(&expected.*.assignment, &assignment),
                else => {
                    std.debug.print("expected .assignment, found {}\n", .{actual});
                    return error.TestExpectedExpression;
                },
            }
        },

        .variable => {
            switch (actual.*) {
                .variable => |variable| try expectVariable(&expected.*.variable, &variable),
                else => {
                    std.debug.print("expected .variable, found {}\n", .{actual});
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

fn expectFunction(expected: *const ast.Function, actual: *const ast.Function) !void {
    try expectIdentifier(&expected.*.name, &actual.*.name);

    try expectEqual(@as(usize, expected.*.parameters.items.len), actual.*.parameters.items.len);
    for (0..expected.parameters.items.len) |i| {
        try expectVariable(&expected.*.parameters.items[i], &actual.*.parameters.items[i]);
    }

    try expectBlock(&expected.*.body, &actual.*.body);
}

fn expectBlock(expected: *const ast.Block, actual: *const ast.Block) anyerror!void {
    try expectEqual(@as(usize, expected.*.statements.items.len), actual.*.statements.items.len);
    for (0..expected.statements.items.len) |i| {
        const expectedStatement = &expected.*.statements.items[i];
        const actualStatement = &actual.*.statements.items[i];
        try expectStatement(expectedStatement, actualStatement);
    }
}

fn expectAssignment(expected: *const ast.Assignment, actual: *const ast.Assignment) anyerror!void {
    try expectVariable(&expected.*.name, &actual.*.name);
    try expectExpression(expected.*.value, actual.*.value);
}

fn expectStatement(expected: *const ast.Statement, actual: *const ast.Statement) !void {
    switch (expected.*) {
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
    }
}

test "assignment statements" {
    {
        try parseProgramForTesting("$x = 5;", struct {
            fn function(program: *const ast.Program) !void {
                var integer = ast.Expression{ .integer = ast.Integer{ .value = 5 } };

                var expression = ast.Expression{
                    .assignment = ast.Assignment{
                        .name = ast.Variable{ .value = "x" },
                        .value = &integer,
                    },
                };

                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{
                        .expression = &expression,
                    },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("$y = 20 * 20;", struct {
            fn function(program: *const ast.Program) !void {
                var left = ast.Expression{ .integer = ast.Integer{ .value = 20 } };
                var right = ast.Expression{ .integer = ast.Integer{ .value = 20 } };

                const infixExpression = ast.InfixExpression{ .left = &left, .operator = ast.Operator.asterisk, .right = &right };

                var expression = ast.Expression{ .infixExpression = infixExpression };

                var assignmentExpression = ast.Expression{
                    .assignment = ast.Assignment{
                        .name = ast.Variable{ .value = "y" },
                        .value = &expression,
                    },
                };

                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{
                        .expression = &assignmentExpression,
                    },
                }, program);
            }
        }.function);
    }

    {
        try parseProgramForTesting("$a = $b = 10;", struct {
            fn function(program: *const ast.Program) !void {
                var integer = ast.Expression{ .integer = ast.Integer{ .value = 10 } };

                const assignment = ast.Assignment{
                    .name = ast.Variable{ .value = "b" },
                    .value = &integer,
                };

                var expression = ast.Expression{ .assignment = assignment };

                var assignmentExpression = ast.Expression{
                    .assignment = ast.Assignment{
                        .name = ast.Variable{ .value = "a" },
                        .value = &expression,
                    },
                };

                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{
                        .expression = &assignmentExpression,
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

    {
        try parseProgramForTesting("33.203;", struct {
            fn function(progam: *const ast.Program) !void {
                var float = ast.Expression{ .float = ast.Float{ .value = 33.203 } };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &float },
                }, progam);
            }
        }.function);
    }

    {
        try parseProgramForTesting("\"hello\";", struct {
            fn function(program: *const ast.Program) !void {
                var str = ast.Expression{ .string_dq_literal = ast.StringLiteral{ .value = "hello" } };
                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &str },
                }, program);
            }
        }.function);
    }
}

test "function literals" {
    {
        try parseProgramForTesting("function add($x, $y) { $x + $y }", struct {
            fn function(program: *const ast.Program) !void {
                const x = ast.Variable{ .value = "x" };
                const y = ast.Variable{ .value = "y" };

                var parameters = std.ArrayList(ast.Variable).init(std.testing.allocator);
                defer parameters.deinit();

                parameters.append(x) catch return error.TestFunctionStatement;

                parameters.append(y) catch return error.TestFunctionStatement;

                var leftExpr = ast.Expression{
                    .variable = ast.Variable{ .value = "x" },
                };
                var rightExpr = ast.Expression{
                    .variable = ast.Variable{ .value = "y" },
                };
                var blockExpr = ast.Expression{
                    .infixExpression = ast.InfixExpression{
                        .left = &leftExpr,
                        .operator = ast.Operator.plus,
                        .right = &rightExpr,
                    },
                };

                var body = std.ArrayList(ast.Statement).init(std.testing.allocator);
                defer body.deinit();
                body.append(ast.Statement{
                    .expressionStatement = ast.ExpressionStatement{ .expression = &blockExpr },
                }) catch return error.TestFunctionStatement;

                const block = ast.Block{ .statements = body };

                var funExpr = ast.Expression{
                    .function = ast.Function{
                        .name = ast.Identifier{ .value = "add" },
                        .parameters = parameters,
                        .body = block,
                    },
                };

                const expressionStatement = ast.ExpressionStatement{
                    .expression = &funExpr,
                };

                try expectOneStatementInProgram(&ast.Statement{
                    .expressionStatement = expressionStatement,
                }, program);
            }
        }.function);
    }
}
