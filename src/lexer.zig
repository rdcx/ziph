const std = @import("std");
const token = @import("token.zig");

const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: u8,

    fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn readIdentifier(self: *Lexer) []const u8 {
        const position = self.position;
        while (isLetter(self.ch)) {
            self.readChar();
        }
        return self.input[position..self.position];
    }

    pub fn skipWhitespace(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') {
            self.readChar();
        }
    }

    pub fn jumpLiteral(self: *Lexer, literal: []const u8) void {
        for (0..literal.len) |_| {
            self.readChar();
        }
    }

    pub fn readInt(self: *Lexer) []const u8 {
        const position = self.position;
        while (isDigit(self.ch)) {
            self.readChar();
        }
        return self.input[position..self.position];
    }

    pub fn nextToken(self: *Lexer) token.Token {
        self.skipWhitespace();

        // default token type is EOF as it’s the last token
        if (self.ch == ';') {
            const tok = token.newToken(token.TokenType.SEMICOLON, ";");
            self.readChar();
            return tok;
        } else if (self.ch == '<') {
            if (self.isOpenTag()) {
                const tok = token.newToken(token.TokenType.PHP_OPEN_TAG, "<?php");
                self.jumpLiteral(tok.literal);
                return tok;
            } else if (self.isShortOpenTag()) {
                const tok = token.newToken(token.TokenType.PHP_SHORT_OPEN_TAG, "<?");
                self.jumpLiteral(tok.literal);
                return tok;
            }
        } else if (self.ch == '?') {
            if (self.isCloseTag()) {
                const tok = token.newToken(token.TokenType.PHP_CLOSE_TAG, "?>");
                self.jumpLiteral(tok.literal);
                return tok;
            }
        } else if (self.ch == '$') {
            // skip the dollar sign
            self.readChar();
            // readIdentifier already advances the position
            const ident = self.readIdentifier();
            const tok = token.newToken(token.TokenType.VARIABLE, ident);
            return tok;
        } else if (isDigit(self.ch)) {
            // readInt already advances the position
            const int = self.readInt();
            const tok = token.newToken(token.TokenType.INTEGER, int);
            return tok;
        } else if (isLetter(self.ch)) {
            // readIdentifier already advances the position
            const ident = self.readIdentifier();
            var t = token.TokenType.IDENT;
            const kw = token.getKeyword(ident);
            if (kw != null) {
                t = kw.?;
            }
            const tok = token.newToken(t, ident);
            return tok;
        } else if (isEOF(self.ch)) {
            const tok = token.newToken(token.TokenType.EOF, "EOF");
            return tok;
        } else {
            return token.newToken(token.TokenType.ILLEGAL, "");
        }

        return token.newToken(token.TokenType.ILLEGAL, "");
    }

    fn isOpenTag(self: *Lexer) bool {
        const expected = "<?php";

        // Ensure there’s enough room in input for the expected string
        if (self.position + expected.len > self.input.len) {
            return false;
        }

        for (0..expected.len) |i| {
            if (self.input[self.position + i] != expected[i]) {
                return false;
            }
        }
        return true;
    }

    fn isShortOpenTag(self: *Lexer) bool {
        const expected = "<?";

        if (self.position + expected.len > self.input.len) {
            return false;
        }

        for (0..expected.len) |i| {
            if (self.input[self.position + i] != expected[i]) {
                return false;
            }
        }

        return true;
    }

    fn isCloseTag(self: *Lexer) bool {
        const expected = "?>";

        if (self.position + expected.len > self.input.len) {
            return false;
        }

        for (0..expected.len) |i| {
            if (self.input[self.position + i] != expected[i]) {
                return false;
            }
        }

        return true;
    }
};

pub fn New(input: []const u8) Lexer {
    var l = Lexer{ .input = input, .position = 0, .read_position = 0, .ch = input[0] };
    l.readChar();
    return l;
}

test "Test jumpLiteral" {
    const input = "echo 10;";
    var l = New(input);

    l.jumpLiteral("echo");
    try std.testing.expect(l.ch == ' ');
}

test "Test readInt" {
    const input = "12345;";
    var l = New(input);

    const int = l.readInt();
    try std.testing.expect(std.mem.eql(u8, "12345", int));
    try std.testing.expect(l.ch == ';');
}

test "Test readChar" {
    const input = "echo 10;";
    var l = New(input);

    try std.testing.expect(l.ch == 'e');
    l.readChar();
    try std.testing.expect(l.ch == 'c');
    l.readChar();
    try std.testing.expect(l.ch == 'h');
    l.readChar();
    try std.testing.expect(l.ch == 'o');
    l.readChar();
    try std.testing.expect(l.ch == ' ');
    l.readChar();
    try std.testing.expect(l.ch == '1');
    l.readChar();
    try std.testing.expect(l.ch == '0');
    l.readChar();
    try std.testing.expect(l.ch == ';');
    l.readChar();
    try std.testing.expect(l.ch == 0);
}

test "Test readIdentifier" {
    const input = "person";
    var l = New(input);

    const ident = l.readIdentifier();
    try std.testing.expect(std.mem.eql(u8, "person", ident));
}

test "Test nextToken skips whitespace" {
    const input = " \t\n\r;";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.SEMICOLON);
    try std.testing.expect(std.mem.eql(u8, ";", tok.literal));
}

test "Test isOpenTag" {
    const input = "<?php";
    var l = New(input);

    try std.testing.expect(l.isOpenTag());
}

test "Test not isOpenTag" {
    const input = "ab<?p";
    var l = New(input);

    try std.testing.expect(!l.isOpenTag());
}

test "Test isShortOpenTag" {
    const input = "<?";
    var l = New(input);

    try std.testing.expect(l.isShortOpenTag());
}

test "Test not isShortOpenTag" {
    const input = "_<?";
    var l = New(input);

    try std.testing.expect(!l.isShortOpenTag());
}

test "Test isCloseTag" {
    const input = "?>";
    var l = New(input);

    try std.testing.expect(l.isCloseTag());
}

test "Test not isCloseTag" {
    const input = "d?>";
    var l = New(input);

    try std.testing.expect(!l.isCloseTag());
}

test "Test nextToken isOpenTag" {
    const input = "\n<?php";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.PHP_OPEN_TAG);
    try std.testing.expect(std.mem.eql(u8, "<?php", tok.literal));
}

test "Test nextToken keyword echo" {
    const input = "echo";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.ECHO);
    try std.testing.expect(std.mem.eql(u8, "echo", tok.literal));
}

test "Test nextToken variable identifier" {
    const input = "$world;";
    var l = New(input);

    const tok = l.nextToken();

    try std.testing.expect(tok.token_type == token.TokenType.VARIABLE);
    try std.testing.expect(std.mem.eql(u8, "world", tok.literal));
}

test "Test nextToken single line" {
    const TestCase = struct {
        expected_type: token.TokenType,
        expected_literal: []const u8,
    };
    const input = "<?php echo 10; ?>";

    // Define your test cases array
    const test_cases = [_]TestCase{
        TestCase{ .expected_type = token.TokenType.PHP_OPEN_TAG, .expected_literal = "<?php" },
        TestCase{ .expected_type = token.TokenType.ECHO, .expected_literal = "echo" },
        TestCase{ .expected_type = token.TokenType.INTEGER, .expected_literal = "10" },
        TestCase{ .expected_type = token.TokenType.SEMICOLON, .expected_literal = ";" },
        TestCase{ .expected_type = token.TokenType.PHP_CLOSE_TAG, .expected_literal = "?>" },
        TestCase{ .expected_type = token.TokenType.EOF, .expected_literal = "EOF" },
        // Add more test cases as needed
    };

    var l = New(input);

    for (test_cases) |test_case| {
        // Assume nextToken function generates a token to be tested
        const tok = l.nextToken();
        var failure = false;
        // Check if the generated token matches the expected type and literal
        if (test_case.expected_type != tok.token_type) {
            failure = true;
            std.debug.print("Expected type: {}, Actual type: {}\n", .{ test_case.expected_type, tok.token_type });
        }
        if (!std.mem.eql(u8, test_case.expected_literal, tok.literal)) {
            failure = true;
            std.debug.print("Expected literal: {s}, Actual literal: {s}\n", .{ test_case.expected_literal, tok.literal });
        }

        if (failure) {
            return error.TypeOrLiteralMismatch;
        }
    }
}

pub fn isEOF(ch: u8) bool {
    return ch == 0;
}

pub fn isLetter(ch: u8) bool {
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or ch == '_';
}

test "Test isLetter" {
    try std.testing.expect(isLetter('a'));
    try std.testing.expect(isLetter('Z'));
    try std.testing.expect(isLetter('_'));
    try std.testing.expect(!isLetter('0'));
    try std.testing.expect(!isLetter('-'));
}

pub fn isDigit(ch: u8) bool {
    return ch >= '0' and ch <= '9';
}

test "Test isDigit" {
    try std.testing.expect(isDigit('0'));
    try std.testing.expect(isDigit('9'));
    try std.testing.expect(!isDigit('a'));
    try std.testing.expect(!isDigit('Z'));
    try std.testing.expect(!isDigit('_'));
    try std.testing.expect(!isDigit(';'));
}
