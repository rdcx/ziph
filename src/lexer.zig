const std = @import("std");
const token = @import("token.zig");

test {
    std.testing.refAllDecls(@This());
}

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
        while (isLetter(self.ch) or isBackslash(self.ch) or (self.position != position and isDigit(self.ch))) {
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

        var tok = self.detectSemicolon();
        if (tok != null) return tok.?;

        tok = self.detectOpenTag();
        if (tok != null) return tok.?;

        tok = self.detectCloseTag();
        if (tok != null) return tok.?;

        tok = self.detectVariable();
        if (tok != null) return tok.?;

        tok = self.detectLeftParen();
        if (tok != null) return tok.?;

        tok = self.detectRightParen();
        if (tok != null) return tok.?;

        tok = self.detectLeftBrace();
        if (tok != null) return tok.?;

        tok = self.detectRightBrace();
        if (tok != null) return tok.?;

        tok = self.detectLeftBracket();
        if (tok != null) return tok.?;

        tok = self.detectRightBracket();
        if (tok != null) return tok.?;

        tok = self.detectInteger();
        if (tok != null) return tok.?;

        tok = self.detectString();
        if (tok != null) return tok.?;

        tok = self.detectIdentifier();
        if (tok != null) return tok.?;

        tok = self.detectPlus();
        if (tok != null) return tok.?;

        tok = self.detectMinus();
        if (tok != null) return tok.?;

        tok = self.detectAssign();
        if (tok != null) return tok.?;

        tok = self.detectColon();
        if (tok != null) return tok.?;

        tok = self.detectEOF();
        if (tok != null) return tok.?;

        return token.newToken(token.TokenType.ILLEGAL, "");
    }

    fn detectSemicolon(self: *Lexer) ?token.Token {
        if (self.ch == ';') {
            const tok = token.newToken(token.TokenType.SEMICOLON, ";");
            self.readChar();
            return tok;
        }
        return null;
    }

    test "detectSemicolon returns SEMICOLON token" {
        const input = ";";
        var l = New(input);

        const tok = l.detectSemicolon().?;
        try std.testing.expect(tok.token_type == token.TokenType.SEMICOLON);
        try std.testing.expect(std.mem.eql(u8, ";", tok.literal));
    }

    fn detectOpenTag(self: *Lexer) ?token.Token {
        if (self.isOpenTag()) {
            const tok = token.newToken(token.TokenType.OPEN_TAG, "<?php");
            self.jumpLiteral("<?php");
            return tok;
        }

        if (self.isShortOpenTag()) {
            const tok = token.newToken(token.TokenType.SHORT_OPEN_TAG, "<?");
            self.jumpLiteral("<?");
            return tok;
        }
        return null;
    }

    test "detectOpenTag returns OPEN_TAG token" {
        const input = "<?php";
        var l = New(input);

        const tok = l.detectOpenTag().?;
        try std.testing.expect(tok.token_type == token.TokenType.OPEN_TAG);
        try std.testing.expect(std.mem.eql(u8, "<?php", tok.literal));
    }

    fn detectCloseTag(self: *Lexer) ?token.Token {
        if (self.isCloseTag()) {
            const tok = token.newToken(token.TokenType.CLOSE_TAG, "?>");
            self.jumpLiteral("?>");
            return tok;
        }
        return null;
    }

    test "detectCloseTag returns CLOSE_TAG token" {
        const input = "?>";
        var l = New(input);

        const tok = l.detectCloseTag().?;
        try std.testing.expect(tok.token_type == token.TokenType.CLOSE_TAG);
        try std.testing.expect(std.mem.eql(u8, "?>", tok.literal));
    }

    fn detectEOF(self: *Lexer) ?token.Token {
        if (isEOF(self.ch)) {
            return token.newToken(token.TokenType.EOF, "EOF");
        }
        return null;
    }

    test "detectEOF returns EOF token" {
        const input = "";
        var l = New(input);
        l.ch = 0;

        const tok = l.detectEOF().?;
        try std.testing.expect(tok.token_type == token.TokenType.EOF);
        try std.testing.expect(std.mem.eql(u8, "EOF", tok.literal));
    }

    fn detectInteger(self: *Lexer) ?token.Token {
        if (!isDigit(self.ch)) {
            return null;
        }

        const int = self.readInt();
        return token.newToken(token.TokenType.INTEGER, int);
    }

    test "detectInteger returns INTEGER token" {
        const input = "12345;";
        var l = New(input);

        const tok = l.detectInteger().?;
        try std.testing.expect(tok.token_type == token.TokenType.INTEGER);
        try std.testing.expect(std.mem.eql(u8, "12345", tok.literal));
    }

    fn detectIdentifier(self: *Lexer) ?token.Token {
        if (!isLetter(self.ch)) {
            return null;
        }

        const ident = self.readIdentifier();
        const kw = token.getKeyword(ident);
        if (kw != null) {
            return token.newToken(kw.?, ident);
        }

        return token.newToken(token.TokenType.IDENT, ident);
    }

    test "detectIdentifier returns IDENT token" {
        const input = "person";
        var l = New(input);

        const tok = l.detectIdentifier().?;
        try std.testing.expect(tok.token_type == token.TokenType.IDENT);
        try std.testing.expect(std.mem.eql(u8, "person", tok.literal));
    }

    test "detectIdentifier returns KEYWORD token" {
        const input = "echo";
        var l = New(input);

        const tok = l.detectIdentifier().?;
        try std.testing.expect(tok.token_type == token.TokenType.ECHO);
        try std.testing.expect(std.mem.eql(u8, "echo", tok.literal));
    }

    fn detectVariable(self: *Lexer) ?token.Token {
        if (self.ch != '$') {
            return null;
        }

        self.readChar();
        const ident = self.readIdentifier();
        return token.newToken(token.TokenType.VARIABLE, ident);
    }

    test "detectVariable returns VARIABLE token" {
        const input = "$world;";
        var l = New(input);

        const tok = l.detectVariable().?;
        try std.testing.expect(tok.token_type == token.TokenType.VARIABLE);
        try std.testing.expect(std.mem.eql(u8, "world", tok.literal));
    }

    fn detectLeftParen(self: *Lexer) ?token.Token {
        if (self.ch == '(') {
            const tok = token.newToken(token.TokenType.LEFT_PAREN, "(");
            self.readChar();
            return tok;
        }
        return null;
    }

    test "detectLeftParen returns LEFT_PAREN token" {
        const input = "(";
        var l = New(input);

        const tok = l.detectLeftParen().?;
        try std.testing.expect(tok.token_type == token.TokenType.LEFT_PAREN);
        try std.testing.expect(std.mem.eql(u8, "(", tok.literal));
    }

    fn detectRightParen(self: *Lexer) ?token.Token {
        if (self.ch == ')') {
            const tok = token.newToken(token.TokenType.RIGHT_PAREN, ")");
            self.readChar();
            return tok;
        }
        return null;
    }

    test "detectRightParen returns RIGHT_PAREN token" {
        const input = ")";
        var l = New(input);

        const tok = l.detectRightParen().?;
        try std.testing.expect(tok.token_type == token.TokenType.RIGHT_PAREN);
        try std.testing.expect(std.mem.eql(u8, ")", tok.literal));
    }

    fn detectLeftBrace(self: *Lexer) ?token.Token {
        if (self.ch == '{') {
            const tok = token.newToken(token.TokenType.LEFT_BRACE, "{");
            self.readChar();
            return tok;
        }
        return null;
    }

    test "detectLeftBrace returns LEFT_BRACE token" {
        const input = "{";
        var l = New(input);

        const tok = l.detectLeftBrace().?;
        try std.testing.expect(tok.token_type == token.TokenType.LEFT_BRACE);
        try std.testing.expect(std.mem.eql(u8, "{", tok.literal));
    }

    fn detectRightBrace(self: *Lexer) ?token.Token {
        if (self.ch == '}') {
            const tok = token.newToken(token.TokenType.RIGHT_BRACE, "}");
            self.readChar();
            return tok;
        }
        return null;
    }

    test "detectRightBrace returns RIGHT_BRACE token" {
        const input = "}";
        var l = New(input);

        const tok = l.detectRightBrace().?;
        try std.testing.expect(tok.token_type == token.TokenType.RIGHT_BRACE);
        try std.testing.expect(std.mem.eql(u8, "}", tok.literal));
    }

    fn detectPlus(self: *Lexer) ?token.Token {
        if (self.ch == '+') {
            const tok = token.newToken(token.TokenType.PLUS, "+");
            self.readChar();
            return tok;
        }
        return null;
    }

    test "detectPlus returns PLUS token" {
        const input = "+";
        var l = New(input);

        const tok = l.detectPlus().?;
        try std.testing.expect(tok.token_type == token.TokenType.PLUS);
        try std.testing.expect(std.mem.eql(u8, "+", tok.literal));
    }

    fn detectMinus(self: *Lexer) ?token.Token {
        if (self.ch == '-') {
            const tok = token.newToken(token.TokenType.MINUS, "-");
            self.readChar();
            return tok;
        }
        return null;
    }

    test "detectMinus returns MINUS token" {
        const input = "-";
        var l = New(input);

        const tok = l.detectMinus().?;
        try std.testing.expect(tok.token_type == token.TokenType.MINUS);
        try std.testing.expect(std.mem.eql(u8, "-", tok.literal));
    }

    fn detectAssign(self: *Lexer) ?token.Token {
        if (self.ch == '=') {
            const tok = token.newToken(token.TokenType.ASSIGN, "=");
            self.readChar();
            return tok;
        }

        return null;
    }

    test "detectAssign returns ASSIGN token" {
        const input = "=";
        var l = New(input);

        const tok = l.detectAssign().?;
        try std.testing.expect(tok.token_type == token.TokenType.ASSIGN);
        try std.testing.expect(std.mem.eql(u8, "=", tok.literal));
    }

    fn detectString(self: *Lexer) ?token.Token {
        if (self.ch == '"') {
            self.readChar();
            const position = self.position;
            while (self.ch != '"') {
                self.readChar();
            }
            const str = self.input[position..self.position];
            self.readChar();
            return token.newToken(token.TokenType.STRING, str);
        }
        return null;
    }

    test "detectString returns STRING token" {
        const input = "\"Hello, World!\"";
        var l = New(input);

        const tok = l.detectString().?;
        try std.testing.expect(tok.token_type == token.TokenType.STRING);
        try std.testing.expect(std.mem.eql(u8, "Hello, World!", tok.literal));
    }

    fn detectLeftBracket(self: *Lexer) ?token.Token {
        if (self.ch == '[') {
            const tok = token.newToken(token.TokenType.LEFT_BRACKET, "[");
            self.readChar();
            return tok;
        }
        return null;
    }

    test "detectLeftBracket returns LEFT_BRACKET token" {
        const input = "[";
        var l = New(input);

        const tok = l.detectLeftBracket().?;
        try std.testing.expect(tok.token_type == token.TokenType.LEFT_BRACKET);
        try std.testing.expect(std.mem.eql(u8, "[", tok.literal));
    }

    fn detectRightBracket(self: *Lexer) ?token.Token {
        if (self.ch == ']') {
            const tok = token.newToken(token.TokenType.RIGHT_BRACKET, "]");
            self.readChar();
            return tok;
        }
        return null;
    }

    test "detectRightBracket returns RIGHT_BRACKET token" {
        const input = "]";
        var l = New(input);

        const tok = l.detectRightBracket().?;
        try std.testing.expect(tok.token_type == token.TokenType.RIGHT_BRACKET);
        try std.testing.expect(std.mem.eql(u8, "]", tok.literal));
    }

    fn detectColon(self: *Lexer) ?token.Token {
        if (self.ch == ':') {
            self.readChar();
            if (self.ch == ':') {
                const tok = token.newToken(token.TokenType.DOUBLE_COLON, "::");
                self.readChar();
                return tok;
            }

            return token.newToken(token.TokenType.COLON, ":");
        }
        return null;
    }

    test "detectColon returns COLON token" {
        const input = ":";
        var l = New(input);

        const tok = l.detectColon().?;
        try std.testing.expect(tok.token_type == token.TokenType.COLON);
        try std.testing.expect(std.mem.eql(u8, ":", tok.literal));
    }

    test "detectColon returns DOUBLE_COLON token" {
        const input = "::";
        var l = New(input);

        const tok = l.detectColon().?;
        try std.testing.expect(tok.token_type == token.TokenType.DOUBLE_COLON);
        try std.testing.expect(std.mem.eql(u8, "::", tok.literal));
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
    var ch: u8 = 0;

    if (input.len > 0) {
        ch = input[0];
    }

    var l = Lexer{ .input = input, .position = 0, .read_position = 0, .ch = ch };
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

test "readIdentifier returns person" {
    const input = "person";
    var l = New(input);

    const ident = l.readIdentifier();
    try std.testing.expect(std.mem.eql(u8, "person", ident));
}

test "readIdentifier returns App\\Person" {
    const input = "App\\Person";
    var l = New(input);

    const ident = l.readIdentifier();
    try std.testing.expect(std.mem.eql(u8, "App\\Person", ident));
}

test "nextToken skips whitespace" {
    const input = " \t\n\r;";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.SEMICOLON);
    try std.testing.expect(std.mem.eql(u8, ";", tok.literal));
}

test "isOpenTag returns true" {
    const input = "<?php";
    var l = New(input);

    try std.testing.expect(l.isOpenTag());
}

test "isOpenTag returns false" {
    const input = "ab<?p";
    var l = New(input);

    try std.testing.expect(!l.isOpenTag());
}

test "isShortOpenTag returns true" {
    const input = "<?";
    var l = New(input);

    try std.testing.expect(l.isShortOpenTag());
}

test "isShortOpenTag returns false" {
    const input = "_<?";
    var l = New(input);

    try std.testing.expect(!l.isShortOpenTag());
}

test "isCloseTag returns true" {
    const input = "?>";
    var l = New(input);

    try std.testing.expect(l.isCloseTag());
}

test "isCloseTag returns false" {
    const input = "d?>";
    var l = New(input);

    try std.testing.expect(!l.isCloseTag());
}

test "nextToken returns OPEN_TAG" {
    const input = "\n<?php";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.OPEN_TAG);
    try std.testing.expect(std.mem.eql(u8, "<?php", tok.literal));
}

test "nextToken returns keyword ECHO" {
    const input = "echo";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.ECHO);
    try std.testing.expect(std.mem.eql(u8, "echo", tok.literal));
}

test "nextToken returns keyword FUNCTION" {
    const input = "function";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.FUNCTION);
    try std.testing.expect(std.mem.eql(u8, "function", tok.literal));
}

test "nextToken returns VARIABLE" {
    const input = "$world;";
    var l = New(input);

    const tok = l.nextToken();

    try std.testing.expect(tok.token_type == token.TokenType.VARIABLE);
    try std.testing.expect(std.mem.eql(u8, "world", tok.literal));
}

test "nextToken returns INTEGER" {
    const input = "10;";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.INTEGER);
    try std.testing.expect(std.mem.eql(u8, "10", tok.literal));
}

test "nextToken returns LEFT_BRACE" {
    const input = "{";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.LEFT_BRACE);
    try std.testing.expect(std.mem.eql(u8, "{", tok.literal));
}

test "nextToken returns RIGHT_BRACE" {
    const input = "}";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.RIGHT_BRACE);
    try std.testing.expect(std.mem.eql(u8, "}", tok.literal));
}

test "nextToken returns LEFT_PAREN" {
    const input = "(";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.LEFT_PAREN);
    try std.testing.expect(std.mem.eql(u8, "(", tok.literal));
}

test "nextToken returns RIGHT_PAREN" {
    const input = ")";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.RIGHT_PAREN);
    try std.testing.expect(std.mem.eql(u8, ")", tok.literal));
}

test "nextToken returns PLUS" {
    const input = "+";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.PLUS);
    try std.testing.expect(std.mem.eql(u8, "+", tok.literal));
}

test "nextToken returns MINUS" {
    const input = "-";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.MINUS);
    try std.testing.expect(std.mem.eql(u8, "-", tok.literal));
}

test "nextToken returns ASSIGN" {
    const input = "=";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.ASSIGN);
    try std.testing.expect(std.mem.eql(u8, "=", tok.literal));
}

test "nextToken returns TYPE int" {
    const input = "int";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.TYPE);
    try std.testing.expect(std.mem.eql(u8, "int", tok.literal));
}

test "nextToken returns TYPE float" {
    const input = "float";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.TYPE);
    try std.testing.expect(std.mem.eql(u8, "float", tok.literal));
}

test "nextToken returns TYPE string" {
    const input = "string";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.TYPE);
    try std.testing.expect(std.mem.eql(u8, "string", tok.literal));
}

test "nextToken returns TYPE bool" {
    const input = "bool";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.TYPE);
    try std.testing.expect(std.mem.eql(u8, "bool", tok.literal));
}

test "nextToken returns STRING" {
    const input = "\"Hello, World!\"";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.STRING);
    try std.testing.expect(std.mem.eql(u8, "Hello, World!", tok.literal));
}

test "nextToken returns LEFT_BRACKET" {
    const input = "[";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.LEFT_BRACKET);
    try std.testing.expect(std.mem.eql(u8, "[", tok.literal));
}

test "nextToken returns RIGHT_BRACKET" {
    const input = "]";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.RIGHT_BRACKET);
    try std.testing.expect(std.mem.eql(u8, "]", tok.literal));
}

test "nextToken returns COLON" {
    const input = ":";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.COLON);
    try std.testing.expect(std.mem.eql(u8, ":", tok.literal));
}

test "nextToken returns DOUBLE_COLON" {
    const input = "::";
    var l = New(input);

    const tok = l.nextToken();
    try std.testing.expect(tok.token_type == token.TokenType.DOUBLE_COLON);
    try std.testing.expect(std.mem.eql(u8, "::", tok.literal));
}

test "nextToken single line" {
    const TestCase = struct {
        expected_type: token.TokenType,
        expected_literal: []const u8,
    };
    const input = "<?php echo 10; ?>";

    // Define your test cases array
    const test_cases = [_]TestCase{
        TestCase{ .expected_type = token.TokenType.OPEN_TAG, .expected_literal = "<?php" },
        TestCase{ .expected_type = token.TokenType.ECHO, .expected_literal = "echo" },
        TestCase{ .expected_type = token.TokenType.INTEGER, .expected_literal = "10" },
        TestCase{ .expected_type = token.TokenType.SEMICOLON, .expected_literal = ";" },
        TestCase{ .expected_type = token.TokenType.CLOSE_TAG, .expected_literal = "?>" },
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

test "Test isEOF" {
    try std.testing.expect(isEOF(0));
    try std.testing.expect(!isEOF('a'));
    try std.testing.expect(!isEOF('Z'));
    try std.testing.expect(!isEOF('_'));
    try std.testing.expect(!isEOF(';'));
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

pub fn isBackslash(ch: u8) bool {
    return ch == '\\';
}

test "Test isBackslash" {
    try std.testing.expect(isBackslash('\\'));
    try std.testing.expect(!isBackslash('a'));
    try std.testing.expect(!isBackslash('Z'));
    try std.testing.expect(!isBackslash('_'));
    try std.testing.expect(!isBackslash(';'));
}
