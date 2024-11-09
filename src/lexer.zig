const std = @import("std");
const token = @import("token.zig");

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

test {
    std.testing.refAllDecls(@This());
}

pub const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: ?u8,
    line_n: usize,
    col_n: usize,

    fn readChar(self: *Lexer) void {
        if (self.ch == '\n') {
            self.line_n += 1;
            self.col_n = 0;
        }

        self.col_n += 1;

        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peekChar(self: *Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        }
        return self.input[self.read_position];
    }

    test "peekChar returns next character" {
        const input = "Hello";
        var l = new(input);

        try expect(l.peekChar() == 'e');
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const position = self.position;
        while (isLetter(self.ch) or isBackslash(self.ch) or (self.position != position and isDigit(self.ch))) {
            self.readChar();
        }
        return self.input[position..self.position];
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') {
            self.readChar();
        }
    }

    fn skipComment(self: *Lexer) void {
        while (self.ch != '\n') {
            self.readChar();
        }
        self.readChar();
    }

    test "skipComment skips single-line comments" {
        const input = "// This is a comment\n";
        var l = new(input);

        l.skipComment();
        try expect(l.ch == 0);
    }

    fn skipMultilineComment(self: *Lexer) void {
        while (true) {
            if (self.ch == '*' and self.peekChar() == '/') {
                self.readChar();
                self.readChar();
                break;
            }
            self.readChar();
        }
    }

    test "skipMultilineComment skips multi-line comments" {
        const input = "/**\n     * Assert that a condition is true.\n     */";
        var l = new(input);

        l.skipMultilineComment();
        try expect(l.ch == 0);
    }

    fn jumpLiteral(self: *Lexer, literal: []const u8) void {
        for (0..literal.len) |_| {
            self.readChar();
        }
    }

    test "Test jumpLiteral" {
        const input = "echo 10;";
        var l = new(input);

        l.jumpLiteral("echo");
        try std.testing.expect(l.ch == ' ');
    }

    fn readInt(self: *Lexer) []const u8 {
        const position = self.position;
        while (isDigit(self.ch)) {
            self.readChar();
        }
        return self.input[position..self.position];
    }

    test "Test readInt" {
        const input = "12345;";
        var l = new(input);

        const int = l.readInt();
        try std.testing.expect(std.mem.eql(u8, "12345", int));
        try std.testing.expect(l.ch == ';');
    }

    pub fn nextToken(self: *Lexer) token.Token {
        self.skipWhitespace();
        if (self.ch == '/' and self.peekChar() == '/') {
            self.skipComment();

            return self.nextToken();
        }

        if (self.ch == '/' and self.peekChar() == '*') {
            self.skipMultilineComment();

            return self.nextToken();
        }

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

        tok = self.detectNumber();
        if (tok != null) return tok.?;

        tok = self.detectString();
        if (tok != null) return tok.?;

        tok = self.detectIdentifier();
        if (tok != null) return tok.?;

        tok = self.detectPlus();
        if (tok != null) return tok.?;

        // Detect object operator before minus operator
        tok = self.detectObjectOperator();
        if (tok != null) return tok.?;

        tok = self.detectMinus();
        if (tok != null) return tok.?;

        // Detect double arrow before assign operator
        tok = self.detectDoubleArrow();
        if (tok != null) return tok.?;

        tok = self.detectEqual();
        if (tok != null) return tok.?;

        // Detect not equal before bang operator
        tok = self.detectNotEqual();
        if (tok != null) return tok.?;

        tok = self.detectBang();
        if (tok != null) return tok.?;

        tok = self.detectAssign();
        if (tok != null) return tok.?;

        tok = self.detectColon();
        if (tok != null) return tok.?;

        tok = self.detectComma();
        if (tok != null) return tok.?;

        tok = self.detectHashSymbol();
        if (tok != null) return tok.?;

        tok = self.detectLogicalAnd();
        if (tok != null) return tok.?;

        tok = self.detectLogicalOr();
        if (tok != null) return tok.?;

        tok = self.detectQuestionMark();
        if (tok != null) return tok.?;

        tok = self.detectAsterisk();
        if (tok != null) return tok.?;

        tok = self.detectSlash();
        if (tok != null) return tok.?;

        tok = self.detectGreaterThan();
        if (tok != null) return tok.?;

        tok = self.detectLessThan();
        if (tok != null) return tok.?;

        tok = self.detectEOF();
        if (tok != null) return tok.?;

        return token.TokenTag.illegal;
    }

    fn detectNumber(self: *Lexer) ?token.Token {
        if (!isDigit(self.ch)) {
            return null;
        }

        const startPosition = self.position;
        const int = self.readInt();
        if (self.ch != '.') {
            return token.Token{ .integer_literal = int };
        }

        self.readChar();
        while (isDigit(self.ch)) {
            self.readChar();
        }
        const float = self.input[startPosition..self.position];
        return token.Token{ .float_literal = float };
    }

    test "detectNumber returns integer_literal token" {
        const input = "12345;";
        var l = new(input);

        const tok = l.detectNumber().?;
        try std.testing.expect(tok == token.TokenTag.integer_literal);
        try std.testing.expect(std.mem.eql(u8, "12345", tok.integer_literal));
    }

    test "detectNumber returns float_literal token" {
        const input = "123.45;";
        var l = new(input);

        const tok = l.detectNumber().?;
        try std.testing.expect(tok == token.TokenTag.float_literal);
        try std.testing.expect(std.mem.eql(u8, "123.45", tok.float_literal));
    }

    fn detectQuestionMark(self: *Lexer) ?token.Token {
        if (self.ch == '?') {
            self.readChar();
            return token.TokenTag.question_mark;
        }
        return null;
    }

    test "detectQuestionMark returns question_mark token" {
        const input = "?";
        var l = new(input);

        const tok = l.detectQuestionMark().?;
        try std.testing.expect(tok == token.TokenTag.question_mark);
    }

    fn detectLogicalAnd(self: *Lexer) ?token.Token {
        if (self.ch == '&' and self.peekChar() == '&') {
            self.readChar();
            self.readChar();
            return token.TokenTag.logical_and;
        }
        return null;
    }

    test "detectLogicalAnd returns logical_and token" {
        const input = "&&";
        var l = new(input);

        const tok = l.detectLogicalAnd().?;
        try std.testing.expect(tok == token.TokenTag.logical_and);
    }

    fn detectLogicalOr(self: *Lexer) ?token.Token {
        if (self.ch == '|' and self.peekChar() == '|') {
            self.readChar();
            self.readChar();
            return token.TokenTag.logical_or;
        }
        return null;
    }

    test "detectLogicalOr returns logical_or token" {
        const input = "||";
        var l = new(input);

        const tok = l.detectLogicalOr().?;
        try std.testing.expect(tok == token.TokenTag.logical_or);
    }

    fn detectHashSymbol(self: *Lexer) ?token.Token {
        if (self.ch == '#') {
            self.readChar();
            return token.TokenTag.hash_symbol;
        }
        return null;
    }

    test "detectHashSymbol returns hash_symbol token" {
        const input = "#";
        var l = new(input);

        const tok = l.detectHashSymbol().?;
        try std.testing.expect(tok == token.TokenTag.hash_symbol);
    }

    fn detectBang(self: *Lexer) ?token.Token {
        if (self.ch == '!') {
            self.readChar();
            return token.TokenTag.bang;
        }
        return null;
    }

    test "detectBang returns BANG token" {
        const input = "!";
        var l = new(input);

        const tok = l.detectBang().?;
        try std.testing.expect(tok == token.TokenTag.bang);
    }

    fn detectEqual(self: *Lexer) ?token.Token {
        if (self.ch == '=' and self.peekChar() == '=') {
            self.readChar();
            if (self.peekChar() == '=') {
                self.readChar();
                self.readChar();
                return token.TokenTag.identical;
            }
            return token.TokenTag.equal;
        }
        return null;
    }

    test "detectEqual returns equal token" {
        const input = "==";
        var l = new(input);

        const tok = l.detectEqual().?;
        try std.testing.expect(tok == token.TokenTag.equal);
    }

    test "detectEqual returns identical token" {
        const input = "===";
        var l = new(input);

        const tok = l.detectEqual().?;
        try std.testing.expect(tok == token.TokenTag.identical);
    }

    fn detectNotEqual(self: *Lexer) ?token.Token {
        if (self.ch == '!' and self.peekChar() == '=') {
            self.readChar();
            if (self.peekChar() == '=') {
                self.readChar();
                self.readChar();
                return token.TokenTag.not_identical;
            }
            return token.TokenTag.not_equal;
        }
        return null;
    }

    test "detectNotEqual returns not_equal token" {
        const input = "!=";
        var l = new(input);

        const tok = l.detectNotEqual().?;
        try std.testing.expect(tok == token.TokenTag.not_equal);
    }

    test "detectNotEqual returns not_identical token" {
        const input = "!== $world;";
        var l = new(input);

        const tok = l.detectNotEqual().?;
        try std.testing.expect(tok == token.TokenTag.not_identical);

        try std.testing.expect(l.nextToken() == token.TokenTag.variable);
    }

    fn detectSemicolon(self: *Lexer) ?token.Token {
        if (self.ch == ';') {
            const tok = token.TokenTag.semicolon;
            self.readChar();
            return tok;
        }
        return null;
    }

    test "detectSemicolon returns SEMICOLON token" {
        const input = ";";
        var l = new(input);

        const tok = l.detectSemicolon().?;
        try std.testing.expect(tok == token.TokenTag.semicolon);
    }

    fn detectOpenTag(self: *Lexer) ?token.Token {
        if (self.isOpenTag()) {
            const tok = token.TokenTag.open_tag;
            self.jumpLiteral("<?php");
            return tok;
        }

        if (self.isShortOpenTag()) {
            const tok = token.TokenTag.open_tag;
            self.jumpLiteral("<?");
            return tok;
        }
        return null;
    }

    test "detectOpenTag returns open_tag token" {
        const input = "<?php";
        var l = new(input);

        const tok = l.detectOpenTag().?;
        try std.testing.expect(tok == token.TokenTag.open_tag);
    }

    fn detectCloseTag(self: *Lexer) ?token.Token {
        if (self.isCloseTag()) {
            const tok = token.TokenTag.close_tag;
            self.jumpLiteral("?>");
            return tok;
        }
        return null;
    }

    test "detectCloseTag returns close_tag token" {
        const input = "?>";
        var l = new(input);

        const tok = l.detectCloseTag().?;
        try std.testing.expect(tok == token.TokenTag.close_tag);
    }

    fn detectEOF(self: *Lexer) ?token.Token {
        if (isEOF(self.ch)) {
            return token.TokenTag.eof;
        }
        return null;
    }

    test "detectEOF returns eof token" {
        const input = "";
        var l = new(input);
        l.ch = 0;

        const tok = l.detectEOF().?;
        try std.testing.expect(tok == token.TokenTag.eof);
    }

    fn detectIdentifier(self: *Lexer) ?token.Token {
        if (!isLetter(self.ch) and !isBackslash(self.ch)) {
            return null;
        }

        const ident = self.readIdentifier();

        return lookupIdent(ident);
    }
    test "detectIdentifier returns ident token" {
        const input = "person";
        var l = new(input);

        const tok = l.detectIdentifier().?;
        try std.testing.expect(tok == token.TokenTag.ident);
        try std.testing.expect(std.mem.eql(u8, "person", tok.ident));
    }

    test "detectIdentifier returns function token" {
        const input = "function";
        var l = new(input);

        const tok = l.detectIdentifier().?;
        try std.testing.expect(tok == token.TokenTag.function);
    }

    test "detectIdentifier allows backslash" {
        const input = "\\SensitivePrameter";
        var l = new(input);

        const tok = l.detectIdentifier().?;
        try std.testing.expect(tok == token.TokenTag.ident);
    }

    fn detectVariable(self: *Lexer) ?token.Token {
        if (self.ch != '$') {
            return null;
        }

        self.readChar();
        const ident = self.readIdentifier();
        return token.Token{ .variable = ident };
    }

    test "detectVariable returns variable token" {
        const input = "$world;";
        var l = new(input);

        const tok = l.detectVariable().?;
        try std.testing.expect(tok == token.TokenTag.variable);
        try std.testing.expect(std.mem.eql(u8, "world", tok.variable));
    }

    fn detectLeftParen(self: *Lexer) ?token.Token {
        if (self.ch == '(') {
            self.readChar();
            return token.TokenTag.left_paren;
        }
        return null;
    }

    test "detectLeftParen returns left_paren token" {
        const input = "(";
        var l = new(input);

        const tok = l.detectLeftParen().?;
        try std.testing.expect(tok == token.TokenTag.left_paren);
    }

    fn detectRightParen(self: *Lexer) ?token.Token {
        if (self.ch == ')') {
            self.readChar();
            return token.TokenTag.right_paren;
        }
        return null;
    }

    test "detectRightParen returns right_paren token" {
        const input = ")";
        var l = new(input);

        const tok = l.detectRightParen().?;
        try std.testing.expect(tok == token.TokenTag.right_paren);
    }

    fn detectLeftBrace(self: *Lexer) ?token.Token {
        if (self.ch == '{') {
            self.readChar();
            return token.TokenTag.left_brace;
        }
        return null;
    }

    test "detectLeftBrace returns left_brace token" {
        const input = "{";
        var l = new(input);

        const tok = l.detectLeftBrace().?;
        try std.testing.expect(tok == token.TokenTag.left_brace);
    }

    fn detectRightBrace(self: *Lexer) ?token.Token {
        if (self.ch == '}') {
            self.readChar();
            return token.TokenTag.right_brace;
        }
        return null;
    }

    test "detectRightBrace returns right_brace token" {
        const input = "}";
        var l = new(input);

        const tok = l.detectRightBrace().?;
        try std.testing.expect(tok == token.TokenTag.right_brace);
    }

    fn detectPlus(self: *Lexer) ?token.Token {
        if (self.ch == '+') {
            self.readChar();
            return token.TokenTag.plus;
        }
        return null;
    }

    test "detectPlus returns plus token" {
        const input = "+";
        var l = new(input);

        const tok = l.detectPlus().?;
        try std.testing.expect(tok == token.TokenTag.plus);
    }

    fn detectMinus(self: *Lexer) ?token.Token {
        if (self.ch == '-') {
            self.readChar();
            return token.TokenTag.minus;
        }
        return null;
    }

    test "detectMinus returns minus token" {
        const input = "-";
        var l = new(input);

        const tok = l.detectMinus().?;
        try std.testing.expect(tok == token.TokenTag.minus);
    }

    fn detectAsterisk(self: *Lexer) ?token.Token {
        if (self.ch == '*') {
            self.readChar();
            return token.TokenTag.asterisk;
        }
        return null;
    }

    test "detectAsterisk returns asterisk token" {
        const input = "*";
        var l = new(input);

        const tok = l.detectAsterisk().?;
        try std.testing.expect(tok == token.TokenTag.asterisk);
    }

    fn detectSlash(self: *Lexer) ?token.Token {
        if (self.ch == '/') {
            self.readChar();
            return token.TokenTag.slash;
        }
        return null;
    }

    test "detectSlash returns slash token" {
        const input = "/";
        var l = new(input);

        const tok = l.detectSlash().?;
        try std.testing.expect(tok == token.TokenTag.slash);
    }

    fn detectGreaterThan(self: *Lexer) ?token.Token {
        if (self.ch == '>') {
            self.readChar();
            if (self.ch == '=') {
                self.readChar();
                return token.TokenTag.gte;
            }
            return token.TokenTag.gt;
        }
        return null;
    }

    test "detectGreaterThan returns gt token" {
        const input = ">";
        var l = new(input);

        const tok = l.detectGreaterThan().?;
        try std.testing.expect(tok == token.TokenTag.gt);
    }

    test "detectGreaterThan returns gte token" {
        const input = ">=";
        var l = new(input);

        const tok = l.detectGreaterThan().?;
        try std.testing.expect(tok == token.TokenTag.gte);
    }

    fn detectLessThan(self: *Lexer) ?token.Token {
        if (self.ch == '<') {
            self.readChar();
            if (self.ch == '=') {
                self.readChar();
                return token.TokenTag.lte;
            }
            return token.TokenTag.lt;
        }
        return null;
    }

    test "detectLessThan returns lt token" {
        const input = "<";
        var l = new(input);

        const tok = l.detectLessThan().?;
        try std.testing.expect(tok == token.TokenTag.lt);
    }

    test "detectLessThan returns lte token" {
        const input = "<=";
        var l = new(input);

        const tok = l.detectLessThan().?;
        try std.testing.expect(tok == token.TokenTag.lte);
    }

    fn detectAssign(self: *Lexer) ?token.Token {
        if (self.ch == '=') {
            self.readChar();
            return token.TokenTag.assign;
        }
        return null;
    }

    test "detectAssign returns assign token" {
        const input = "=";
        var l = new(input);

        const tok = l.detectAssign().?;
        try std.testing.expect(tok == token.TokenTag.assign);
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
            return token.Token{ .string_dq_literal = str };
        }

        if (self.ch == '\'') {
            self.readChar();
            const position = self.position;
            while (self.ch != '\'') {
                self.readChar();
            }
            const str = self.input[position..self.position];
            self.readChar();
            return token.Token{ .string_sq_literal = str };
        }
        return null;
    }

    test "detectString returns string_dq_literal token" {
        const input = "\"Hello, World!\"";
        var l = new(input);

        const tok = l.detectString().?;
        try std.testing.expect(tok == token.TokenTag.string_dq_literal);
        try std.testing.expect(std.mem.eql(u8, "Hello, World!", tok.string_dq_literal));
    }

    test "detectString returns string_sq_literal token" {
        const input = "'Hello, World!'";
        var l = new(input);

        const tok = l.detectString().?;
        try std.testing.expect(tok == token.TokenTag.string_sq_literal);
        try std.testing.expect(std.mem.eql(u8, "Hello, World!", tok.string_sq_literal));
    }

    fn detectLeftBracket(self: *Lexer) ?token.Token {
        if (self.ch == '[') {
            self.readChar();
            return token.TokenTag.left_bracket;
        }
        return null;
    }

    test "detectLeftBracket returns left_bracket token" {
        const input = "[";
        var l = new(input);

        const tok = l.detectLeftBracket().?;
        try std.testing.expect(tok == token.TokenTag.left_bracket);
    }

    fn detectRightBracket(self: *Lexer) ?token.Token {
        if (self.ch == ']') {
            self.readChar();
            return token.TokenTag.right_bracket;
        }
        return null;
    }

    test "detectRightBracket returns RIGHT_BRACKET token" {
        const input = "]";
        var l = new(input);

        const tok = l.detectRightBracket().?;
        try std.testing.expect(tok == token.TokenTag.right_bracket);
    }

    fn detectColon(self: *Lexer) ?token.Token {
        if (self.ch == ':') {
            self.readChar();
            if (self.ch == ':') {
                self.jumpLiteral("::");
                return token.TokenTag.double_colon;
            }
            return token.TokenTag.colon;
        }
        return null;
    }

    test "detectColon returns colon token" {
        const input = ":";
        var l = new(input);

        const tok = l.detectColon().?;
        try std.testing.expect(tok == token.TokenTag.colon);
    }

    test "detectColon returns double_colon token" {
        const input = "::";
        var l = new(input);

        const tok = l.detectColon().?;
        try std.testing.expect(tok == token.TokenTag.double_colon);
    }

    fn detectObjectOperator(self: *Lexer) ?token.Token {
        if (self.isObjectOperator()) {
            self.jumpLiteral("->");
            return token.TokenTag.object_operator;
        }
        return null;
    }

    test "detectObjectOperator returns object_operator token" {
        const input = "->";
        var l = new(input);

        const tok = l.detectObjectOperator().?;
        try std.testing.expect(tok == token.TokenTag.object_operator);
    }

    fn detectDoubleArrow(self: *Lexer) ?token.Token {
        if (self.input.len < self.read_position + 1) {
            return null;
        }
        if (self.ch == '=' and self.input[self.read_position] == '>') {
            self.jumpLiteral("=>");
            return token.TokenTag.double_arrow;
        }
        return null;
    }

    test "detectDoubleArrow returns double_arrow token" {
        const input = "=>";
        var l = new(input);

        const tok = l.detectDoubleArrow().?;
        try std.testing.expect(tok == token.TokenTag.double_arrow);
    }

    fn detectComma(self: *Lexer) ?token.Token {
        if (self.ch == ',') {
            self.readChar();
            return token.TokenTag.comma;
        }
        return null;
    }

    test "detectComma returns COMMA token" {
        const input = ",";
        var l = new(input);

        const tok = l.detectComma().?;
        try std.testing.expect(tok == token.TokenTag.comma);
    }

    fn isObjectOperator(self: *Lexer) bool {
        if (self.input.len < self.read_position + 1) {
            return false;
        }

        return self.ch == '-' and self.input[self.read_position] == '>';
    }

    test "isObjectOperator returns true" {
        const input = "->";
        var l = new(input);

        try std.testing.expect(l.isObjectOperator());
    }

    test "isObjectOperator returns false" {
        const input = "-";
        var l = new(input);

        try std.testing.expect(!l.isObjectOperator());
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

pub fn new(input: []const u8) Lexer {
    var ch: u8 = 0;

    if (input.len > 0) {
        ch = input[0];
    }

    var l = Lexer{ .input = input, .position = 0, .read_position = 0, .ch = ch, .line_n = 1, .col_n = 1 };
    l.readChar();
    return l;
}

// Type system check functions
fn isInteger(ident: []const u8) bool {
    return std.mem.eql(u8, "integer", ident);
}

fn isString(ident: []const u8) bool {
    return std.mem.eql(u8, "string", ident);
}

fn isFloat(ident: []const u8) bool {
    return std.mem.eql(u8, "float", ident);
}

fn isBool(ident: []const u8) bool {
    return std.mem.eql(u8, "bool", ident);
}

fn isType(ident: []const u8) bool {
    return std.mem.eql(u8, "type", ident);
}

// Keyword check functions
fn isFunction(ident: []const u8) bool {
    return std.mem.eql(u8, "function", ident);
}

fn isFn(ident: []const u8) bool {
    return std.mem.eql(u8, "fn", ident);
}

fn isClass(ident: []const u8) bool {
    return std.mem.eql(u8, "class", ident);
}

fn isNew(ident: []const u8) bool {
    return std.mem.eql(u8, "new", ident);
}

fn isInterface(ident: []const u8) bool {
    return std.mem.eql(u8, "interface", ident);
}

fn isTrait(ident: []const u8) bool {
    return std.mem.eql(u8, "trait", ident);
}

fn isNamespace(ident: []const u8) bool {
    return std.mem.eql(u8, "namespace", ident);
}

fn isUse(ident: []const u8) bool {
    return std.mem.eql(u8, "use", ident);
}

fn isConst(ident: []const u8) bool {
    return std.mem.eql(u8, "const", ident);
}

fn isVar(ident: []const u8) bool {
    return std.mem.eql(u8, "var", ident);
}

fn isPublic(ident: []const u8) bool {
    return std.mem.eql(u8, "public", ident);
}

fn isProtected(ident: []const u8) bool {
    return std.mem.eql(u8, "protected", ident);
}

fn isPrivate(ident: []const u8) bool {
    return std.mem.eql(u8, "private", ident);
}

fn isStatic(ident: []const u8) bool {
    return std.mem.eql(u8, "static", ident);
}

fn isAbstract(ident: []const u8) bool {
    return std.mem.eql(u8, "abstract", ident);
}

fn isFinal(ident: []const u8) bool {
    return std.mem.eql(u8, "final", ident);
}

fn isExtends(ident: []const u8) bool {
    return std.mem.eql(u8, "extends", ident);
}

fn isImplements(ident: []const u8) bool {
    return std.mem.eql(u8, "implements", ident);
}

fn isReturn(ident: []const u8) bool {
    return std.mem.eql(u8, "return", ident);
}

fn isIf(ident: []const u8) bool {
    return std.mem.eql(u8, "if", ident);
}

fn isElse(ident: []const u8) bool {
    return std.mem.eql(u8, "else", ident);
}

fn isElseIf(ident: []const u8) bool {
    return std.mem.eql(u8, "elseif", ident);
}

fn isWhile(ident: []const u8) bool {
    return std.mem.eql(u8, "while", ident);
}

fn isDo(ident: []const u8) bool {
    return std.mem.eql(u8, "do", ident);
}

fn isFor(ident: []const u8) bool {
    return std.mem.eql(u8, "for", ident);
}

fn isForeach(ident: []const u8) bool {
    return std.mem.eql(u8, "foreach", ident);
}

fn isSwitch(ident: []const u8) bool {
    return std.mem.eql(u8, "switch", ident);
}

fn isCase(ident: []const u8) bool {
    return std.mem.eql(u8, "case", ident);
}

fn isDefault(ident: []const u8) bool {
    return std.mem.eql(u8, "default", ident);
}

fn isBreak(ident: []const u8) bool {
    return std.mem.eql(u8, "break", ident);
}

fn isMatch(ident: []const u8) bool {
    return std.mem.eql(u8, "match", ident);
}

fn isContinue(ident: []const u8) bool {
    return std.mem.eql(u8, "continue", ident);
}

fn isGoto(ident: []const u8) bool {
    return std.mem.eql(u8, "goto", ident);
}

fn isThrow(ident: []const u8) bool {
    return std.mem.eql(u8, "throw", ident);
}

fn isTry(ident: []const u8) bool {
    return std.mem.eql(u8, "try", ident);
}

fn isCatch(ident: []const u8) bool {
    return std.mem.eql(u8, "catch", ident);
}

fn isFinally(ident: []const u8) bool {
    return std.mem.eql(u8, "finally", ident);
}

fn isYield(ident: []const u8) bool {
    return std.mem.eql(u8, "yield", ident);
}

fn isAs(ident: []const u8) bool {
    return std.mem.eql(u8, "as", ident);
}

fn isInstanceOf(ident: []const u8) bool {
    return std.mem.eql(u8, "instanceof", ident);
}

fn isInsteadOf(ident: []const u8) bool {
    return std.mem.eql(u8, "insteadof", ident);
}

fn isGlobal(ident: []const u8) bool {
    return std.mem.eql(u8, "global", ident);
}

fn isDeclare(ident: []const u8) bool {
    return std.mem.eql(u8, "declare", ident);
}

fn isEcho(ident: []const u8) bool {
    return std.mem.eql(u8, "echo", ident);
}

fn isPrint(ident: []const u8) bool {
    return std.mem.eql(u8, "print", ident);
}

fn isInclude(ident: []const u8) bool {
    return std.mem.eql(u8, "include", ident);
}

fn isIncludeOnce(ident: []const u8) bool {
    return std.mem.eql(u8, "include_once", ident);
}

fn isRequire(ident: []const u8) bool {
    return std.mem.eql(u8, "require", ident);
}

fn isRequireOnce(ident: []const u8) bool {
    return std.mem.eql(u8, "require_once", ident);
}

fn isTrue(ident: []const u8) bool {
    return std.mem.eql(u8, "true", ident);
}

fn isFalse(ident: []const u8) bool {
    return std.mem.eql(u8, "false", ident);
}

fn isNull(ident: []const u8) bool {
    return std.mem.eql(u8, "null", ident);
}

fn lookupIdent(ident: []const u8) token.Token {
    // Type system checks
    if (isInteger(ident)) return token.TokenTag.integer_t;
    if (isString(ident)) return token.TokenTag.string_t;
    if (isFloat(ident)) return token.TokenTag.float_t;
    if (isBool(ident)) return token.TokenTag.bool_t;
    if (isType(ident)) return token.TokenTag.type_;

    if (isFunction(ident)) return token.TokenTag.function;
    if (isFn(ident)) return token.TokenTag.fn_;
    if (isClass(ident)) return token.TokenTag.class;
    if (isNew(ident)) return token.TokenTag.new;
    if (isInterface(ident)) return token.TokenTag.interface;
    if (isTrait(ident)) return token.TokenTag.trait;
    if (isNamespace(ident)) return token.TokenTag.namespace;
    if (isUse(ident)) return token.TokenTag.use;
    if (isConst(ident)) return token.TokenTag.const_;
    if (isVar(ident)) return token.TokenTag.var_;
    if (isPublic(ident)) return token.TokenTag.public;
    if (isProtected(ident)) return token.TokenTag.protected;
    if (isPrivate(ident)) return token.TokenTag.private;
    if (isStatic(ident)) return token.TokenTag.static;
    if (isAbstract(ident)) return token.TokenTag.abstract;
    if (isFinal(ident)) return token.TokenTag.final;
    if (isExtends(ident)) return token.TokenTag.extends;
    if (isImplements(ident)) return token.TokenTag.implements;
    if (isReturn(ident)) return token.TokenTag.return_;
    if (isIf(ident)) return token.TokenTag.if_;
    if (isElse(ident)) return token.TokenTag.else_;
    if (isElseIf(ident)) return token.TokenTag.elseif;
    if (isWhile(ident)) return token.TokenTag.while_;
    if (isDo(ident)) return token.TokenTag.do_;
    if (isFor(ident)) return token.TokenTag.for_;
    if (isForeach(ident)) return token.TokenTag.foreach;
    if (isSwitch(ident)) return token.TokenTag.switch_;
    if (isCase(ident)) return token.TokenTag.case;
    if (isDefault(ident)) return token.TokenTag.default;
    if (isBreak(ident)) return token.TokenTag.break_;
    if (isMatch(ident)) return token.TokenTag.match;
    if (isContinue(ident)) return token.TokenTag.continue_;
    if (isGoto(ident)) return token.TokenTag.goto;
    if (isThrow(ident)) return token.TokenTag.throw;
    if (isTry(ident)) return token.TokenTag.try_;
    if (isCatch(ident)) return token.TokenTag.catch_;
    if (isFinally(ident)) return token.TokenTag.finally;
    if (isYield(ident)) return token.TokenTag.yield;
    if (isAs(ident)) return token.TokenTag.as;
    if (isInstanceOf(ident)) return token.TokenTag.instanceof;
    if (isInsteadOf(ident)) return token.TokenTag.insteadof;
    if (isGlobal(ident)) return token.TokenTag.global;
    if (isDeclare(ident)) return token.TokenTag.declare;
    if (isEcho(ident)) return token.TokenTag.echo;
    if (isPrint(ident)) return token.TokenTag.print;
    if (isInclude(ident)) return token.TokenTag.include;
    if (isIncludeOnce(ident)) return token.TokenTag.include_once;
    if (isRequire(ident)) return token.TokenTag.require;
    if (isRequireOnce(ident)) return token.TokenTag.require_once;

    // Literal checks
    if (isTrue(ident)) return token.TokenTag.true_literal;
    if (isFalse(ident)) return token.TokenTag.false_literal;
    if (isNull(ident)) return token.TokenTag.null_literal;

    return token.Token{ .ident = ident };
}

test "lookupIdent" {

    // Literal tests
    try std.testing.expect(lookupIdent("true") == token.TokenTag.true_literal);
    try std.testing.expect(lookupIdent("false") == token.TokenTag.false_literal);
    try std.testing.expect(lookupIdent("null") == token.TokenTag.null_literal);

    // Type system tests
    try std.testing.expect(lookupIdent("integer") == token.TokenTag.integer_t);
    try std.testing.expect(lookupIdent("string") == token.TokenTag.string_t);
    try std.testing.expect(lookupIdent("float") == token.TokenTag.float_t);
    try std.testing.expect(lookupIdent("bool") == token.TokenTag.bool_t);
    try std.testing.expect(lookupIdent("type") == token.TokenTag.type_);

    // Keyword tests
    try std.testing.expect(lookupIdent("function") == token.TokenTag.function);
    try std.testing.expect(lookupIdent("fn") == token.TokenTag.fn_);
    try std.testing.expect(lookupIdent("class") == token.TokenTag.class);
    try std.testing.expect(lookupIdent("interface") == token.TokenTag.interface);
    try std.testing.expect(lookupIdent("trait") == token.TokenTag.trait);
    try std.testing.expect(lookupIdent("namespace") == token.TokenTag.namespace);
    try std.testing.expect(lookupIdent("use") == token.TokenTag.use);
    try std.testing.expect(lookupIdent("const") == token.TokenTag.const_);
    try std.testing.expect(lookupIdent("var") == token.TokenTag.var_);
    try std.testing.expect(lookupIdent("public") == token.TokenTag.public);
    try std.testing.expect(lookupIdent("protected") == token.TokenTag.protected);
    try std.testing.expect(lookupIdent("private") == token.TokenTag.private);
    try std.testing.expect(lookupIdent("static") == token.TokenTag.static);
    try std.testing.expect(lookupIdent("abstract") == token.TokenTag.abstract);
    try std.testing.expect(lookupIdent("final") == token.TokenTag.final);
    try std.testing.expect(lookupIdent("extends") == token.TokenTag.extends);
    try std.testing.expect(lookupIdent("implements") == token.TokenTag.implements);
    try std.testing.expect(lookupIdent("return") == token.TokenTag.return_);
    try std.testing.expect(lookupIdent("if") == token.TokenTag.if_);
    try std.testing.expect(lookupIdent("else") == token.TokenTag.else_);
    try std.testing.expect(lookupIdent("elseif") == token.TokenTag.elseif);
    try std.testing.expect(lookupIdent("while") == token.TokenTag.while_);
    try std.testing.expect(lookupIdent("do") == token.TokenTag.do_);
    try std.testing.expect(lookupIdent("for") == token.TokenTag.for_);
    try std.testing.expect(lookupIdent("foreach") == token.TokenTag.foreach);
    try std.testing.expect(lookupIdent("switch") == token.TokenTag.switch_);
    try std.testing.expect(lookupIdent("case") == token.TokenTag.case);
    try std.testing.expect(lookupIdent("default") == token.TokenTag.default);
    try std.testing.expect(lookupIdent("break") == token.TokenTag.break_);
    try std.testing.expect(lookupIdent("match") == token.TokenTag.match);
    try std.testing.expect(lookupIdent("continue") == token.TokenTag.continue_);
    try std.testing.expect(lookupIdent("goto") == token.TokenTag.goto);
    try std.testing.expect(lookupIdent("throw") == token.TokenTag.throw);
    try std.testing.expect(lookupIdent("try") == token.TokenTag.try_);
    try std.testing.expect(lookupIdent("catch") == token.TokenTag.catch_);
    try std.testing.expect(lookupIdent("finally") == token.TokenTag.finally);
    try std.testing.expect(lookupIdent("yield") == token.TokenTag.yield);
    try std.testing.expect(lookupIdent("as") == token.TokenTag.as);
    try std.testing.expect(lookupIdent("instanceof") == token.TokenTag.instanceof);
    try std.testing.expect(lookupIdent("insteadof") == token.TokenTag.insteadof);
    try std.testing.expect(lookupIdent("global") == token.TokenTag.global);
    try std.testing.expect(lookupIdent("declare") == token.TokenTag.declare);
    try std.testing.expect(lookupIdent("echo") == token.TokenTag.echo);
    try std.testing.expect(lookupIdent("print") == token.TokenTag.print);
    try std.testing.expect(lookupIdent("include") == token.TokenTag.include);
    try std.testing.expect(lookupIdent("include_once") == token.TokenTag.include_once);
    try std.testing.expect(lookupIdent("require") == token.TokenTag.require);
    try std.testing.expect(lookupIdent("require_once") == token.TokenTag.require_once);
}

test "Test readChar" {
    const input = "echo 10;";
    var l = new(input);

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
    var l = new(input);

    const ident = l.readIdentifier();
    try std.testing.expect(std.mem.eql(u8, "person", ident));
}

test "readIdentifier returns App\\Person" {
    const input = "App\\Person";
    var l = new(input);

    const ident = l.readIdentifier();
    try std.testing.expect(std.mem.eql(u8, "App\\Person", ident));
}

test "isOpenTag returns true" {
    const input = "<?php";
    var l = new(input);

    try std.testing.expect(l.isOpenTag());
}

test "isOpenTag returns false" {
    const input = "ab<?p";
    var l = new(input);

    try std.testing.expect(!l.isOpenTag());
}

test "isShortOpenTag returns true" {
    const input = "<?";
    var l = new(input);

    try std.testing.expect(l.isShortOpenTag());
}

test "isShortOpenTag returns false" {
    const input = "_<?";
    var l = new(input);

    try std.testing.expect(!l.isShortOpenTag());
}

test "isCloseTag returns true" {
    const input = "?>";
    var l = new(input);

    try std.testing.expect(l.isCloseTag());
}

test "isCloseTag returns false" {
    const input = "d?>";
    var l = new(input);

    try std.testing.expect(!l.isCloseTag());
}

pub fn isEOF(ch: ?u8) bool {
    if (ch == null) {
        return true;
    }
    return ch == 0;
}

test "Test isEOF" {
    try std.testing.expect(isEOF(0));
    try std.testing.expect(!isEOF('a'));
    try std.testing.expect(!isEOF('Z'));
    try std.testing.expect(!isEOF('_'));
    try std.testing.expect(!isEOF(';'));
}

pub fn isLetter(ch: ?u8) bool {
    if (ch == null) {
        return false;
    }

    return (ch.? >= 'a' and ch.? <= 'z') or (ch.? >= 'A' and ch.? <= 'Z') or ch.? == '_';
}

test "Test isLetter" {
    try std.testing.expect(isLetter('a'));
    try std.testing.expect(isLetter('Z'));
    try std.testing.expect(isLetter('_'));
    try std.testing.expect(!isLetter('0'));
    try std.testing.expect(!isLetter('-'));
}

pub fn isDigit(ch: ?u8) bool {
    if (ch == null) {
        return false;
    }
    return ch.? >= '0' and ch.? <= '9';
}

test "Test isDigit" {
    try std.testing.expect(isDigit('0'));
    try std.testing.expect(isDigit('9'));
    try std.testing.expect(!isDigit('a'));
    try std.testing.expect(!isDigit('Z'));
    try std.testing.expect(!isDigit('_'));
    try std.testing.expect(!isDigit(';'));
}

pub fn isBackslash(ch: ?u8) bool {
    if (ch == null) {
        return false;
    }
    return ch.? == '\\';
}

test "Test isBackslash" {
    try std.testing.expect(isBackslash('\\'));
    try std.testing.expect(!isBackslash('a'));
    try std.testing.expect(!isBackslash('Z'));
    try std.testing.expect(!isBackslash('_'));
    try std.testing.expect(!isBackslash(';'));
}

// test utils
fn expectStringInnerToken(expected: []const u8, actual: token.Token) !void {
    switch (actual) {
        token.Token.ident, token.Token.variable, token.Token.integer_literal, token.Token.float_literal, token.Token.string_dq_literal, token.Token.string_sq_literal => |value| try expectEqualStrings(expected, value),
        else => unreachable,
    }
}

fn expectIdent(expected: []const u8, actual: token.Token) !void {
    try expect(actual == .ident);
    try expectStringInnerToken(expected, actual);
}

fn expectVariable(expected: []const u8, actual: token.Token) !void {
    try expect(actual == .variable);
    try expectStringInnerToken(expected, actual);
}

fn expectInt(expected: []const u8, actual: token.Token) !void {
    try expect(actual == .integer_literal);
    try expectStringInnerToken(expected, actual);
}

fn expectStringLiteral(expected: []const u8, actual: token.Token) !void {
    try expect(actual == .string_dq_literal or actual == .string_sq_literal);
    try expectStringInnerToken(expected, actual);
}

fn expectFloat(expected: []const u8, actual: token.Token) !void {
    try expect(actual == .float_literal);
    try expectStringInnerToken(expected, actual);
}

test "EOF lexer" {
    const input = "$x = 10;";
    var lexer = new(input);

    try expectVariable("x", lexer.nextToken());
    try expectEqual(token.Token.assign, lexer.nextToken());
    try expectInt("10", lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());

    try expectEqual(token.Token.eof, lexer.nextToken());
}

test "PHP lexer" {
    const input =
        \\ <?php
        \\ 
        \\ namespace App\Classes;
        \\ 
        \\ class Test {
        \\     public function __construct() {
        \\         echo "Test class has been initiated";
        \\     }
        \\ 
        \\     public function add(int $a, int $b): int {
        \\         return $a + $b;
        \\     }
        \\ }
        \\ 
        \\ $test = new Test();
        \\ echo $test->add(5, 10);
        \\ echo $test->add(10, 20) !== 30;
        \\ echo !false;
        \\ function withParameter(string $a, #[\SensitiveParameter] string $b) { }
        \\ || && 
        \\ ? :
        \\ + - * / > < >= <= 
        \\ 3.001
        \\
        \\ function add($x, $y) {  }
    ;

    var lexer = new(input);

    // PHP open tag
    try expectEqual(token.Token.open_tag, lexer.nextToken());

    // Namespace declaration
    try expectEqual(token.Token.namespace, lexer.nextToken());
    try expectIdent("App\\Classes", lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());

    // Class declaration
    try expectEqual(token.Token.class, lexer.nextToken());
    try expectIdent("Test", lexer.nextToken());
    try expectEqual(token.Token.left_brace, lexer.nextToken());

    // Public function __construct
    try expectEqual(token.Token.public, lexer.nextToken());
    try expectEqual(token.Token.function, lexer.nextToken());
    try expectIdent("__construct", lexer.nextToken());
    try expectEqual(token.Token.left_paren, lexer.nextToken());
    try expectEqual(token.Token.right_paren, lexer.nextToken());
    try expectEqual(token.Token.left_brace, lexer.nextToken());
    try expectEqual(token.Token.echo, lexer.nextToken());
    try expectStringLiteral("Test class has been initiated", lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectEqual(token.Token.right_brace, lexer.nextToken());

    // Public function add
    try expectEqual(token.Token.public, lexer.nextToken());
    try expectEqual(token.Token.function, lexer.nextToken());
    try expectIdent("add", lexer.nextToken());
    try expectEqual(token.Token.left_paren, lexer.nextToken());
    try expectIdent("int", lexer.nextToken());
    try expectVariable("a", lexer.nextToken());
    try expectEqual(token.Token.comma, lexer.nextToken());
    try expectIdent("int", lexer.nextToken());
    try expectVariable("b", lexer.nextToken());
    try expectEqual(token.Token.right_paren, lexer.nextToken());
    try expectEqual(token.Token.colon, lexer.nextToken());
    try expectIdent("int", lexer.nextToken());
    try expectEqual(token.Token.left_brace, lexer.nextToken());
    try expectEqual(token.Token.return_, lexer.nextToken());
    try expectVariable("a", lexer.nextToken());
    try expectEqual(token.Token.plus, lexer.nextToken());
    try expectVariable("b", lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());
    try expectEqual(token.Token.right_brace, lexer.nextToken());

    // Close class
    try expectEqual(token.Token.right_brace, lexer.nextToken());

    // Variable and instantiation
    try expectVariable("test", lexer.nextToken());
    try expectEqual(token.Token.assign, lexer.nextToken());
    try expectEqual(token.Token.new, lexer.nextToken());
    try expectIdent("Test", lexer.nextToken());
    try expectEqual(token.Token.left_paren, lexer.nextToken());
    try expectEqual(token.Token.right_paren, lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());

    // Function call with arguments
    try expectEqual(token.Token.echo, lexer.nextToken());
    try expectVariable("test", lexer.nextToken());
    try expectEqual(token.Token.object_operator, lexer.nextToken());
    try expectIdent("add", lexer.nextToken());
    try expectEqual(token.Token.left_paren, lexer.nextToken());
    try expectInt("5", lexer.nextToken());
    try expectEqual(token.Token.comma, lexer.nextToken());
    try expectInt("10", lexer.nextToken());
    try expectEqual(token.Token.right_paren, lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());

    // Function call with arguments and comparison
    try expectEqual(token.Token.echo, lexer.nextToken());
    try expectVariable("test", lexer.nextToken());
    try expectEqual(token.Token.object_operator, lexer.nextToken());
    try expectIdent("add", lexer.nextToken());
    try expectEqual(token.Token.left_paren, lexer.nextToken());
    try expectInt("10", lexer.nextToken());
    try expectEqual(token.Token.comma, lexer.nextToken());
    try expectInt("20", lexer.nextToken());
    try expectEqual(token.Token.right_paren, lexer.nextToken());
    try expectEqual(token.Token.not_identical, lexer.nextToken());
    try expectInt("30", lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());

    // Echo boolean
    try expectEqual(token.Token.echo, lexer.nextToken());
    try expectEqual(token.Token.bang, lexer.nextToken());
    try expectEqual(token.Token.false_literal, lexer.nextToken());
    try expectEqual(token.Token.semicolon, lexer.nextToken());

    // Function with attributes
    try expectEqual(token.Token.function, lexer.nextToken());
    try expectIdent("withParameter", lexer.nextToken());
    try expectEqual(token.Token.left_paren, lexer.nextToken());
    try expectEqual(token.Token.string_t, lexer.nextToken());
    try expectVariable("a", lexer.nextToken());
    try expectEqual(token.Token.comma, lexer.nextToken());
    try expectEqual(token.Token.hash_symbol, lexer.nextToken());
    try expectEqual(token.Token.left_bracket, lexer.nextToken());
    try expectIdent("\\SensitiveParameter", lexer.nextToken());
    try expectEqual(token.Token.right_bracket, lexer.nextToken());
    try expectEqual(token.Token.string_t, lexer.nextToken());
    try expectVariable("b", lexer.nextToken());
    try expectEqual(token.Token.right_paren, lexer.nextToken());
    try expectEqual(token.Token.left_brace, lexer.nextToken());
    try expectEqual(token.Token.right_brace, lexer.nextToken());

    // Logical operators
    try expectEqual(token.Token.logical_or, lexer.nextToken());
    try expectEqual(token.Token.logical_and, lexer.nextToken());

    // Ternary operator
    try expectEqual(token.Token.question_mark, lexer.nextToken());
    try expectEqual(token.Token.colon, lexer.nextToken());

    // Arithmetic operators
    try expectEqual(token.Token.plus, lexer.nextToken());
    try expectEqual(token.Token.minus, lexer.nextToken());
    try expectEqual(token.Token.asterisk, lexer.nextToken());
    try expectEqual(token.Token.slash, lexer.nextToken());
    try expectEqual(token.Token.gt, lexer.nextToken());
    try expectEqual(token.Token.lt, lexer.nextToken());
    try expectEqual(token.Token.gte, lexer.nextToken());
    try expectEqual(token.Token.lte, lexer.nextToken());

    // Float
    try expectFloat("3.001", lexer.nextToken());

    // Function declaration
    try expectEqual(token.Token.function, lexer.nextToken());
    try expectIdent("add", lexer.nextToken());
    try expectEqual(token.Token.left_paren, lexer.nextToken());
    try expectVariable("x", lexer.nextToken());
    try expectEqual(token.Token.comma, lexer.nextToken());
    try expectVariable("y", lexer.nextToken());
    try expectEqual(token.Token.right_paren, lexer.nextToken());
    try expectEqual(token.Token.left_brace, lexer.nextToken());
    try expectEqual(token.Token.right_brace, lexer.nextToken());

    // End of file
    try expectEqual(token.Token.eof, lexer.nextToken());
}
