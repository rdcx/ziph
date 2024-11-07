const std = @import("std");
const token = @import("token.zig");

test {
    std.testing.refAllDecls(@This());
}

const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: ?u8,

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

        tok = self.detectIntegerLiteral();
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

        tok = self.detectAssign();
        if (tok != null) return tok.?;

        tok = self.detectColon();
        if (tok != null) return tok.?;

        tok = self.detectComma();
        if (tok != null) return tok.?;

        tok = self.detectEOF();
        if (tok != null) return tok.?;

        return token.TokenTag.illegal;
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
        var l = New(input);

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
        var l = New(input);

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
        var l = New(input);

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
        var l = New(input);
        l.ch = 0;

        const tok = l.detectEOF().?;
        try std.testing.expect(tok == token.TokenTag.eof);
    }

    fn detectIntegerLiteral(self: *Lexer) ?token.Token {
        if (!isDigit(self.ch)) {
            return null;
        }

        const int = self.readInt();
        return token.Token{ .integer_literal = int };
    }

    test "detectIntegerLiteral returns integer_literal token" {
        const input = "12345;";
        var l = New(input);

        const tok = l.detectIntegerLiteral().?;
        try std.testing.expect(tok == token.TokenTag.integer_literal);
        try std.testing.expect(std.mem.eql(u8, "12345", tok.integer_literal));
    }

    fn detectIdentifier(self: *Lexer) ?token.Token {
        if (!isLetter(self.ch)) {
            return null;
        }

        const ident = self.readIdentifier();

        return lookupIdent(ident);
    }
    test "detectIdentifier returns ident token" {
        const input = "person";
        var l = New(input);

        const tok = l.detectIdentifier().?;
        try std.testing.expect(tok == token.TokenTag.ident);
        try std.testing.expect(std.mem.eql(u8, "person", tok.ident));
    }

    test "detectIdentifier returns function token" {
        const input = "function";
        var l = New(input);

        const tok = l.detectIdentifier().?;
        try std.testing.expect(tok == token.TokenTag.function);
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
        var l = New(input);

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
        var l = New(input);

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
        var l = New(input);

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
        var l = New(input);

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
        var l = New(input);

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
        var l = New(input);

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
        var l = New(input);

        const tok = l.detectMinus().?;
        try std.testing.expect(tok == token.TokenTag.minus);
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
        var l = New(input);

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
            return token.Token{ .string_double_quote_literal = str };
        }

        if (self.ch == '\'') {
            self.readChar();
            const position = self.position;
            while (self.ch != '\'') {
                self.readChar();
            }
            const str = self.input[position..self.position];
            self.readChar();
            return token.Token{ .string_single_quote_literal = str };
        }
        return null;
    }

    test "detectString returns string_double_quote_literal token" {
        const input = "\"Hello, World!\"";
        var l = New(input);

        const tok = l.detectString().?;
        try std.testing.expect(tok == token.TokenTag.string_double_quote_literal);
        try std.testing.expect(std.mem.eql(u8, "Hello, World!", tok.string_double_quote_literal));
    }

    test "detectString returns string_single_quote_literal token" {
        const input = "'Hello, World!'";
        var l = New(input);

        const tok = l.detectString().?;
        try std.testing.expect(tok == token.TokenTag.string_single_quote_literal);
        try std.testing.expect(std.mem.eql(u8, "Hello, World!", tok.string_single_quote_literal));
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
        var l = New(input);

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
        var l = New(input);

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
        var l = New(input);

        const tok = l.detectColon().?;
        try std.testing.expect(tok == token.TokenTag.colon);
    }

    test "detectColon returns double_colon token" {
        const input = "::";
        var l = New(input);

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
        var l = New(input);

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
        var l = New(input);

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
        var l = New(input);

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
        var l = New(input);

        try std.testing.expect(l.isObjectOperator());
    }

    test "isObjectOperator returns false" {
        const input = "-";
        var l = New(input);

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

pub fn New(input: []const u8) Lexer {
    var ch: u8 = 0;

    if (input.len > 0) {
        ch = input[0];
    }

    var l = Lexer{ .input = input, .position = 0, .read_position = 0, .ch = ch };
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

fn lookupIdent(ident: []const u8) token.Token {
    if (isFunction(ident)) return token.TokenTag.function;
    if (isFn(ident)) return token.TokenTag.fn_;
    if (isClass(ident)) return token.TokenTag.class;
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

    return token.Token{ .ident = ident };
}

test "lookupIdent" {
    try std.testing.expect(lookupIdent("function") == token.TokenTag.function);
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
