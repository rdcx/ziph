const std = @import("std");

pub const TokenType = enum {
    // Control Symbols
    OPEN_TAG,
    OPEN_TAG_WITH_ECHO,
    SHORT_OPEN_TAG,
    CLOSE_TAG,
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    SEMICOLON,
    ILLEGAL,
    LEFT_BRACKET,
    RIGHT_BRACKET,

    //
    EOF,

    // Identifiers and types
    VARIABLE,
    IDENT,
    INTEGER,
    STRING,
    STRING_SINGLE_QUOTE,
    FLOAT,
    BOOL,
    TYPE,

    // Keywords
    FUNCTION,
    CLASS,
    INTERFACE,
    TRAIT,
    NAMESPACE,
    USE,
    CONST,
    VAR,
    PUBLIC,
    PROTECTED,
    PRIVATE,
    STATIC,
    ABSTRACT,
    FINAL,
    EXTENDS,
    IMPLEMENTS,
    RETURN,
    IF,
    ELSE,
    ELSEIF,
    WHILE,
    DO,
    FOR,
    FOREACH,
    SWITCH,
    CASE,
    DEFAULT,
    BREAK,
    MATCH,
    CONTINUE,
    GOTO,
    THROW,
    TRY,
    CATCH,
    FINALLY,
    YIELD,
    AS,
    INSTANCEOF,
    INSTEADOF,
    GLOBAL,
    DECLARE,
    ECHO,
    PRINT,
    INCLUDE,
    INCLUDE_ONCE,
    REQUIRE,
    REQUIRE_ONCE,

    // Operators
    PLUS,
    MINUS,
    ASSIGN,
    COMMA,

    // Special operators
    COLON,
    DOUBLE_COLON,
    OBJECT_OPERATOR,
    NULL_SAFE_OBJECT_OPERATOR,
    COALESCE,
    DOUBLE_ARROW,
    ELIPSIS,
};

pub const Keyword = struct {
    name: []const u8,
    token_type: TokenType,
};

pub const Keywords = [_]Keyword{
    // Tags
    Keyword{ .name = "<?php", .token_type = TokenType.OPEN_TAG },
    Keyword{ .name = "<?=", .token_type = TokenType.OPEN_TAG_WITH_ECHO },
    Keyword{ .name = "<?", .token_type = TokenType.SHORT_OPEN_TAG },
    Keyword{ .name = "?>", .token_type = TokenType.CLOSE_TAG },

    // Types
    Keyword{ .name = "int", .token_type = TokenType.TYPE },
    Keyword{ .name = "float", .token_type = TokenType.TYPE },
    Keyword{ .name = "string", .token_type = TokenType.TYPE },
    Keyword{ .name = "bool", .token_type = TokenType.TYPE },
    Keyword{ .name = "callable", .token_type = TokenType.TYPE },
    Keyword{ .name = "void", .token_type = TokenType.TYPE },
    Keyword{ .name = "mixed", .token_type = TokenType.TYPE },
    Keyword{ .name = "iterable", .token_type = TokenType.TYPE },
    Keyword{ .name = "object", .token_type = TokenType.TYPE },
    Keyword{ .name = "array", .token_type = TokenType.TYPE },

    Keyword{ .name = "class", .token_type = TokenType.CLASS },
    Keyword{ .name = "interface", .token_type = TokenType.INTERFACE },
    Keyword{ .name = "trait", .token_type = TokenType.TRAIT },
    Keyword{ .name = "namespace", .token_type = TokenType.NAMESPACE },
    Keyword{ .name = "use", .token_type = TokenType.USE },
    Keyword{ .name = "function", .token_type = TokenType.FUNCTION },
    Keyword{ .name = "return", .token_type = TokenType.RETURN },
    Keyword{ .name = "if", .token_type = TokenType.IF },
    Keyword{ .name = "else", .token_type = TokenType.ELSE },
    Keyword{ .name = "elseif", .token_type = TokenType.ELSEIF },
    Keyword{ .name = "while", .token_type = TokenType.WHILE },
    Keyword{ .name = "do", .token_type = TokenType.DO },
    Keyword{ .name = "for", .token_type = TokenType.FOR },
    Keyword{ .name = "foreach", .token_type = TokenType.FOREACH },
    Keyword{ .name = "switch", .token_type = TokenType.SWITCH },
    Keyword{ .name = "case", .token_type = TokenType.CASE },
    Keyword{ .name = "default", .token_type = TokenType.DEFAULT },
    Keyword{ .name = "break", .token_type = TokenType.BREAK },
    Keyword{ .name = "match", .token_type = TokenType.MATCH },
    Keyword{ .name = "continue", .token_type = TokenType.CONTINUE },
    Keyword{ .name = "goto", .token_type = TokenType.GOTO },
    Keyword{ .name = "throw", .token_type = TokenType.THROW },
    Keyword{ .name = "try", .token_type = TokenType.TRY },
    Keyword{ .name = "catch", .token_type = TokenType.CATCH },
    Keyword{ .name = "finally", .token_type = TokenType.FINALLY },
    Keyword{ .name = "yield", .token_type = TokenType.YIELD },
    Keyword{ .name = "as", .token_type = TokenType.AS },
    Keyword{ .name = "instanceof", .token_type = TokenType.INSTANCEOF },
    Keyword{ .name = "insteadof", .token_type = TokenType.INSTEADOF },
    Keyword{ .name = "global", .token_type = TokenType.GLOBAL },
    Keyword{ .name = "declare", .token_type = TokenType.DECLARE },

    Keyword{ .name = "echo", .token_type = TokenType.ECHO },
};

pub fn getKeyword(keyword: []const u8) ?TokenType {
    for (Keywords) |kw| {
        if (std.mem.eql(u8, kw.name, keyword)) {
            return kw.token_type;
        }
    }

    return null;
}

test "getKeyword" {
    try std.testing.expect(getKeyword("<?php") == TokenType.OPEN_TAG);
    try std.testing.expect(getKeyword("<?=") == TokenType.OPEN_TAG_WITH_ECHO);
    try std.testing.expect(getKeyword("<?") == TokenType.SHORT_OPEN_TAG);
    try std.testing.expect(getKeyword("?>") == TokenType.CLOSE_TAG);
    try std.testing.expect(getKeyword("echo") == TokenType.ECHO);
    try std.testing.expect(getKeyword("function") == TokenType.FUNCTION);

    try std.testing.expect(getKeyword("int") == TokenType.TYPE);
    try std.testing.expect(getKeyword("float") == TokenType.TYPE);
    try std.testing.expect(getKeyword("string") == TokenType.TYPE);
    try std.testing.expect(getKeyword("bool") == TokenType.TYPE);
    try std.testing.expect(getKeyword("callable") == TokenType.TYPE);
    try std.testing.expect(getKeyword("void") == TokenType.TYPE);
    try std.testing.expect(getKeyword("mixed") == TokenType.TYPE);
    try std.testing.expect(getKeyword("iterable") == TokenType.TYPE);
    try std.testing.expect(getKeyword("object") == TokenType.TYPE);
    try std.testing.expect(getKeyword("array") == TokenType.TYPE);

    try std.testing.expect(getKeyword("class") == TokenType.CLASS);
    try std.testing.expect(getKeyword("interface") == TokenType.INTERFACE);
    try std.testing.expect(getKeyword("trait") == TokenType.TRAIT);
    try std.testing.expect(getKeyword("namespace") == TokenType.NAMESPACE);
    try std.testing.expect(getKeyword("use") == TokenType.USE);

    try std.testing.expect(getKeyword("return") == TokenType.RETURN);
    try std.testing.expect(getKeyword("if") == TokenType.IF);
    try std.testing.expect(getKeyword("else") == TokenType.ELSE);
    try std.testing.expect(getKeyword("elseif") == TokenType.ELSEIF);
    try std.testing.expect(getKeyword("while") == TokenType.WHILE);
    try std.testing.expect(getKeyword("do") == TokenType.DO);
    try std.testing.expect(getKeyword("for") == TokenType.FOR);
    try std.testing.expect(getKeyword("foreach") == TokenType.FOREACH);
    try std.testing.expect(getKeyword("switch") == TokenType.SWITCH);
    try std.testing.expect(getKeyword("case") == TokenType.CASE);
    try std.testing.expect(getKeyword("default") == TokenType.DEFAULT);
    try std.testing.expect(getKeyword("break") == TokenType.BREAK);
    try std.testing.expect(getKeyword("continue") == TokenType.CONTINUE);
    try std.testing.expect(getKeyword("goto") == TokenType.GOTO);
    try std.testing.expect(getKeyword("throw") == TokenType.THROW);
    try std.testing.expect(getKeyword("try") == TokenType.TRY);
    try std.testing.expect(getKeyword("catch") == TokenType.CATCH);
    try std.testing.expect(getKeyword("finally") == TokenType.FINALLY);
    try std.testing.expect(getKeyword("yield") == TokenType.YIELD);
    try std.testing.expect(getKeyword("as") == TokenType.AS);
    try std.testing.expect(getKeyword("instanceof") == TokenType.INSTANCEOF);
    try std.testing.expect(getKeyword("insteadof") == TokenType.INSTEADOF);
    try std.testing.expect(getKeyword("global") == TokenType.GLOBAL);
    try std.testing.expect(getKeyword("declare") == TokenType.DECLARE);

    try std.testing.expect(getKeyword("invalid") == null);
}

// Define the Token struct
pub const Token = struct {
    token_type: TokenType,
    literal: []const u8,
};

pub fn newToken(token_type: TokenType, literal: []const u8) Token {
    return Token{ .token_type = token_type, .literal = literal };
}
