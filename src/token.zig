const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,
    PHP_OPEN_TAG,
    PHP_SHORT_OPEN_TAG,
    PHP_CLOSE_TAG,
    IDENT,
    INTEGER,
    ECHO,
    SEMICOLON,
    VARIABLE,
};

pub const Keyword = struct {
    name: []const u8,
    token_type: TokenType,
};

pub const Keywords = [_]Keyword{
    Keyword{ .name = "<?php", .token_type = TokenType.PHP_OPEN_TAG },
    Keyword{ .name = "<?", .token_type = TokenType.PHP_SHORT_OPEN_TAG },
    Keyword{ .name = "?>", .token_type = TokenType.PHP_CLOSE_TAG },
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

test "Test getKeyword" {
    try std.testing.expect(getKeyword("<?php") == TokenType.PHP_OPEN_TAG);
    try std.testing.expect(getKeyword("<?") == TokenType.PHP_SHORT_OPEN_TAG);
    try std.testing.expect(getKeyword("?>") == TokenType.PHP_CLOSE_TAG);
    try std.testing.expect(getKeyword("echo") == TokenType.ECHO);
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
