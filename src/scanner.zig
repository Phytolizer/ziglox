const std = @import("std");

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

pub const TokenKind = enum {
    tk_left_paren,
    tk_right_paren,
    tk_left_brace,
    tk_right_brace,
    tk_comma,
    tk_dot,
    tk_minus,
    tk_plus,
    tk_semicolon,
    tk_slash,
    tk_star,

    tk_bang,
    tk_bang_equal,
    tk_equal,
    tk_equal_equal,
    tk_greater,
    tk_greater_equal,
    tk_less,
    tk_less_equal,

    tk_identifier,
    tk_string,
    tk_number,

    tk_and,
    tk_class,
    tk_else,
    tk_false,
    tk_for,
    tk_fun,
    tk_if,
    tk_nil,
    tk_or,
    tk_print,
    tk_return,
    tk_super,
    tk_this,
    tk_true,
    tk_var,
    tk_while,

    tk_error,
    tk_eof,
};

pub const Token = struct {
    kind: TokenKind,
    text: []const u8,
    line: usize,
};

pub const Scanner = struct {
    source: []const u8,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    const Self = @This();

    pub fn init(source: []const u8) Self {
        return Self{
            .source = source,
        };
    }

    pub fn scanToken(self: *Self) Token {
        self.skipWhitespace();
        self.start = self.current;

        if (self.isAtEnd()) {
            return self.makeToken(.tk_eof);
        }

        const c = self.advance();
        switch (c) {
            '(' => return self.makeToken(.tk_left_paren),
            ')' => return self.makeToken(.tk_right_paren),
            '{' => return self.makeToken(.tk_left_brace),
            '}' => return self.makeToken(.tk_right_brace),
            ';' => return self.makeToken(.tk_semicolon),
            ',' => return self.makeToken(.tk_comma),
            '.' => return self.makeToken(.tk_dot),
            '-' => return self.makeToken(.tk_minus),
            '+' => return self.makeToken(.tk_plus),
            '/' => return self.makeToken(.tk_slash),
            '*' => return self.makeToken(.tk_star),
            '!' => return self.makeToken(if (self.match('=')) .tk_bang_equal else .tk_bang),
            '=' => return self.makeToken(if (self.match('=')) .tk_equal_equal else .tk_equal),
            '>' => return self.makeToken(if (self.match('=')) .tk_greater_equal else .tk_greater),
            '<' => return self.makeToken(if (self.match('=')) .tk_less_equal else .tk_less),
            '"' => return self.string(),
            else => {
                if (isAlpha(c)) {
                    return self.identifier();
                }
                if (isDigit(c)) {
                    return self.number();
                }
            },
        }

        return self.errorToken("Unexpected character.");
    }

    fn skipWhitespace(self: *Self) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn string(self: *Self) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            return self.errorToken("Unterminated string.");
        }
        _ = self.advance();
        return self.makeToken(.tk_string);
    }

    fn number(self: *Self) Token {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();
            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        return self.makeToken(.tk_number);
    }

    fn identifier(self: *Self) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(self.identifierKind());
    }

    fn identifierKind(self: *Self) TokenKind {
        switch (self.source[self.start]) {
            'a' => return self.checkKeyword(1, "nd", .tk_and),
            'c' => return self.checkKeyword(1, "lass", .tk_class),
            'e' => return self.checkKeyword(1, "lse", .tk_else),
            'f' => {
                if (self.current - self.start > 1) {
                    switch (self.source[self.start + 1]) {
                        'a' => return self.checkKeyword(2, "lse", .tk_false),
                        'o' => return self.checkKeyword(2, "r", .tk_for),
                        'u' => return self.checkKeyword(2, "n", .tk_fun),
                        else => {},
                    }
                }
            },
            'i' => return self.checkKeyword(1, "f", .tk_if),
            'n' => return self.checkKeyword(1, "il", .tk_nil),
            'o' => return self.checkKeyword(1, "r", .tk_or),
            'p' => return self.checkKeyword(1, "rint", .tk_print),
            'r' => return self.checkKeyword(1, "eturn", .tk_return),
            's' => return self.checkKeyword(1, "uper", .tk_super),
            't' => {
                if (self.current - self.start > 1) {
                    switch (self.source[self.start + 1]) {
                        'h' => return self.checkKeyword(2, "is", .tk_this),
                        'r' => return self.checkKeyword(2, "ue", .tk_true),
                        else => {},
                    }
                }
            },
            'v' => return self.checkKeyword(1, "ar", .tk_var),
            'w' => return self.checkKeyword(1, "hile", .tk_while),
            else => {},
        }
        return .tk_identifier;
    }

    fn checkKeyword(
        self: *Self,
        comptime start: usize,
        comptime expected: []const u8,
        comptime kind: TokenKind,
    ) TokenKind {
        return if (std.mem.eql(u8, self.source[self.start + start .. self.current], expected))
            kind
        else
            .tk_identifier;
    }

    fn peek(self: *Self) u8 {
        return if (self.isAtEnd())
            0
        else
            self.source[self.current];
    }

    fn peekNext(self: *Self) u8 {
        return if (self.current + 1 >= self.source.len)
            0
        else
            self.source[self.current + 1];
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) {
            return false;
        }

        if (self.source[self.current] != expected) {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn advance(self: *Self) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }

    fn makeToken(self: *Self, kind: TokenKind) Token {
        return Token{
            .kind = kind,
            .text = self.source[self.start..self.current],
            .line = self.line,
        };
    }

    fn errorToken(self: *Self, comptime message: []const u8) Token {
        return Token{
            .kind = .tk_error,
            .text = message,
            .line = self.line,
        };
    }
};
