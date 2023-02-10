const std = @import("std");

const Scanner = struct {
    source: []const u8,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,
};

var scanner: Scanner = undefined;

pub fn init(source: []const u8) void {
    scanner = .{
        .source = source,
    };
}

pub const Token = struct {
    kind: Kind,
    text: []const u8,
    line: usize,

    pub const Kind = enum(usize) {
        // Single-character tokens.
        left_paren,
        right_paren,
        left_brace,
        right_brace,
        comma,
        dot,
        minus,
        plus,
        semicolon,
        slash,
        star,
        // One or two character tokens.
        bang,
        bang_equal,
        equal,
        equal_equal,
        greater,
        greater_equal,
        less,
        less_equal,
        // Literals.
        identifier,
        string,
        number,
        // Keywords.
        @"and",
        class,
        @"else",
        false,
        @"for",
        fun,
        @"if",
        nil,
        @"or",
        print,
        @"return",
        super,
        this,
        true,
        @"var",
        @"while",

        @"error",
        eof,
    };
};

fn isDigit(c: u8) bool {
    return '0' <= c and c <= '9';
}

fn isAlpha(c: u8) bool {
    return ('a' <= c and c <= 'z') or
        ('A' <= c and c <= 'Z') or
        c == '_';
}

pub fn scanToken() Token {
    skipWhitespace();
    scanner.start = scanner.current;

    if (isAtEnd()) {
        return makeToken(.eof);
    }

    const c = advance();
    if (isDigit(c)) return number();
    if (isAlpha(c)) return identifier();
    switch (c) {
        '(' => return makeToken(.left_paren),
        ')' => return makeToken(.right_paren),
        '{' => return makeToken(.left_brace),
        '}' => return makeToken(.right_brace),
        ';' => return makeToken(.semicolon),
        ',' => return makeToken(.comma),
        '.' => return makeToken(.dot),
        '-' => return makeToken(.minus),
        '+' => return makeToken(.plus),
        '/' => return makeToken(.slash),
        '*' => return makeToken(.star),
        '!' => if (match('='))
            return makeToken(.bang_equal)
        else
            return makeToken(.bang),
        '=' => if (match('='))
            return makeToken(.equal_equal)
        else
            return makeToken(.equal),
        '<' => if (match('='))
            return makeToken(.less_equal)
        else
            return makeToken(.less),
        '>' => if (match('='))
            return makeToken(.greater_equal)
        else
            return makeToken(.greater),
        '"' => return string(),
        else => {},
    }

    return errorToken("Unexpected character.");
}

fn isAtEnd() bool {
    return scanner.current >= scanner.source.len;
}

fn advance() u8 {
    scanner.current += 1;
    return scanner.source[scanner.current - 1];
}

fn peek() u8 {
    if (isAtEnd()) return 0;
    return scanner.source[scanner.current];
}

fn peekNext() u8 {
    if (scanner.current + 1 >= scanner.source.len) return 0;
    return scanner.source[scanner.current + 1];
}

fn match(expected: u8) bool {
    if (isAtEnd()) return false;
    if (scanner.source[scanner.current] != expected) return false;

    scanner.current += 1;
    return true;
}

fn makeToken(kind: Token.Kind) Token {
    return Token{
        .kind = kind,
        .text = scanner.source[scanner.start..scanner.current],
        .line = scanner.line,
    };
}

fn errorToken(message: []const u8) Token {
    return Token{
        .kind = .@"error",
        .text = message,
        .line = scanner.line,
    };
}

fn skipWhitespace() void {
    while (true) {
        const c = peek();
        switch (c) {
            ' ', '\r', '\t' => _ = advance(),
            '\n' => {
                scanner.line += 1;
                _ = advance();
            },
            '/' => if (peekNext() == '/') {
                while (peek() != '\n' and !isAtEnd()) _ = advance();
            } else {
                return;
            },
            else => return,
        }
    }
}

fn string() Token {
    while (peek() != '"' and !isAtEnd()) {
        if (peek() == '\n') scanner.line += 1;
        _ = advance();
    }

    if (isAtEnd()) return errorToken("Unterminated string.");

    _ = advance();
    return makeToken(.string);
}

fn number() Token {
    while (isDigit(peek())) _ = advance();

    if (peek() == '.' and isDigit(peekNext())) {
        _ = advance();
        while (isDigit(peek())) _ = advance();
    }

    return makeToken(.number);
}

fn identifier() Token {
    while (isAlpha(peek()) or isDigit(peek())) _ = advance();
    return makeToken(identifierKind());
}

fn identifierKind() Token.Kind {
    switch (scanner.source[scanner.start]) {
        'a' => return checkKeyword(1, "nd", .@"and"),
        'c' => return checkKeyword(1, "lass", .class),
        'e' => return checkKeyword(1, "lse", .@"else"),
        'f' => {
            if (scanner.current - scanner.start > 1) {
                switch (scanner.source[scanner.start + 1]) {
                    'a' => return checkKeyword(2, "lse", .false),
                    'o' => return checkKeyword(2, "r", .@"for"),
                    'u' => return checkKeyword(2, "n", .fun),
                    else => {},
                }
            }
        },
        'i' => return checkKeyword(1, "f", .@"if"),
        'n' => return checkKeyword(1, "il", .nil),
        'o' => return checkKeyword(1, "r", .@"or"),
        'p' => return checkKeyword(1, "rint", .print),
        'r' => return checkKeyword(1, "eturn", .@"return"),
        's' => return checkKeyword(1, "uper", .super),
        't' => {
            if (scanner.current - scanner.start > 1) {
                switch (scanner.source[scanner.start + 1]) {
                    'h' => return checkKeyword(2, "is", .this),
                    'r' => return checkKeyword(2, "ue", .true),
                    else => {},
                }
            }
        },
        'v' => return checkKeyword(1, "ar", .@"var"),
        'w' => return checkKeyword(1, "hile", .@"while"),
        else => {},
    }
    return .identifier;
}

fn checkKeyword(start: usize, rest: []const u8, kind: Token.Kind) Token.Kind {
    if (scanner.current - scanner.start == start + rest.len and
        std.mem.eql(u8, scanner.source[scanner.start + start .. scanner.current], rest))
    {
        return kind;
    }
    return .identifier;
}
