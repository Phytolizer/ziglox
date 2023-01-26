const std = @import("std");
const errors = @import("errors.zig");
const object = @import("object.zig");
const Token = @import("token.zig").Token;

a: std.mem.Allocator,
source: []const u8,
tokens: std.ArrayList(Token),
start: usize = 0,
current: usize = 0,
line: usize = 1,

pub fn init(a: std.mem.Allocator, source: []const u8) @This() {
    return .{
        .a = a,
        .source = source,
        .tokens = std.ArrayList(Token).init(a),
    };
}

pub fn deinit(self: @This()) void {
    self.tokens.deinit();
}

pub fn scanTokens(self: *@This()) ![]Token {
    while (!self.isAtEnd()) {
        self.start = self.current;
        try self.scanToken();
    }

    try self.tokens.append(Token.init(
        .eof,
        "",
        try object.Null.init(self.a),
        self.line,
    ));
    return try self.tokens.toOwnedSlice();
}

fn isAtEnd(self: @This()) bool {
    return self.current >= self.source.len;
}

fn scanToken(self: *@This()) !void {
    const c = self.advance();

    switch (c) {
        '(' => try self.addToken(.left_paren, null),
        ')' => try self.addToken(.right_paren, null),
        '{' => try self.addToken(.left_brace, null),
        '}' => try self.addToken(.right_brace, null),
        ',' => try self.addToken(.comma, null),
        '.' => try self.addToken(.dot, null),
        '-' => try self.addToken(.minus, null),
        '+' => try self.addToken(.plus, null),
        ';' => try self.addToken(.semicolon, null),
        '*' => try self.addToken(.star, null),
        '!' => try self.addToken(if (self.match('=')) .bang_equal else .bang, null),
        '=' => try self.addToken(if (self.match('=')) .equal_equal else .equal, null),
        '<' => try self.addToken(if (self.match('=')) .less_equal else .less, null),
        '>' => try self.addToken(if (self.match('=')) .greater_equal else .greater, null),
        '/' => {
            if (self.match('/')) {
                while (self.peek() != '\n' and !self.isAtEnd()) {
                    _ = self.advance();
                }
            } else {
                try self.addToken(.slash, null);
            }
        },
        ' ', '\r', '\t' => {},
        '\n' => self.line += 1,
        '"' => try self.string(),
        else => {
            errors.fail(self.line, "Unexpected character.");
        },
    }
}

fn advance(self: *@This()) u8 {
    const c = self.source[self.current];
    self.current += 1;
    return c;
}

fn addToken(self: *@This(), @"type": Token.Type, literal_in: ?*object.Object) !void {
    const literal = literal_in orelse try object.Null.init(self.a);
    const text = self.source[self.start..self.current];
    try self.tokens.append(Token.init(@"type", text, literal, self.line));
}

fn match(self: *@This(), expected: u8) bool {
    if (self.isAtEnd()) return false;
    if (self.source[self.current] != expected) return false;

    self.current += 1;
    return true;
}

fn peek(self: @This()) u8 {
    if (self.isAtEnd()) return 0;
    return self.source[self.current];
}

fn string(self: *@This()) !void {
    while (self.peek() != '"' and !self.isAtEnd()) {
        if (self.peek() == '\n') self.line += 1;
        _ = self.advance();
    }

    if (self.isAtEnd()) {
        errors.fail(self.line, "Unterminated string.");
        return;
    }

    _ = self.advance();
    const value = self.source[self.start + 1 .. self.current - 1];
    try self.addToken(.string, try object.String.init(self.a, value));
}
