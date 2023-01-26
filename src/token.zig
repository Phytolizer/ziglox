const std = @import("std");
const object = @import("object.zig");

pub const Token = struct {
    type: Type,
    lexeme: []const u8,
    literal: *object.Object,
    line: usize,

    pub const Type = enum {
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
        bang,
        bang_equal,
        equal,
        equal_equal,
        greater,
        greater_equal,
        less,
        less_equal,
        identifier,
        string,
        number,
        @"and",
        class,
        @"else",
        false,
        fun,
        @"for",
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
        eof,

        pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            var tn_upper: [64]u8 = undefined;
            for (@tagName(self)) |ch, i| {
                tn_upper[i] = std.ascii.toUpper(ch);
            }
            try writer.print("{s}", .{tn_upper[0..@tagName(self).len]});
        }
    };

    pub fn init(@"type": Type, lexeme: []const u8, literal: *object.Object, line: usize) @This() {
        return .{ .type = @"type", .lexeme = lexeme, .literal = literal, .line = line };
    }

    pub fn deinit(self: @This()) void {
        self.literal.deinit();
    }

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{} {s} {}", .{ self.type, self.lexeme, self.literal });
    }
};
