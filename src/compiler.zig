const std = @import("std");
const scanner = @import("scanner.zig");
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const Token = scanner.Token;
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const debug = @import("debug.zig");
const obj = @import("obj.zig");

pub fn compile(source: []const u8, chunk: *Chunk) !void {
    scanner.init(source);
    parser = .{};
    compiling_chunk = chunk;
    advance();

    while (!match(.eof)) {
        try declaration();
    }

    try endCompiler();
    if (parser.had_error)
        return error.Compile;
}

const Parser = struct {
    current: Token = undefined,
    previous: Token = undefined,
    had_error: bool = false,
    panic_mode: bool = false,
};

var parser = Parser{};
var compiling_chunk: *Chunk = undefined;

fn currentChunk() *Chunk {
    return compiling_chunk;
}

fn advance() void {
    parser.previous = parser.current;

    while (true) {
        parser.current = scanner.scanToken();
        if (parser.current.kind != .@"error") break;

        errorAtCurrent(parser.current.text);
    }
}

fn errorAtCurrent(message: []const u8) void {
    errorAt(&parser.current, message);
}

fn errorAtPrevious(message: []const u8) void {
    errorAt(&parser.previous, message);
}

fn errorAt(token: *Token, message: []const u8) void {
    if (parser.panic_mode) return;
    parser.panic_mode = true;
    std.debug.print("[line {d}] Error", .{token.line});

    if (token.kind == .eof) {
        std.debug.print(" at end", .{});
    } else if (token.kind == .@"error") {
        // Nothing.
    } else {
        std.debug.print(" at '{s}'", .{token.text});
    }

    std.debug.print(": {s}\n", .{message});
    parser.had_error = true;
}

fn consume(kind: Token.Kind, message: []const u8) void {
    if (parser.current.kind == kind) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

fn emitByte(byte: u8) !void {
    try currentChunk().write(byte, parser.previous.line);
}

fn emitOp(op: OpCode) !void {
    try emitByte(@enumToInt(op));
}

fn emitBytes(bytes: []const u8) !void {
    for (bytes) |byte| {
        try emitByte(byte);
    }
}

fn emitOps(ops: []const OpCode) !void {
    for (ops) |op| {
        try emitOp(op);
    }
}

fn emitReturn() !void {
    try emitOp(.@"return");
}

fn emitConstant(value: Value) !void {
    try currentChunk().writeConstant(value, parser.previous.line);
}

fn endCompiler() !void {
    try emitReturn();
    if (debug.PRINT_CODE and !parser.had_error) {
        try debug.disassembleChunk(currentChunk(), "code");
    }
}

fn expression() ParseError!void {
    try parsePrecedence(.assignment);
}

fn identifierConstant(name: Token) !usize {
    return try currentChunk().addConstant(.{
        .obj = obj.castObj(try obj.copyString(name.text)),
    });
}

fn parseVariable(error_message: []const u8) !usize {
    consume(.identifier, error_message);
    return identifierConstant(parser.previous);
}

fn varDeclaration() !void {
    const global = try parseVariable("Expect variable name.");

    if (match(.equal))
        try expression()
    else
        try emitOp(.nil);
    consume(.semicolon, "Expect ';' after variable declaration.");

    try defineVariable(global);
}

fn defineVariable(global: usize) !void {
    try emitDynamic(.define_global, .define_global_long, global);
}

fn number(_: bool) ParseError!void {
    const value = std.fmt.parseFloat(f64, parser.previous.text) catch unreachable;
    try emitConstant(.{ .number = value });
}

fn grouping(_: bool) ParseError!void {
    try expression();
    consume(.right_paren, "Expect ')' after expression.");
}

fn unary(_: bool) ParseError!void {
    const operator_kind = parser.previous.kind;

    try parsePrecedence(.unary);

    switch (operator_kind) {
        .bang => try emitOp(.not),
        .minus => try emitOp(.negate),
        else => unreachable,
    }
}

fn binary(_: bool) ParseError!void {
    const operator_kind = parser.previous.kind;

    const rule = getRule(operator_kind);
    try parsePrecedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

    switch (operator_kind) {
        .bang_equal => try emitOps(&.{ .equal, .not }),
        .equal_equal => try emitOp(.equal),
        .greater => try emitOp(.greater),
        .greater_equal => try emitOps(&.{ .less, .not }),
        .less => try emitOp(.less),
        .less_equal => try emitOps(&.{ .greater, .not }),
        .plus => try emitOp(.add),
        .minus => try emitOp(.subtract),
        .star => try emitOp(.multiply),
        .slash => try emitOp(.divide),
        else => unreachable,
    }
}

const Precedence = enum(usize) {
    none,
    assignment,
    @"or",
    @"and",
    equality,
    comparison,
    term,
    factor,
    unary,
    call,
    primary,
};

const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = .none,
};

const ParseError = std.mem.Allocator.Error;

const ParseFn = *const fn (can_assign: bool) ParseError!void;

const rules = std.EnumArray(Token.Kind, ParseRule).init(.{
    .left_paren = .{ .prefix = grouping },
    .right_paren = .{},
    .left_brace = .{},
    .right_brace = .{},
    .comma = .{},
    .dot = .{},
    .minus = .{ .prefix = unary, .infix = binary, .precedence = .term },
    .plus = .{ .infix = binary, .precedence = .term },
    .semicolon = .{},
    .slash = .{ .infix = binary, .precedence = .factor },
    .star = .{ .infix = binary, .precedence = .factor },
    .bang = .{ .prefix = unary },
    .bang_equal = .{ .infix = binary, .precedence = .equality },
    .equal = .{},
    .equal_equal = .{ .infix = binary, .precedence = .equality },
    .greater = .{ .infix = binary, .precedence = .comparison },
    .greater_equal = .{ .infix = binary, .precedence = .comparison },
    .less = .{ .infix = binary, .precedence = .comparison },
    .less_equal = .{ .infix = binary, .precedence = .comparison },
    .identifier = .{ .prefix = variable },
    .string = .{ .prefix = string },
    .number = .{ .prefix = number },
    .@"and" = .{},
    .class = .{},
    .@"else" = .{},
    .false = .{ .prefix = literal },
    .@"for" = .{},
    .fun = .{},
    .@"if" = .{},
    .nil = .{ .prefix = literal },
    .@"or" = .{},
    .print = .{},
    .@"return" = .{},
    .super = .{},
    .this = .{},
    .true = .{ .prefix = literal },
    .@"var" = .{},
    .@"while" = .{},
    .@"error" = .{},
    .eof = .{},
});

fn getRule(kind: Token.Kind) *const ParseRule {
    return &rules.get(kind);
}

fn parsePrecedence(precedence: Precedence) ParseError!void {
    advance();
    const prefixRule = getRule(parser.previous.kind).prefix orelse {
        errorAtPrevious("Expect expression.");
        return;
    };

    const can_assign = @enumToInt(precedence) <= @enumToInt(Precedence.assignment);
    try prefixRule(can_assign);

    while (@enumToInt(precedence) <= @enumToInt(getRule(parser.current.kind).precedence)) {
        advance();
        const infixRule = getRule(parser.previous.kind).infix orelse unreachable;
        try infixRule(can_assign);
    }

    if (can_assign and match(.equal)) {
        errorAtPrevious("Invalid assignment target.");
    }
}

fn literal(_: bool) ParseError!void {
    switch (parser.previous.kind) {
        .nil => try emitOp(.nil),
        .true => try emitOp(.true),
        .false => try emitOp(.false),
        else => unreachable,
    }
}

fn string(_: bool) ParseError!void {
    try emitConstant(Value.initObj(try obj.copyString(
        parser.previous.text[1 .. parser.previous.text.len - 1],
    )));
}

fn emitDynamic(short_op: OpCode, long_op: OpCode, value: usize) !void {
    if (value <= std.math.maxInt(u8)) {
        try emitOp(short_op);
        try emitByte(@intCast(u8, value));
    } else if (value <= std.math.maxInt(u24)) {
        try emitOp(long_op);
        try emitBytes(&.{
            @truncate(u8, value >> 16),
            @truncate(u8, value >> 8),
            @truncate(u8, value),
        });
    } else unreachable;
}

fn namedVariable(name: Token, can_assign: bool) ParseError!void {
    const arg = try identifierConstant(name);
    if (can_assign and match(.equal)) {
        try expression();
        try emitDynamic(.set_global, .set_global_long, arg);
    } else {
        try emitDynamic(.get_global, .get_global_long, arg);
    }
}

fn variable(can_assign: bool) ParseError!void {
    try namedVariable(parser.previous, can_assign);
}

fn declaration() !void {
    if (match(.@"var"))
        try varDeclaration()
    else
        try statement();

    if (parser.panic_mode) synchronize();
}

fn synchronize() void {
    parser.panic_mode = false;

    while (parser.current.kind != .eof) {
        if (parser.previous.kind == .semicolon) return;

        switch (parser.current.kind) {
            .class,
            .fun,
            .@"var",
            .@"for",
            .@"if",
            .@"while",
            .print,
            .@"return",
            => return,
            else => {
                // Do nothing.
            },
        }

        advance();
    }
}

fn statement() !void {
    if (match(.print))
        try printStatement()
    else
        try expressionStatement();
}

fn printStatement() !void {
    try expression();
    consume(.semicolon, "Expect ';' after value.");
    try emitOp(.print);
}

fn expressionStatement() !void {
    try expression();
    consume(.semicolon, "Expect ';' after expression.");
    try emitOp(.pop);
}

fn match(kind: Token.Kind) bool {
    if (!check(kind)) return false;
    advance();
    return true;
}

fn check(kind: Token.Kind) bool {
    return parser.current.kind == kind;
}
