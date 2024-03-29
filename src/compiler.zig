const std = @import("std");
const scanner = @import("scanner.zig");
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const Token = scanner.Token;
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const ObjFunction = value_mod.ObjFunction;
const debug = @import("debug.zig");
const memory = @import("memory.zig");

pub fn compile(source: []const u8) !*ObjFunction {
    scanner.init(source);
    parser = .{};
    var compiler = Compiler{};
    try initCompiler(&compiler, .script);
    advance();

    while (!match(.eof)) {
        try declaration();
    }

    const function = try endCompiler();
    if (parser.had_error)
        return error.Compile;
    return function;
}

pub fn markRoots() void {
    var compiler = current;
    while (compiler) |c| : (compiler = c.enclosing) {
        memory.markObject(value_mod.castObj(c.function));
    }
}

const Parser = struct {
    current: Token = undefined,
    previous: Token = undefined,
    had_error: bool = false,
    panic_mode: bool = false,
};

var parser = Parser{};
var current: ?*Compiler = null;

fn currentChunk() *Chunk {
    return &current.?.function.chunk;
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
    try emitByte(@intFromEnum(op));
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

fn emitLoop(loop_start: usize) !void {
    try emitOp(.loop);

    const offset = currentChunk().code.len - loop_start + 2;
    if (offset > std.math.maxInt(u16)) {
        errorAtPrevious("Loop body too large.");
    }

    try emitBytes(&.{
        @as(u8, @truncate(offset >> 8)),
        @as(u8, @truncate(offset)),
    });
}

fn emitJump(instruction: OpCode) !usize {
    try emitOp(instruction);
    try emitBytes(&.{ 0xff, 0xff });
    return currentChunk().code.len - 2;
}

fn emitReturn() !void {
    try emitOps(&.{ .nil, .@"return" });
}

fn emitConstant(value: Value) !void {
    try currentChunk().writeConstant(value, parser.previous.line);
}

fn patchJump(offset: usize) void {
    const jump = currentChunk().code.len - offset - 2;
    if (jump > std.math.maxInt(u16)) {
        errorAtPrevious("Too much code to jump over.");
    }

    currentChunk().code[offset] = @as(u8, @truncate(jump >> 8));
    currentChunk().code[offset + 1] = @as(u8, @truncate(jump));
}

fn initCompiler(compiler: *Compiler, @"type": FunctionType) !void {
    compiler.enclosing = current;
    compiler.type = @"type";
    compiler.function = try value_mod.newFunction();
    current = compiler;
    if (@"type" != .script) {
        current.?.function.name = try value_mod.copyString(parser.previous.text);
    }

    const local = &current.?.locals[current.?.local_count];
    current.?.local_count += 1;
    local.depth = 0;
    local.is_captured = false;
    local.name.text = "";
}

fn endCompiler() ParseError!*ObjFunction {
    try emitReturn();
    const function = current.?.function;
    if (debug.PRINT_CODE and !parser.had_error) {
        try debug.disassembleChunk(
            currentChunk(),
            if (function.name) |name| name.text else "<script>",
        );
    }

    current = current.?.enclosing;
    return function;
}

fn beginScope() void {
    current.?.scope_depth += 1;
}

fn endScope() !void {
    current.?.scope_depth -= 1;

    while (current.?.local_count > 0 and
        (current.?.locals[current.?.local_count - 1].depth orelse 0) >
        current.?.scope_depth)
    {
        if (current.?.locals[current.?.local_count - 1].is_captured) {
            try emitOp(.close_upvalue);
        } else {
            try emitOp(.pop);
        }
        current.?.local_count -= 1;
    }
}

fn expression() ParseError!void {
    try parsePrecedence(.assignment);
}

fn block() ParseError!void {
    while (!check(.right_brace) and !check(.eof)) {
        try declaration();
    }

    consume(.right_brace, "Expect '}' after block.");
}

fn parseFunction(@"type": FunctionType) ParseError!void {
    var compiler = Compiler{};
    try initCompiler(&compiler, @"type");
    beginScope();

    consume(.left_paren, "Expect '(' after function name.");
    if (!check(.right_paren)) {
        while (true) {
            current.?.function.arity += 1;
            if (current.?.function.arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            const constant = try parseVariable("Expect parameter name.");
            try defineVariable(constant);

            if (!match(.comma)) break;
        }
    }
    consume(.right_paren, "Expect ')' after parameters.");
    consume(.left_brace, "Expect '{' before function body.");
    try block();

    const function = try endCompiler();
    const constant = try currentChunk().addConstant(Value.initObj(function));
    try emitDynamic(.closure, .closure_long, constant);

    for (0..function.upvalue_count) |i| {
        try emitBytes(&.{
            @intFromBool(compiler.upvalues[i].is_local),
            compiler.upvalues[i].index,
        });
    }
}

fn funDeclaration() ParseError!void {
    const global = try parseVariable("Expect function name.");
    markInitialized();
    try parseFunction(.function);
    try defineVariable(global);
}

fn identifierConstant(name: Token) !usize {
    return try currentChunk().addConstant(
        Value.initObj(try value_mod.copyString(name.text)),
    );
}

fn identifiersEqual(a: Token, b: Token) bool {
    return std.mem.eql(u8, a.text, b.text);
}

fn resolveLocal(compiler: *Compiler, name: Token) ?usize {
    var i = compiler.local_count;
    while (i > 0) : (i -= 1) {
        const local = &compiler.locals[i - 1];
        if (identifiersEqual(name, local.name)) {
            if (local.depth == null) {
                errorAtPrevious("Can't read local variable in its own initializer.");
            }
            return i - 1;
        }
    }
    return null;
}

fn addUpvalue(compiler: *Compiler, index: u8, is_local: bool) u8 {
    const upvalue_count = compiler.function.upvalue_count;

    for (0..upvalue_count) |i| {
        const upvalue = &compiler.upvalues[i];
        if (upvalue.index == index and upvalue.is_local == is_local) {
            return @as(u8, @intCast(i));
        }
    }

    if (upvalue_count == UINT8_COUNT) {
        errorAtPrevious("Too many closure variables in function.");
        return 0;
    }

    compiler.upvalues[upvalue_count].is_local = is_local;
    compiler.upvalues[upvalue_count].index = index;
    compiler.function.upvalue_count += 1;
    return @as(u8, @intCast(upvalue_count));
}

fn resolveUpvalue(compiler: *Compiler, name: Token) ?u8 {
    if (compiler.enclosing) |enclosing| {
        if (resolveLocal(enclosing, name)) |local| {
            enclosing.locals[local].is_captured = true;
            return addUpvalue(compiler, @as(u8, @intCast(local)), true);
        }

        if (resolveUpvalue(enclosing, name)) |upvalue| {
            return addUpvalue(compiler, upvalue, false);
        }
    }

    return null;
}

fn addLocal(name: Token) void {
    if (current.?.local_count == UINT8_COUNT) {
        errorAtPrevious("Too many local variables in function.");
        return;
    }

    const local = &current.?.locals[current.?.local_count];
    current.?.local_count += 1;
    local.name = name;
    local.depth = null;
    local.is_captured = false;
}

fn declareVariable() void {
    if (current.?.scope_depth == 0) return;

    const name = &parser.previous;
    var i = current.?.local_count;
    while (i > 0) : (i -= 1) {
        const local = &current.?.locals[i - 1];
        if (local.depth) |depth| if (depth < current.?.scope_depth) {
            break;
        };

        if (identifiersEqual(name.*, local.name)) {
            errorAtPrevious("Already a variable with this name in this scope.");
        }
    }
    addLocal(name.*);
}

fn parseVariable(error_message: []const u8) !usize {
    consume(.identifier, error_message);

    declareVariable();
    if (current.?.scope_depth > 0) return 0;

    return try identifierConstant(parser.previous);
}

fn markInitialized() void {
    if (current.?.scope_depth == 0) return;

    current.?.locals[current.?.local_count - 1].depth = current.?.scope_depth;
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
    if (current.?.scope_depth > 0) {
        markInitialized();
        return;
    }

    try emitDynamic(.define_global, .define_global_long, global);
}

fn argumentList() !u8 {
    var arg_count: u8 = 0;
    if (!check(.right_paren)) {
        while (true) {
            try expression();
            if (arg_count == 255) {
                errorAtPrevious("Can't have more than 255 arguments.");
            }
            arg_count += 1;

            if (!match(.comma)) break;
        }
    }
    consume(.right_paren, "Expect ')' after arguments.");
    return arg_count;
}

fn @"and"(_: bool) ParseError!void {
    const end_jump = try emitJump(.jump_if_false);

    try emitOp(.pop);
    try parsePrecedence(.@"and");

    patchJump(end_jump);
}

fn number(_: bool) ParseError!void {
    const value = std.fmt.parseFloat(f64, parser.previous.text) catch unreachable;
    try emitConstant(.{ .number = value });
}

fn @"or"(_: bool) ParseError!void {
    const else_jump = try emitJump(.jump_if_false);
    const end_jump = try emitJump(.jump);

    patchJump(else_jump);
    try emitOp(.pop);

    try parsePrecedence(.@"or");
    patchJump(end_jump);
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
    try parsePrecedence(@as(Precedence, @enumFromInt(@intFromEnum(rule.precedence) + 1)));

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

fn call(_: bool) ParseError!void {
    const arg_count = try argumentList();
    try emitOp(.call);
    try emitByte(arg_count);
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

pub const UINT8_COUNT = std.math.maxInt(u8) + 1;

const Local = struct {
    name: Token,
    depth: ?usize,
    is_captured: bool,
};

const FunctionType = enum {
    function,
    script,
};

const Upvalue = struct {
    index: u8,
    is_local: bool,
};

const Compiler = struct {
    enclosing: ?*@This() = null,
    function: *ObjFunction = undefined,
    type: FunctionType = .script,

    locals: [UINT8_COUNT]Local = undefined,
    local_count: usize = 0,
    upvalues: [UINT8_COUNT]Upvalue = undefined,
    scope_depth: usize = 0,
};

const ParseError = std.mem.Allocator.Error || std.fs.File.WriteError;

const ParseFn = *const fn (can_assign: bool) ParseError!void;

const rules = std.EnumArray(Token.Kind, ParseRule).init(.{
    .left_paren = .{ .prefix = grouping, .infix = call, .precedence = .call },
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
    .@"and" = .{ .infix = @"and" },
    .class = .{},
    .@"else" = .{},
    .false = .{ .prefix = literal },
    .@"for" = .{},
    .fun = .{},
    .@"if" = .{},
    .nil = .{ .prefix = literal },
    .@"or" = .{ .infix = @"or" },
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

    const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.assignment);
    try prefixRule(can_assign);

    while (@intFromEnum(precedence) <= @intFromEnum(getRule(parser.current.kind).precedence)) {
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
    try emitConstant(Value.initObj(try value_mod.copyString(
        parser.previous.text[1 .. parser.previous.text.len - 1],
    )));
}

fn emitDynamic(short_op: OpCode, long_op: OpCode, value: usize) !void {
    if (value <= std.math.maxInt(u8)) {
        try emitOp(short_op);
        try emitByte(@intCast(value));
    } else if (value <= std.math.maxInt(u24)) {
        try emitOp(long_op);
        try emitBytes(&.{
            @truncate(value >> 16),
            @truncate(value >> 8),
            @truncate(value),
        });
    } else unreachable;
}

fn namedVariable(name: Token, can_assign: bool) ParseError!void {
    const Ops = struct {
        get: OpCode,
        set: OpCode,
        get_long: OpCode,
        set_long: OpCode,
    };
    var ops = Ops{
        .get = .get_local,
        .set = .set_local,
        .get_long = .get_local_long,
        .set_long = .set_local_long,
    };
    const arg = resolveLocal(current.?, name) orelse blk: {
        if (resolveUpvalue(current.?, name)) |upvalue| {
            ops.get = .get_upvalue;
            ops.set = .set_upvalue;
            break :blk upvalue;
        } else {
            ops.get = .get_global;
            ops.set = .set_global;
            ops.get_long = .get_global_long;
            ops.set_long = .set_global_long;
            break :blk try identifierConstant(name);
        }
    };

    if (can_assign and match(.equal)) {
        try expression();
        try emitDynamic(ops.set, ops.set_long, arg);
    } else {
        try emitDynamic(ops.get, ops.get_long, arg);
    }
}

fn variable(can_assign: bool) ParseError!void {
    try namedVariable(parser.previous, can_assign);
}

fn declaration() ParseError!void {
    if (match(.fun)) {
        try funDeclaration();
    } else if (match(.@"var")) {
        try varDeclaration();
    } else {
        try statement();
    }

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

fn statement() ParseError!void {
    if (match(.print)) {
        try printStatement();
    } else if (match(.@"for")) {
        try forStatement();
    } else if (match(.@"if")) {
        try ifStatement();
    } else if (match(.@"return")) {
        try returnStatement();
    } else if (match(.@"while")) {
        try whileStatement();
    } else if (match(.left_brace)) {
        beginScope();
        try block();
        try endScope();
    } else {
        try expressionStatement();
    }
}

fn printStatement() !void {
    try expression();
    consume(.semicolon, "Expect ';' after value.");
    try emitOp(.print);
}

fn returnStatement() !void {
    if (current.?.type == .script) {
        errorAtPrevious("Can't return from top-level code.");
    }
    if (match(.semicolon)) {
        try emitReturn();
    } else {
        try expression();
        consume(.semicolon, "Expect ';' after return value.");
        try emitOp(.@"return");
    }
}

fn whileStatement() !void {
    const loop_start = currentChunk().code.len;
    consume(.left_paren, "Expect '(' after 'while'.");
    try expression();
    consume(.right_paren, "Expect ')' after condition.");

    const exit_jump = try emitJump(.jump_if_false);
    try emitOp(.pop);
    try statement();
    try emitLoop(loop_start);

    patchJump(exit_jump);
    try emitOp(.pop);
}

fn expressionStatement() !void {
    try expression();
    consume(.semicolon, "Expect ';' after expression.");
    try emitOp(.pop);
}

fn forStatement() !void {
    beginScope();
    consume(.left_paren, "Expect '(' after 'for'.");
    if (match(.semicolon)) {
        // no initializer
    } else if (match(.@"var")) {
        try varDeclaration();
    } else {
        try expressionStatement();
    }

    var loop_start = currentChunk().code.len;
    var exit_jump: ?usize = null;
    if (!match(.semicolon)) {
        try expression();
        consume(.semicolon, "Expect ';' after loop condition.");

        exit_jump = try emitJump(.jump_if_false);
        try emitOp(.pop);
    }

    if (!match(.right_paren)) {
        const body_jump = try emitJump(.jump);
        const increment_start = currentChunk().code.len;
        try expression();
        try emitOp(.pop);
        consume(.right_paren, "Expect ')' after for clauses.");

        try emitLoop(loop_start);
        loop_start = increment_start;
        patchJump(body_jump);
    }

    try statement();
    try emitLoop(loop_start);
    if (exit_jump) |ej| {
        patchJump(ej);
        try emitOp(.pop);
    }
    try endScope();
}

fn ifStatement() !void {
    consume(.left_paren, "Expect '(' after 'if'.");
    try expression();
    consume(.right_paren, "Expect ')' after condition.");

    const then_jump = try emitJump(.jump_if_false);
    try emitOp(.pop);
    try statement();

    const else_jump = try emitJump(.jump);

    patchJump(then_jump);
    try emitOp(.pop);

    if (match(.@"else")) try statement();
    patchJump(else_jump);
}

fn match(kind: Token.Kind) bool {
    if (!check(kind)) return false;
    advance();
    return true;
}

fn check(kind: Token.Kind) bool {
    return parser.current.kind == kind;
}
