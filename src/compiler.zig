const std = @import("std");
const VM = @import("vm.zig").VM;
const scannerMod = @import("scanner.zig");
const Scanner = scannerMod.Scanner;
const Token = scannerMod.Token;
const TokenKind = scannerMod.TokenKind;
const chunkMod = @import("chunk.zig");
const Chunk = chunkMod.Chunk;
const OpCode = chunkMod.OpCode;
const valueMod = @import("value.zig");
const Value = valueMod.Value;
const common = @import("common.zig");
const debug = @import("debug.zig");
const objectMod = @import("object.zig");
const Allocator = std.mem.Allocator;

const Precedence = enum(u8) {
    prec_none,
    prec_assignment,
    prec_or,
    prec_and,
    prec_equality,
    prec_comparison,
    prec_term,
    prec_factor,
    prec_unary,
    prec_call,
    prec_primary,
};

const ParseError = std.mem.Allocator.Error || std.fmt.ParseFloatError;

const ParseFn = ?fn (parser: *Parser, canAssign: bool) ParseError!void;

const ParseRule = struct {
    prefix: ParseFn = null,
    infix: ParseFn = null,
    precedence: Precedence = .prec_none,
};

fn getRule(kind: TokenKind) ParseRule {
    return switch (kind) {
        .tk_left_paren => .{
            .prefix = Parser.grouping,
            .precedence = .prec_none,
        },
        .tk_minus => .{
            .prefix = Parser.unary,
            .infix = Parser.binary,
            .precedence = .prec_term,
        },
        .tk_plus => .{
            .infix = Parser.binary,
            .precedence = .prec_term,
        },
        .tk_slash => .{
            .infix = Parser.binary,
            .precedence = .prec_factor,
        },
        .tk_star => .{
            .infix = Parser.binary,
            .precedence = .prec_factor,
        },
        .tk_number => .{
            .prefix = Parser.number,
        },
        .tk_false => .{
            .prefix = Parser.literal,
        },
        .tk_true => .{
            .prefix = Parser.literal,
        },
        .tk_nil => .{
            .prefix = Parser.literal,
        },
        .tk_bang => .{
            .prefix = Parser.unary,
        },
        .tk_bang_equal => .{
            .infix = Parser.binary,
            .precedence = .prec_equality,
        },
        .tk_equal_equal => .{
            .infix = Parser.binary,
            .precedence = .prec_equality,
        },
        .tk_greater => .{
            .infix = Parser.binary,
            .precedence = .prec_comparison,
        },
        .tk_greater_equal => .{
            .infix = Parser.binary,
            .precedence = .prec_comparison,
        },
        .tk_less => .{
            .infix = Parser.binary,
            .precedence = .prec_comparison,
        },
        .tk_less_equal => .{
            .infix = Parser.binary,
            .precedence = .prec_comparison,
        },
        .tk_string => .{
            .prefix = Parser.string,
        },
        .tk_identifier => .{
            .prefix = Parser.variable,
        },
        else => .{},
    };
}

const Parser = struct {
    scanner: *Scanner,
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,
    compiler: ?*Compiler,
    allocator: Allocator,

    const Self = @This();

    pub fn init(scanner: *Scanner, allocator: Allocator) Self {
        return Self{
            .scanner = scanner,
            .current = undefined,
            .previous = undefined,
            .hadError = false,
            .panicMode = false,
            .compiler = null,
            .allocator = allocator,
        };
    }

    pub fn advance(self: *Self) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.kind != .tk_error) {
                break;
            }

            self.errorAtCurrent(self.current.text);
        }
    }

    fn errorAtCurrent(self: *Self, message: []const u8) void {
        self.errorAt(&self.current, message);
    }

    pub fn emitError(self: *Self, message: []const u8) void {
        self.errorAt(&self.previous, message);
    }

    fn errorAt(self: *Self, token: *Token, message: []const u8) void {
        if (self.panicMode) {
            return;
        }
        self.panicMode = true;
        std.debug.print("[line {d}] Error", .{token.line});

        if (token.kind == .tk_eof) {
            std.debug.print(" at end", .{});
        } else if (token.kind == .tk_error) {
            // Nothing.
        } else {
            std.debug.print(" at '{s}'", .{token.text});
        }

        std.debug.print(": {s}\n", .{message});
        self.hadError = true;
    }

    pub fn consume(self: *Self, kind: TokenKind, comptime message: []const u8) void {
        if (self.current.kind == kind) {
            self.advance();
            return;
        }
        self.errorAtCurrent(message);
    }

    pub fn expression(self: *Self) ParseError!void {
        try self.parsePrecedence(.prec_assignment);
    }

    fn number(self: *Self, canAssign: bool) ParseError!void {
        _ = canAssign;
        const value = try std.fmt.parseFloat(f64, self.previous.text);
        try self.compiler.?.emitConstant(valueMod.numberVal(value));
    }

    fn grouping(self: *Self, canAssign: bool) ParseError!void {
        _ = canAssign;
        try self.expression();
        self.consume(.tk_right_paren, "Expect ')' after expression.");
    }

    fn literal(self: *Self, canAssign: bool) ParseError!void {
        _ = canAssign;
        switch (self.previous.kind) {
            .tk_false => try self.compiler.?.emitOp(.op_false),
            .tk_true => try self.compiler.?.emitOp(.op_true),
            .tk_nil => try self.compiler.?.emitOp(.op_nil),
            else => unreachable,
        }
    }

    fn string(self: *Self, canAssign: bool) ParseError!void {
        _ = canAssign;
        try self.compiler.?.emitConstant(valueMod.objVal(
            try objectMod.copyString(self.compiler.?.vm, self.previous.text[1 .. self.previous.text.len - 1]),
        ));
    }

    fn variable(self: *Self, canAssign: bool) ParseError!void {
        try self.namedVariable(&self.previous, canAssign);
    }

    fn namedVariable(self: *Self, name: *Token, canAssign: bool) !void {
        const maybeArg = self.compiler.?.resolveLocal(name);
        const ops: struct {
            arg: u8,
            get: OpCode,
            set: OpCode,
        } = if (maybeArg) |arg| .{
            .arg = arg,
            .get = .op_get_local,
            .set = .op_set_local,
        } else .{
            .arg = try self.compiler.?.identifierConstant(name),
            .get = .op_get_global,
            .set = .op_set_global,
        };

        if (canAssign and self.match(.tk_equal)) {
            try self.expression();
            try self.compiler.?.emitOp(ops.set);
        } else {
            try self.compiler.?.emitOp(ops.get);
        }
        try self.compiler.?.emitByte(ops.arg);
    }

    fn unary(self: *Self, canAssign: bool) ParseError!void {
        _ = canAssign;
        const operatorKind = self.previous.kind;

        try self.parsePrecedence(.prec_unary);

        switch (operatorKind) {
            .tk_minus => try self.compiler.?.emitOp(.op_negate),
            .tk_bang => try self.compiler.?.emitOp(.op_not),
            else => unreachable,
        }
    }

    fn binary(self: *Self, canAssign: bool) ParseError!void {
        _ = canAssign;
        const operatorKind = self.previous.kind;
        const rule = getRule(operatorKind);
        try self.parsePrecedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

        switch (operatorKind) {
            .tk_bang_equal => try self.compiler.?.emitOps(.op_equal, .op_not),
            .tk_equal_equal => try self.compiler.?.emitOp(.op_equal),
            .tk_greater => try self.compiler.?.emitOp(.op_greater),
            .tk_greater_equal => try self.compiler.?.emitOps(.op_less, .op_not),
            .tk_less => try self.compiler.?.emitOp(.op_less),
            .tk_less_equal => try self.compiler.?.emitOps(.op_greater, .op_not),
            .tk_plus => try self.compiler.?.emitOp(.op_add),
            .tk_minus => try self.compiler.?.emitOp(.op_subtract),
            .tk_star => try self.compiler.?.emitOp(.op_multiply),
            .tk_slash => try self.compiler.?.emitOp(.op_divide),
            else => unreachable,
        }
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) ParseError!void {
        self.advance();
        const prefixRule = getRule(self.previous.kind).prefix;
        if (prefixRule == null) {
            self.emitError("Expect expression.");
            return;
        }

        const canAssign = @enumToInt(precedence) <= @enumToInt(Precedence.prec_assignment);
        try prefixRule.?(self, canAssign);

        while (@enumToInt(precedence) <= @enumToInt(getRule(self.current.kind).precedence)) {
            self.advance();
            const infixRule = getRule(self.previous.kind).infix;
            try infixRule.?(self, canAssign);
        }

        if (canAssign and match(self, .tk_equal)) {
            self.emitError("Invalid assignment target.");
        }
    }

    pub fn declaration(self: *Self) ParseError!void {
        if (self.match(.tk_var)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panicMode) {
            self.synchronize();
        }
    }

    fn parseVariable(self: *Self, comptime errorMessage: []const u8) !u8 {
        self.consume(.tk_identifier, errorMessage);

        self.compiler.?.declareVariable();
        if (self.compiler.?.scopeDepth > 0) {
            return 0;
        }

        return try self.compiler.?.identifierConstant(&self.previous);
    }

    fn varDeclaration(self: *Self) !void {
        const global = try self.parseVariable("Expect variable name.");

        if (self.match(.tk_equal)) {
            try self.expression();
        } else {
            try self.compiler.?.emitOp(.op_nil);
        }
        self.consume(.tk_semicolon, "Expect ';' after variable declaration.");

        try self.compiler.?.defineVariable(global);
    }

    fn synchronize(self: *Self) void {
        self.panicMode = false;
        while (self.current.kind != .tk_eof) {
            if (self.previous.kind == .tk_semicolon) {
                return;
            }

            switch (self.current.kind) {
                .tk_class,
                .tk_fun,
                .tk_var,
                .tk_for,
                .tk_if,
                .tk_while,
                .tk_print,
                .tk_return,
                => return,
                else => {},
            }
            self.advance();
        }
    }

    fn statement(self: *Self) ParseError!void {
        if (self.match(.tk_print)) {
            try self.printStatement();
        } else if (self.match(.tk_if)) {
            try self.ifStatement();
        } else if (self.match(.tk_left_brace)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    fn ifStatement(self: *Self) ParseError!void {
        self.consume(.tk_left_paren, "Expect '(' after 'if'.");
        try self.expression();
        self.consume(.tk_right_paren, "Expect ')' after condition.");

        const thenJump = try self.compiler.?.emitJump(.op_jump_if_false);
        try self.statement();

        self.compiler.?.patchJump(thenJump);
    }

    fn block(self: *Self) ParseError!void {
        while (!self.check(.tk_right_brace) and !self.check(.tk_eof)) {
            try self.declaration();
        }

        self.consume(.tk_right_brace, "Expect '}' after block.");
    }

    fn beginScope(self: *Self) void {
        self.compiler.?.scopeDepth += 1;
    }

    fn endScope(self: *Self) !void {
        self.compiler.?.scopeDepth -= 1;

        while (self.compiler.?.localCount > 0) : (self.compiler.?.localCount -= 1) {
            if (self.compiler.?.locals[self.compiler.?.localCount - 1].depth) |depth| {
                if (depth <= self.compiler.?.scopeDepth) {
                    break;
                }
            } else break;

            try self.compiler.?.emitOp(.op_pop);
        }
    }

    fn expressionStatement(self: *Self) !void {
        try self.expression();
        self.consume(.tk_semicolon, "Expect ';' after expression.");
        try self.compiler.?.emitOp(.op_pop);
    }

    pub fn match(self: *Self, kind: TokenKind) bool {
        if (!self.check(kind)) {
            return false;
        }

        self.advance();
        return true;
    }

    fn check(self: *Self, kind: TokenKind) bool {
        return self.current.kind == kind;
    }

    fn printStatement(self: *Self) !void {
        try self.expression();
        self.consume(.tk_semicolon, "Expect ';' after value.");
        try self.compiler.?.emitOp(.op_print);
    }
};

const u8Count = std.math.maxInt(u8) + 1;

const Local = struct {
    name: Token,
    depth: ?usize,
};

const Compiler = struct {
    compilingChunk: *Chunk,
    parser: *Parser,
    vm: *VM,
    locals: [u8Count]Local = undefined,
    localCount: usize = 0,
    scopeDepth: usize = 0,

    const Self = @This();

    pub fn init(compilingChunk: *Chunk, parser: *Parser, vm: *VM) Self {
        return Self{
            .compilingChunk = compilingChunk,
            .parser = parser,
            .vm = vm,
        };
    }

    pub fn emitByte(self: *Self, byte: u8) !void {
        try self.compilingChunk.write(byte, self.parser.previous.line);
    }

    pub fn emitOp(self: *Self, op: OpCode) !void {
        try self.emitByte(@enumToInt(op));
    }

    pub fn currentChunk(self: *Self) *Chunk {
        return self.compilingChunk;
    }

    fn emitReturn(self: *Self) !void {
        try self.emitOp(.op_return);
    }

    pub fn identifierConstant(self: *Self, name: *Token) !u8 {
        return try self.makeConstant(valueMod.objVal(
            try objectMod.copyString(self.vm, name.text),
        ));
    }

    pub fn end(self: *Self) !void {
        try self.emitReturn();
        if (common.debugPrintCode) {
            try debug.disassembleChunk(
                std.io.getStdOut().writer(),
                self.currentChunk(),
                "code",
            );
        }
    }

    pub fn emitBytes(self: *Self, b1: u8, b2: u8) !void {
        try self.emitByte(b1);
        try self.emitByte(b2);
    }

    pub fn emitOps(self: *Self, op1: OpCode, op2: OpCode) !void {
        try self.emitBytes(@enumToInt(op1), @enumToInt(op2));
    }

    pub fn emitConstant(self: *Self, value: Value) !void {
        try self.emitBytes(
            @enumToInt(OpCode.op_constant),
            try self.makeConstant(value),
        );
    }

    fn makeConstant(self: *Self, value: Value) !u8 {
        const constant = try self.currentChunk().addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            self.parser.emitError("Too many constants in one chunk.");
            return 0;
        }

        return @intCast(u8, constant);
    }

    pub fn defineVariable(self: *Self, global: u8) !void {
        if (self.scopeDepth > 0) {
            self.markInitialized();
            return;
        }

        try self.emitOp(.op_define_global);
        try self.emitByte(global);
    }

    fn markInitialized(self: *Self) void {
        self.locals[self.localCount - 1].depth = self.scopeDepth;
    }

    fn identifiersEqual(a: *Token, b: *Token) bool {
        return std.mem.eql(u8, a.text, b.text);
    }

    pub fn declareVariable(self: *Self) void {
        if (self.scopeDepth == 0) {
            return;
        }

        const name = &self.parser.previous;
        var i = self.localCount;
        while (i > 0) : (i -= 1) {
            const local = &self.locals[i - 1];
            if (local.depth) |depth| {
                if (depth < self.scopeDepth) {
                    break;
                }
            }

            if (identifiersEqual(name, &local.name)) {
                self.parser.emitError("Already a variable with this name in this scope.");
            }
        }
        self.addLocal(name);
    }

    fn addLocal(self: *Self, name: *Token) void {
        if (self.localCount == u8Count) {
            self.parser.emitError("Too many local variables in function.");
            return;
        }
        const local = &self.locals[self.localCount];
        self.localCount += 1;
        local.name = name.*;
        local.depth = null;
    }

    pub fn resolveLocal(self: *Self, name: *Token) ?u8 {
        var i = self.localCount;
        while (i > 0) : (i -= 1) {
            const local = &self.locals[i - 1];

            if (identifiersEqual(name, &local.name)) {
                if (local.depth == null) {
                    self.parser.emitError(
                        "Can't read local variable in its own initializer.",
                    );
                }
                return @truncate(u8, i - 1);
            }
        }
        return null;
    }

    pub fn emitJump(self: *Self, instruction: OpCode) !usize {
        try self.emitOp(instruction);
        try self.emitBytes(0xff, 0xff);
        return self.currentChunk().count - 2;
    }

    pub fn patchJump(self: *Self, offset: usize) void {
        const jump = self.currentChunk().count - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            self.parser.emitError("Too much code to jump over.");
        }

        self.currentChunk().code.?[offset] = @truncate(u8, jump >> 8);
        self.currentChunk().code.?[offset + 1] = @truncate(u8, jump);
    }
};

pub fn compile(source: []const u8, chunk: *Chunk, vm: *VM) !bool {
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner, chunk.allocator);
    var compiler = Compiler.init(chunk, &parser, vm);
    parser.compiler = &compiler;
    parser.advance();
    while (!parser.match(.tk_eof)) {
        try parser.declaration();
    }
    try compiler.end();
    return !parser.hadError;
}
