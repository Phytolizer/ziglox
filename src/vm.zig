const chunkMod = @import("chunk.zig");
const Chunk = chunkMod.Chunk;
const OpCode = chunkMod.OpCode;
const compiler = @import("compiler.zig");
const valueMod = @import("value.zig");
const Value = valueMod.Value;
const debug = @import("debug.zig");
const common = @import("common.zig");
const math = @import("math.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

pub const VM = struct {
    const stackMax = 256;

    chunk: ?*Chunk,
    ip: usize,
    stack: [stackMax]Value,
    stackTop: usize,

    const Self = @This();

    pub fn init() Self {
        return Self{
            .chunk = null,
            .ip = 0,
            .stack = undefined,
            .stackTop = 0,
        };
    }

    fn resetStack(self: *Self) void {
        self.stackTop = 0;
    }

    fn push(self: *Self, value: Value) void {
        self.stack[self.stackTop] = value;
        self.stackTop += 1;
    }

    fn pop(self: *Self) Value {
        self.stackTop -= 1;
        return self.stack[self.stackTop];
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn interpret(self: *Self, allocator: Allocator, source: []const u8) !InterpretResult {
        var chunk = Chunk.init(allocator);
        defer chunk.deinit();

        if (!(try compiler.compile(source, &chunk))) {
            return .compile_error;
        }

        self.chunk = &chunk;
        self.ip = 0;

        const result = try self.run();
        return result;
    }

    fn readByte(self: *Self) u8 {
        const b = self.chunk.?.code.?[self.ip];
        self.ip += 1;
        return b;
    }

    fn readConstant(self: *Self) Value {
        return self.chunk.?.constants.values.?[self.readByte()];
    }

    fn binaryOp(self: *Self, comptime valueType: fn (f64) Value, comptime op: fn (comptime T: type, Value, Value) Value) !void {
        if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
            self.runtimeError("Operands must be numbers.", .{});
            return error.RuntimeError;
        }
        const b = self.pop().asNumber() catch unreachable;
        const a = self.pop().asNumber() catch unreachable;
        self.push(valueType(op(f64, a, b)));
    }

    fn peek(self: *Self, distance: usize) Value {
        return self.stack[self.stackTop - distance - 1];
    }

    fn runtimeError(self: *Self, comptime format: []const u8, comptime args: anytype) void {
        std.debug.print(format, args);
        std.debug.print("\n", .{});

        const instruction = self.ip;
        const line = self.chunk.?.lines.?[instruction];
        std.debug.print("[line {d}] in script\n", .{line});
        self.resetStack();
    }

    fn run(self: *Self) !InterpretResult {
        const stdout = std.io.getStdOut().writer();
        while (true) {
            if (common.debugTraceExecution) {
                try stdout.writeAll("          ");
                var slot: usize = 0;
                while (slot < self.stackTop) : (slot += 1) {
                    try stdout.writeAll("[ ");
                    try valueMod.printValue(stdout, self.stack[slot]);
                    try stdout.writeAll(" ]");
                }
                try stdout.writeAll("\n");
                _ = try debug.disassembleInstruction(stdout, self.chunk.?, self.ip);
            }
            const instruction = @intToEnum(OpCode, self.readByte());
            switch (instruction) {
                .op_constant => {
                    const constant = self.readConstant();
                    self.push(constant);
                },
                .op_negate => {
                    if (!self.peek(0).isNumber()) {
                        self.runtimeError("Operand must be a number.", .{});
                        return .runtime_error;
                    }
                    self.push(valueMod.numberVal(-(try self.pop().asNumber())));
                },
                .op_add => {
                    self.binaryOp(valueMod.numberVal, math.add) catch
                        return .runtime_error;
                },
                .op_subtract => {
                    self.binaryOp(valueMod.numberVal, math.sub) catch
                        return .runtime_error;
                },
                .op_multiply => {
                    self.binaryOp(valueMod.numberVal, math.mul) catch
                        return .runtime_error;
                },
                .op_divide => {
                    self.binaryOp(valueMod.numberVal, math.div) catch
                        return .runtime_error;
                },
                .op_return => {
                    try valueMod.printValue(stdout, self.pop());
                    try stdout.writeAll("\n");
                    return .ok;
                },
            }
        }
    }
};
