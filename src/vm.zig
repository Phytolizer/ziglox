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
const objectMod = @import("object.zig");
const memoryMod = @import("memory.zig");
const Obj = objectMod.Obj;
const Table = @import("table.zig").Table;

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

pub const VM = struct {
    const stackMax = 256;

    chunk: ?*Chunk = null,
    ip: usize = 0,
    stack: [stackMax]Value = undefined,
    stackTop: usize = 0,
    allocator: Allocator,
    objects: ?*Obj = null,
    strings: Table,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .allocator = allocator,
            .strings = Table.init(allocator),
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
        self.freeObjects();
        self.strings.deinit();
    }

    fn freeObjects(self: *Self) void {
        var obj = self.objects;
        while (obj) |o| {
            const next = o.next;
            o.deinit(self.allocator);
            obj = next;
        }
    }

    pub fn interpret(self: *Self, source: []const u8) !InterpretResult {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        if (!(try compiler.compile(source, &chunk, self))) {
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

    fn binaryOp(self: *Self, comptime T: type, comptime valueType: fn (T) Value, comptime op: fn (comptime T: type, Value, Value) Value) !void {
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
                .op_nil => {
                    self.push(valueMod.nilVal());
                },
                .op_true => {
                    self.push(valueMod.boolVal(true));
                },
                .op_false => {
                    self.push(valueMod.boolVal(false));
                },
                .op_pop => {
                    _ = self.pop();
                },
                .op_equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(valueMod.boolVal(a.equals(b)));
                },
                .op_greater => {
                    self.binaryOp(bool, valueMod.boolVal, math.greater) catch
                        return .runtime_error;
                },
                .op_less => {
                    self.binaryOp(bool, valueMod.boolVal, math.less) catch
                        return .runtime_error;
                },
                .op_add => {
                    if (objectMod.isString(self.peek(0)) and objectMod.isString(self.peek(1))) {
                        try self.concatenate();
                    } else if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
                        self.binaryOp(f64, valueMod.numberVal, math.add) catch unreachable;
                    } else {
                        self.runtimeError("Operands must be two numbers or two strings.", .{});
                        return .runtime_error;
                    }
                },
                .op_subtract => {
                    self.binaryOp(f64, valueMod.numberVal, math.sub) catch
                        return .runtime_error;
                },
                .op_multiply => {
                    self.binaryOp(f64, valueMod.numberVal, math.mul) catch
                        return .runtime_error;
                },
                .op_divide => {
                    self.binaryOp(f64, valueMod.numberVal, math.div) catch
                        return .runtime_error;
                },
                .op_not => {
                    self.push(valueMod.boolVal(self.pop().isFalsey()));
                },
                .op_print => {
                    try valueMod.printValue(stdout, self.pop());
                    try stdout.writeAll("\n");
                },
                .op_return => {
                    return .ok;
                },
            }
        }
    }

    fn concatenate(self: *Self) !void {
        const b = objectMod.asString(self.pop()) catch unreachable;
        const a = objectMod.asString(self.pop()) catch unreachable;

        const length = a.data.len + b.data.len;
        const chars = try self.allocator.alloc(u8, length);
        std.mem.copy(u8, chars, a.data);
        std.mem.copy(u8, chars[a.data.len..], b.data);
        const result = try objectMod.takeString(self, chars);
        self.push(valueMod.objVal(&result.obj));
    }
};
