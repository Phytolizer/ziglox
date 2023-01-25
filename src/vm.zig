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
const u8Count = common.u8Count;

// natives
fn clockNative(args: []Value) Value {
    _ = args;
    return valueMod.numberVal(@intToFloat(f64, std.time.timestamp()));
}
comptime {
    std.debug.assert(@TypeOf(&clockNative) == Obj.NativeFnImpl);
}

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

const CallFrame = struct {
    function: *Obj.Function,
    code: []const u8,
    ip: usize,
    slots: []Value,
    slotOffset: usize,
};

pub const VM = struct {
    const framesMax = 64;
    const stackMax = framesMax * u8Count;

    frames: [framesMax]CallFrame = undefined,
    frameCount: usize = 0,
    stack: [stackMax]Value = undefined,
    stackTop: usize = 0,
    allocator: Allocator,
    objects: ?*Obj = null,
    strings: Table,
    globals: Table,

    const Self = @This();

    pub fn init(allocator: Allocator) !Self {
        var result = Self{
            .allocator = allocator,
            .strings = Table.init(allocator),
            .globals = Table.init(allocator),
        };

        try result.defineNative("clock", clockNative);

        return result;
    }

    fn resetStack(self: *Self) void {
        self.stackTop = 0;
        self.frameCount = 0;
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
        self.globals.deinit();
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
        const function = try compiler.compile(source, self);
        if (function) |f| {
            self.push(valueMod.objVal(&f.obj));
            _ = self.call(f, 0);
        } else {
            return .compile_error;
        }

        return try self.run();
    }

    fn readByte(frame: *CallFrame) u8 {
        const b = frame.code[frame.ip];
        frame.ip += 1;
        return b;
    }

    fn readShort(frame: *CallFrame) u16 {
        const b1 = frame.code[frame.ip];
        const b2 = frame.code[frame.ip + 1];
        frame.ip += 2;
        return (@as(u16, b1) << 8) | @as(u16, b2);
    }

    fn readConstant(frame: *CallFrame) Value {
        return frame.function.chunk.constants.values.?[readByte(frame)];
    }

    fn readString(frame: *CallFrame) *Obj.String {
        return objectMod.asString(readConstant(frame)) catch
        // cannot happen because the compiler will never generate a non-string constant
            unreachable;
    }

    fn binaryOp(
        self: *Self,
        comptime T: type,
        comptime valueType: fn (T) Value,
        comptime op: fn (comptime type, anytype, anytype) T,
    ) !void {
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

    fn runtimeError(self: *Self, comptime format: []const u8, args: anytype) void {
        std.debug.print(format, args);
        std.debug.print("\n", .{});

        var i = self.frameCount;
        while (i > 0) : (i -= 1) {
            const frame = &self.frames[i - 1];
            const f = frame.function;
            const instruction = frame.ip - 1;
            std.debug.print("[line {d}] in ", .{f.chunk.lines.?[instruction]});
            if (f.name) |name| {
                std.debug.print("{s}()\n", .{name.data});
            } else {
                std.debug.print("script\n", .{});
            }
        }
        self.resetStack();
    }

    fn defineNative(self: *Self, name: []const u8, f: Obj.NativeFnImpl) !void {
        self.push(valueMod.objVal(
            try objectMod.copyString(self, name),
        ));
        self.push(valueMod.objVal(
            &(try objectMod.initNative(self, f)).obj,
        ));
        _ = try self.globals.set(
            objectMod.asString(self.peek(1)) catch unreachable,
            self.peek(0),
        );
        _ = self.pop();
        _ = self.pop();
    }

    fn run(self: *Self) !InterpretResult {
        const stdout = std.io.getStdOut().writer();
        var frame = &self.frames[self.frameCount - 1];
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
                _ = try debug.disassembleInstruction(stdout, &frame.function.chunk, frame.ip);
            }
            const instruction = @intToEnum(OpCode, readByte(frame));
            switch (instruction) {
                .op_constant => {
                    const constant = readConstant(frame);
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
                .op_get_local => {
                    const slot = readByte(frame);
                    self.push(frame.slots[slot]);
                },
                .op_set_local => {
                    const slot = readByte(frame);
                    frame.slots[@as(usize, slot)] = self.peek(0);
                },
                .op_get_global => {
                    const name = readString(frame);
                    const value = if (self.globals.get(name)) |v|
                        v
                    else {
                        self.runtimeError("Undefined variable '{s}'.", .{name.data});
                        return .runtime_error;
                    };
                    self.push(value);
                },
                .op_define_global => {
                    const name = readString(frame);
                    _ = try self.globals.set(name, self.peek(0));
                    _ = self.pop();
                },
                .op_set_global => {
                    const name = readString(frame);
                    if (try self.globals.set(name, self.peek(0))) {
                        // did not exist before, so undo what was just done
                        _ = self.globals.delete(name);
                        self.runtimeError("Undefined variable '{s}'.\n", .{name.data});
                        return .runtime_error;
                    }
                },
                .op_equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(valueMod.boolVal(a.equals(b)));
                },
                .op_greater => {
                    self.binaryOp(
                        bool,
                        valueMod.boolVal,
                        struct {
                            fn greater(comptime T: type, a: T, b: T) bool {
                                return a > b;
                            }
                        }.greater,
                    ) catch return .runtime_error;
                },
                .op_less => {
                    self.binaryOp(
                        bool,
                        valueMod.boolVal,
                        struct {
                            fn less(comptime T: type, a: T, b: T) bool {
                                return a < b;
                            }
                        }.less,
                    ) catch return .runtime_error;
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
                .op_jump => {
                    const offset = readShort(frame);
                    frame.ip += offset;
                },
                .op_jump_if_false => {
                    const offset = readShort(frame);
                    if (self.peek(0).isFalsey()) {
                        frame.ip += offset;
                    }
                },
                .op_loop => {
                    const offset = readShort(frame);
                    frame.ip -= offset;
                },
                .op_call => {
                    const argCount = readByte(frame);
                    if (!self.callValue(self.peek(@as(usize, argCount)), argCount)) {
                        return .runtime_error;
                    }
                    frame = &self.frames[self.frameCount - 1];
                },
                .op_return => {
                    const result = self.pop();
                    self.frameCount -= 1;
                    if (self.frameCount == 0) {
                        _ = self.pop();
                        return .ok;
                    }

                    self.stackTop = frame.slotOffset;
                    self.push(result);
                    frame = &self.frames[self.frameCount - 1];
                },
            }
        }
    }

    fn callValue(self: *Self, callee: Value, argCount: u8) bool {
        if (callee.maybeObj()) |obj| {
            switch (obj.kind) {
                .obj_function => {
                    return self.call(obj.asFunction() catch unreachable, argCount);
                },
                .obj_native => {
                    const native = obj.asNative() catch unreachable;
                    const result = native.function(self.stack[self.stackTop - @as(usize, argCount) ..]);
                    self.stackTop -= @as(usize, argCount) + 1;
                    self.push(result);
                    return true;
                },
                else => {},
            }
        }
        // Non-callable object type.
        self.runtimeError("Can only call functions and classes.", .{});
        return false;
    }

    fn call(self: *Self, f: *Obj.Function, argCount: u8) bool {
        if (@as(usize, argCount) != f.arity) {
            self.runtimeError("Expected {d} arguments but got {d}.", .{ f.arity, argCount });
            return false;
        }
        if (self.frameCount == framesMax) {
            self.runtimeError("Stack overflow.", .{});
            return false;
        }
        const frame = &self.frames[self.frameCount];
        self.frameCount += 1;
        frame.function = f;
        frame.ip = 0;
        frame.code = f.chunk.code.?;
        frame.slotOffset = self.stackTop - @as(usize, argCount) - 1;
        frame.slots = self.stack[frame.slotOffset..];
        return true;
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
