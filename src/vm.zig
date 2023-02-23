const std = @import("std");
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const debug = @import("debug.zig");
const compiler = @import("compiler.zig");
const UINT8_COUNT = compiler.UINT8_COUNT;
const g = @import("global.zig");
const obj_mod = @import("obj.zig");
const Obj = obj_mod.Obj;
const ObjString = obj_mod.ObjString;
const ObjFunction = obj_mod.ObjFunction;
const table_mod = @import("table.zig");
const Table = table_mod.Table;

const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * UINT8_COUNT;

const CallFrame = struct {
    function: *ObjFunction,
    ip: usize,
    slots: []Value,
};

const VM = struct {
    frames: [FRAMES_MAX]CallFrame = undefined,
    frame_count: usize = 0,

    chunk: ?*Chunk = null,
    ip: usize = 0,
    stack: [STACK_MAX]Value = undefined,
    stack_top: usize = 0,
    objects: ?*Obj = null,
    globals: Table = .{},
    strings: Table = .{},
};

pub var vm = VM{};

pub fn init() void {
    resetStack();
}

fn resetStack() void {
    vm.stack_top = 0;
}

pub fn deinit() void {
    vm.globals.deinit();
    vm.strings.deinit();
    freeObjects();
}

fn freeObjects() void {
    var object = vm.objects;
    while (object) |obj| {
        const next = obj.next;
        obj.deinit();
        object = next;
    }
}

fn push(value: Value) void {
    vm.stack[vm.stack_top] = value;
    vm.stack_top += 1;
}

fn pop() Value {
    vm.stack_top -= 1;
    return vm.stack[vm.stack_top];
}

fn peek(distance: usize) Value {
    return vm.stack[vm.stack_top - 1 - distance];
}

fn isFalsey(v: Value) bool {
    return v.isNil() or (v.isBoolean() and !v.boolean);
}

fn runtimeError(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt ++ "\n", args);

    const frame = &vm.frames[vm.frame_count - 1];
    const instruction = frame.ip - 1;
    const line = frame.function.chunk.getLine(instruction);
    std.debug.print("[line {d}] in script\n", .{line});
}

const InterpretError = error{
    Compile,
    Runtime,
} || std.fs.File.WriteError ||
    std.mem.Allocator.Error;

pub fn interpret(source: []const u8) InterpretError!void {
    const function = try compiler.compile(source);

    push(.{ .obj = obj_mod.castObj(function) });
    const frame = &vm.frames[vm.frame_count];
    vm.frame_count += 1;
    frame.function = function;
    frame.ip = 0;
    frame.slots = &vm.stack;

    try run();
}

fn run() !void {
    const frame = &vm.frames[vm.frame_count - 1];
    const Reader = struct {
        frame: *CallFrame,

        fn readByte(self: @This()) u8 {
            const byte = self.frame.function.chunk.code[self.frame.ip];
            self.frame.ip += 1;
            return byte;
        }
        fn readShort(self: @This()) u16 {
            const hi = self.readByte();
            const lo = self.readByte();

            return (@as(u16, hi) << 8) | lo;
        }
        fn read3Bytes(self: @This()) u24 {
            const hi = self.readByte();
            const md = self.readByte();
            const lo = self.readByte();

            return (@as(u24, hi) << 16) | (@as(u24, md) << 8) | lo;
        }
        fn readConstant(self: @This()) Value {
            const constant = self.frame.function.chunk.constants.values[self.readByte()];
            return constant;
        }
        fn readConstantLong(self: @This()) Value {
            const constant = self.read3Bytes();
            return self.frame.function.chunk.constants.values[constant];
        }
        fn readString(self: @This()) *ObjString {
            return self.readConstant().asString();
        }
        fn readStringLong(self: @This()) *ObjString {
            return self.readConstantLong().asString();
        }
    };
    const reader = Reader{ .frame = frame };
    const binaryOp = struct {
        fn f(comptime value_kind: Value.Kind, comptime T: type, comptime op: fn (f64, f64) T) !void {
            if (!peek(0).isNumber() or !peek(1).isNumber()) {
                runtimeError("Operands must be numbers.", .{});
                return error.Runtime;
            }
            const b = pop().number;
            const a = pop().number;
            push(@unionInit(Value, @tagName(value_kind), op(a, b)));
        }
    }.f;
    const lengthOps = struct {
        reader: Reader,

        fn defineGlobal(self: @This(), comptime readFn: fn (Reader) *ObjString) !void {
            const name = readFn(self.reader);
            _ = try vm.globals.set(name, peek(0));
            _ = pop();
        }
        fn getGlobal(self: @This(), comptime readFn: fn (Reader) *ObjString) !void {
            const name = readFn(self.reader);
            if (vm.globals.get(name)) |value| {
                push(value);
            } else {
                runtimeError("Undefined variable '{s}'.", .{name.text});
                return error.Runtime;
            }
        }
        fn setGlobal(self: @This(), comptime readFn: fn (Reader) *ObjString) !void {
            const name = readFn(self.reader);
            if (try vm.globals.set(name, peek(0))) {
                _ = vm.globals.delete(name);
                runtimeError("Undefined variable '{s}'.", .{name.text});
                return error.Runtime;
            }
        }
        fn readNum(self: @This(), comptime long: bool) usize {
            return if (long)
                self.reader.read3Bytes()
            else
                self.reader.readByte();
        }
        fn getLocal(self: @This(), comptime long: bool) !void {
            const slot = self.readNum(long);
            push(self.reader.frame.slots[slot]);
        }
        fn setLocal(self: @This(), comptime long: bool) !void {
            const slot = self.readNum(long);
            self.reader.frame.slots[slot] = peek(0);
        }
    }{ .reader = reader };

    var bw = std.io.bufferedWriter(std.io.getStdOut().writer());
    const bww = bw.writer();

    while (true) {
        if (debug.TRACE_EXECUTION) {
            try bww.writeAll("          ");
            var slot: usize = 0;
            while (slot < vm.stack_top) : (slot += 1) {
                try bww.writeAll("[ ");
                try value_mod.printValue(bww, vm.stack[slot]);
                try bww.writeAll(" ]");
            }
            try bww.writeByte('\n');
            _ = try debug.disassembleInstruction(&frame.function.chunk, frame.ip, bww);
            try bw.flush();
        }
        const instruction = reader.readByte();
        switch (@intToEnum(OpCode, instruction)) {
            .constant => {
                const constant = reader.readConstant();
                push(constant);
            },
            .constant_long => {
                const constant = reader.readConstantLong();
                push(constant);
            },
            .nil => push(.nil),
            .true => push(.{ .boolean = true }),
            .false => push(.{ .boolean = false }),
            .pop => _ = pop(),
            .get_local => try lengthOps.getLocal(false),
            .get_local_long => try lengthOps.getLocal(true),
            .get_global => try lengthOps.getGlobal(Reader.readString),
            .get_global_long => try lengthOps.getGlobal(Reader.readStringLong),
            .define_global => try lengthOps.defineGlobal(Reader.readString),
            .define_global_long => try lengthOps.defineGlobal(Reader.readStringLong),
            .set_local => try lengthOps.setLocal(false),
            .set_local_long => try lengthOps.setLocal(true),
            .set_global => try lengthOps.setGlobal(Reader.readString),
            .set_global_long => try lengthOps.setGlobal(Reader.readStringLong),
            .equal => {
                const b = pop();
                const a = pop();
                push(.{ .boolean = a.equals(b) });
            },
            .greater => try binaryOp(.boolean, bool, struct {
                fn op(a: f64, b: f64) bool {
                    return a > b;
                }
            }.op),
            .less => try binaryOp(.boolean, bool, struct {
                fn op(a: f64, b: f64) bool {
                    return a < b;
                }
            }.op),
            .add => {
                if (peek(0).isString() and peek(1).isString()) {
                    try concatenate();
                } else if (peek(0).isNumber() and peek(1).isNumber()) {
                    const b = pop().number;
                    const a = pop().number;
                    push(.{ .number = a + b });
                } else {
                    runtimeError("Operands must be two numbers or two strings.", .{});
                    return error.Runtime;
                }
            },
            .subtract => try binaryOp(.number, f64, struct {
                fn op(a: f64, b: f64) f64 {
                    return a - b;
                }
            }.op),
            .multiply => try binaryOp(.number, f64, struct {
                fn op(a: f64, b: f64) f64 {
                    return a * b;
                }
            }.op),
            .divide => try binaryOp(.number, f64, struct {
                fn op(a: f64, b: f64) f64 {
                    return a / b;
                }
            }.op),
            .not => push(.{ .boolean = isFalsey(pop()) }),
            .negate => {
                if (!peek(0).isNumber()) {
                    runtimeError("Operand must be a number", .{});
                    return error.Runtime;
                }
                push(.{ .number = -pop().number });
            },
            .print => {
                try value_mod.printValue(bww, pop());
                try bww.writeByte('\n');
            },
            .jump => {
                const offset = reader.readShort();
                frame.ip += offset;
            },
            .jump_if_false => {
                const offset = reader.readShort();
                if (isFalsey(peek(0))) frame.ip += offset;
            },
            .loop => {
                const offset = reader.readShort();
                frame.ip -= offset;
            },
            .@"return" => {
                break;
            },
            _ => {
                @panic("Unknown opcode");
            },
        }
    }

    try bw.flush();
}

fn concatenate() !void {
    const b = pop().asCstring();
    const a = pop().asCstring();

    const chars = try std.mem.concat(g.allocator, u8, &.{ a, b });

    const result = try obj_mod.takeString(chars);
    push(Value.initObj(result));
}
