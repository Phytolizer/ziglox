const std = @import("std");
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const debug = @import("debug.zig");
const compiler = @import("compiler.zig");
const g = @import("global.zig");
const obj_mod = @import("obj.zig");
const Obj = obj_mod.Obj;
const ObjString = obj_mod.ObjString;
const table_mod = @import("table.zig");
const Table = table_mod.Table;

const STACK_MAX = 256;

const VM = struct {
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
    std.debug.print(fmt, args);
    const instruction = vm.ip - 1;
    const line = vm.chunk.?.getLine(instruction);
    std.debug.print("\n[line {d}] in script\n", .{line});
}

const InterpretError = error{
    Compile,
    Runtime,
} || std.fs.File.WriteError ||
    std.mem.Allocator.Error;

pub fn interpret(source: []const u8) InterpretError!void {
    var chunk = Chunk.init();
    defer chunk.deinit();

    try compiler.compile(source, &chunk);

    vm.chunk = &chunk;
    vm.ip = 0;

    try run();
}

fn run() !void {
    const Reader = struct {
        fn readByte() u8 {
            const byte = vm.chunk.?.code[vm.ip];
            vm.ip += 1;
            return byte;
        }
        fn readShort() u16 {
            const hi = readByte();
            const lo = readByte();

            return (@as(u16, hi) << 8) | lo;
        }
        fn read3Bytes() u24 {
            const hi = readByte();
            const md = readByte();
            const lo = readByte();

            return (@as(u24, hi) << 16) | (@as(u24, md) << 8) | lo;
        }
        fn readConstant() Value {
            const constant = vm.chunk.?.constants.values[readByte()];
            return constant;
        }
        fn readConstantLong() Value {
            const constant = read3Bytes();
            return vm.chunk.?.constants.values[constant];
        }
        fn readString() *ObjString {
            return readConstant().asString();
        }
        fn readStringLong() *ObjString {
            return readConstantLong().asString();
        }
    };
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
    const LengthOps = struct {
        fn defineGlobal(comptime readFn: fn () *ObjString) !void {
            const name = readFn();
            _ = try vm.globals.set(name, peek(0));
            _ = pop();
        }
        fn getGlobal(comptime readFn: fn () *ObjString) !void {
            const name = readFn();
            if (vm.globals.get(name)) |value| {
                push(value);
            } else {
                runtimeError("Undefined variable '{s}'.", .{name.text});
                return error.Runtime;
            }
        }
        fn setGlobal(comptime readFn: fn () *ObjString) !void {
            const name = readFn();
            if (try vm.globals.set(name, peek(0))) {
                _ = vm.globals.delete(name);
                runtimeError("Undefined variable '{s}'.", .{name.text});
                return error.Runtime;
            }
        }
        fn readNum(comptime long: bool) usize {
            return if (long)
                Reader.read3Bytes()
            else
                Reader.readByte();
        }
        fn getLocal(comptime long: bool) !void {
            const slot = readNum(long);
            push(vm.stack[slot]);
        }
        fn setLocal(comptime long: bool) !void {
            const slot = readNum(long);
            vm.stack[slot] = peek(0);
        }
    };

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
            _ = try debug.disassembleInstruction(vm.chunk.?, vm.ip, bww);
        }
        const instruction = Reader.readByte();
        switch (@intToEnum(OpCode, instruction)) {
            .constant => {
                const constant = Reader.readConstant();
                push(constant);
            },
            .constant_long => {
                const constant = Reader.readConstantLong();
                push(constant);
            },
            .nil => push(.nil),
            .true => push(.{ .boolean = true }),
            .false => push(.{ .boolean = false }),
            .pop => _ = pop(),
            .get_local => try LengthOps.getLocal(false),
            .get_local_long => try LengthOps.getLocal(true),
            .get_global => try LengthOps.getGlobal(Reader.readString),
            .get_global_long => try LengthOps.getGlobal(Reader.readStringLong),
            .define_global => try LengthOps.defineGlobal(Reader.readString),
            .define_global_long => try LengthOps.defineGlobal(Reader.readStringLong),
            .set_local => try LengthOps.setLocal(false),
            .set_local_long => try LengthOps.setLocal(true),
            .set_global => try LengthOps.setGlobal(Reader.readString),
            .set_global_long => try LengthOps.setGlobal(Reader.readStringLong),
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
                break;
            },
            .jump => {
                const offset = Reader.readShort();
                vm.ip += offset;
            },
            .jump_if_false => {
                const offset = Reader.readShort();
                if (isFalsey(peek(0))) vm.ip += offset;
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
