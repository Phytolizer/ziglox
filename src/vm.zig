const std = @import("std");
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const debug = @import("debug.zig");

const STACK_MAX = 256;

const VM = struct {
    chunk: ?*Chunk = null,
    ip: usize = 0,
    stack: [STACK_MAX]Value = undefined,
    stack_top: usize = 0,
};

var vm = VM{};

pub fn init() void {
    resetStack();
}

fn resetStack() void {
    vm.stack_top = 0;
}

pub fn deinit() void {}

fn push(value: Value) void {
    vm.stack[vm.stack_top] = value;
    vm.stack_top += 1;
}

fn pop() Value {
    vm.stack_top -= 1;
    return vm.stack[vm.stack_top];
}

pub fn interpret(chunk: *Chunk) !void {
    vm.chunk = chunk;
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
        fn readConstant() Value {
            const constant = vm.chunk.?.constants.values[readByte()];
            return constant;
        }
        fn readConstantLong() Value {
            const hi = readByte();
            const md = readByte();
            const lo = readByte();

            const constant: u24 = (@as(u24, hi) << 16) | (@as(u24, md) << 8) | lo;
            return vm.chunk.?.constants.values[constant];
        }
    };
    const binaryOp = struct {
        fn f(comptime op: fn (Value, Value) Value) void {
            const b = pop();
            const a = pop();
            push(op(a, b));
        }
    }.f;

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
            .add => binaryOp(struct {
                fn op(a: Value, b: Value) Value {
                    return a + b;
                }
            }.op),
            .subtract => binaryOp(struct {
                fn op(a: Value, b: Value) Value {
                    return a - b;
                }
            }.op),
            .multiply => binaryOp(struct {
                fn op(a: Value, b: Value) Value {
                    return a * b;
                }
            }.op),
            .divide => binaryOp(struct {
                fn op(a: Value, b: Value) Value {
                    return a / b;
                }
            }.op),
            .negate => {
                push(-pop());
            },
            .@"return" => {
                try value_mod.printValue(bww, pop());
                break;
            },
            _ => {
                @panic("Unknown opcode");
            },
        }
    }

    try bw.flush();
}
