const std = @import("std");
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const debug = @import("debug.zig");
const compiler = @import("compiler.zig");

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

fn peek(distance: usize) Value {
    return vm.stack[vm.stack_top - 1 - distance];
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
        fn f(comptime value_kind: Value.Kind, comptime op: fn (f64, f64) f64) !void {
            if (!peek(0).isNumber() or !peek(1).isNumber()) {
                runtimeError("Operands must be numbers.", .{});
                return error.Runtime;
            }
            const b = pop().number;
            const a = pop().number;
            push(@unionInit(Value, @tagName(value_kind), op(a, b)));
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
            .nil => push(.nil),
            .true => push(.{ .boolean = true }),
            .false => push(.{ .boolean = false }),
            .add => try binaryOp(.number, struct {
                fn op(a: f64, b: f64) f64 {
                    return a + b;
                }
            }.op),
            .subtract => try binaryOp(.number, struct {
                fn op(a: f64, b: f64) f64 {
                    return a - b;
                }
            }.op),
            .multiply => try binaryOp(.number, struct {
                fn op(a: f64, b: f64) f64 {
                    return a * b;
                }
            }.op),
            .divide => try binaryOp(.number, struct {
                fn op(a: f64, b: f64) f64 {
                    return a / b;
                }
            }.op),
            .negate => {
                if (!peek(0).isNumber()) {
                    runtimeError("Operand must be a number", .{});
                    return error.Runtime;
                }
                push(.{ .number = -pop().number });
            },
            .@"return" => {
                try value_mod.printValue(bww, pop());
                try bww.writeByte('\n');
                break;
            },
            _ => {
                @panic("Unknown opcode");
            },
        }
    }

    try bw.flush();
}
