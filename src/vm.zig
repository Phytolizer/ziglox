const std = @import("std");
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const OpCode = chunk_mod.OpCode;
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const debug = @import("debug.zig");

const VM = struct {
    chunk: ?*Chunk = null,
    ip: usize = 0,
};

var vm = VM{};

pub fn init() void {}

pub fn deinit() void {}

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

    var bw = std.io.bufferedWriter(std.io.getStdOut().writer());
    const bww = bw.writer();

    while (true) {
        if (debug.TRACE_EXECUTION) {
            _ = try debug.disassembleInstruction(vm.chunk.?, vm.ip, bww);
        }
        const instruction = Reader.readByte();
        switch (@intToEnum(OpCode, instruction)) {
            .constant => {
                const constant = Reader.readConstant();
                try value_mod.printValue(bww, constant);
                try bww.writeByte('\n');
            },
            .constant_long => {
                const constant = Reader.readConstantLong();
                try value_mod.printValue(bww, constant);
                try bww.writeByte('\n');
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
