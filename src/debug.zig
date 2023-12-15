const std = @import("std");
const chunk = @import("chunk.zig");
const Chunk = chunk.Chunk;
const value = @import("value.zig");

pub const TRACE_EXECUTION = false;
pub const STRESS_GC = true;
pub const LOG_GC = true;
pub const PRINT_CODE = false;

pub fn disassembleChunk(c: *const Chunk, name: []const u8) !void {
    var buf = std.io.bufferedWriter(std.io.getStdOut().writer());
    const bw = buf.writer();

    try bw.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < c.code.len) {
        offset = try disassembleInstruction(c, offset, bw);
    }
    try buf.flush();
}

pub fn disassembleInstruction(c: *const Chunk, offset: usize, writer: anytype) !usize {
    try writer.print("{d:0>4} ", .{offset});
    if (offset > 0 and c.getLine(offset) == c.getLine(offset - 1)) {
        try writer.writeAll("   | ");
    } else {
        try writer.print("{d:>4} ", .{c.getLine(offset)});
    }

    const instruction = c.code[offset];
    switch (@as(chunk.OpCode, @enumFromInt(instruction))) {
        .constant,
        .get_global,
        .define_global,
        .set_global,
        => return try constantInstruction(instruction, c, offset, writer),
        .constant_long,
        .get_global_long,
        .define_global_long,
        .set_global_long,
        => return try constantLongInstruction(instruction, c, offset, writer),
        .closure => {
            var ofs = offset;
            ofs += 1;
            const constant = c.code[ofs];
            ofs += 1;
            try writer.print("{s:<16} {d:>4} ", .{ chunk.OpCode.names[instruction], constant });
            try value.printValue(writer, c.constants.values[constant]);
            try writer.writeAll("\n");

            const function = c.constants.values[constant].asFunction();
            for (0..function.upvalue_count) |_| {
                const is_local = c.code[ofs];
                ofs += 1;
                const index = c.code[ofs];
                ofs += 1;
                try writer.print(
                    "{d:0>4}      |                     {s} {d}\n",
                    .{ ofs - 2, if (is_local) "local" else "upvalue", index },
                );
            }

            return ofs;
        },
        .get_local,
        .set_local,
        .call,
        .get_upvalue,
        .set_upvalue,
        => return try numInstruction(instruction, c, offset, writer),
        .get_local_long,
        .set_local_long,
        => return try numLongInstruction(instruction, c, offset, writer),
        .jump,
        .jump_if_false,
        => return try jumpInstruction(instruction, 1, c, offset, writer),
        .loop => return try jumpInstruction(instruction, -1, c, offset, writer),
        .nil,
        .true,
        .false,
        .pop,
        .equal,
        .greater,
        .less,
        .add,
        .subtract,
        .multiply,
        .divide,
        .not,
        .negate,
        .print,
        .close_upvalue,
        .@"return",
        => return try simpleInstruction(instruction, offset, writer),
        _ => {
            try writer.print("Unknown opcode {d}\n", .{instruction});
            return offset + 1;
        },
    }
}

fn simpleInstruction(kind: usize, offset: usize, writer: anytype) !usize {
    try writer.print("{s}\n", .{chunk.OpCode.names[kind]});
    return offset + 1;
}

fn constantInstruction(kind: usize, c: *const Chunk, offset: usize, writer: anytype) !usize {
    const constant = c.code[offset + 1];
    try writer.print("{s:<16} {d:>4} '", .{ chunk.OpCode.names[kind], constant });
    try value.printValue(writer, c.constants.values[constant]);
    try writer.writeAll("'\n");
    return offset + 2;
}

fn constantLongInstruction(kind: usize, c: *const Chunk, offset: usize, writer: anytype) !usize {
    const hi = c.code[offset + 1];
    const md = c.code[offset + 2];
    const lo = c.code[offset + 3];
    const constant: u24 = (@as(u24, hi) << 16) | (@as(u24, md) << 8) | lo;
    try writer.print("{s:<16} {d:>4} '", .{ chunk.OpCode.names[kind], constant });
    try value.printValue(writer, c.constants.values[constant]);
    try writer.writeAll("'\n");
    return offset + 4;
}

fn numInstruction(kind: usize, c: *const Chunk, offset: usize, writer: anytype) !usize {
    const slot = c.code[offset + 1];
    try writer.print("{s:<16} {d:>4}\n", .{ chunk.OpCode.names[kind], slot });
    return offset + 2;
}

fn numLongInstruction(kind: usize, c: *const Chunk, offset: usize, writer: anytype) !usize {
    const hi = c.code[offset + 1];
    const md = c.code[offset + 2];
    const lo = c.code[offset + 3];
    const num: u24 = (@as(u24, hi) << 16) | (@as(u24, md) << 8) | lo;
    try writer.print("{s:<16} {d:>4}\n", .{ chunk.OpCode.names[kind], num });
    return offset + 4;
}

fn jumpInstruction(
    kind: usize,
    comptime sign: i2,
    c: *const Chunk,
    offset: usize,
    writer: anytype,
) !usize {
    const jump = (@as(u16, c.code[offset + 1]) << 8) | c.code[offset + 2];
    const jump_target = if (sign == 1)
        offset + 3 + jump
    else
        offset + 3 - jump;
    try writer.print("{s:<16} {d:4} -> {d}\n", .{
        chunk.OpCode.names[kind],
        offset,
        jump_target,
    });
    return offset + 3;
}
