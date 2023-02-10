const std = @import("std");
const chunk = @import("chunk.zig");
const Chunk = chunk.Chunk;
const value = @import("value.zig");

pub const TRACE_EXECUTION = true;
pub const PRINT_CODE = true;

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
    switch (@intToEnum(chunk.OpCode, instruction)) {
        .constant => return try constantInstruction("OP_CONSTANT", c, offset, writer),
        .constant_long => return try constantLongInstruction("OP_CONSTANT_LONG", c, offset, writer),
        .nil => return try simpleInstruction("OP_NIL", offset, writer),
        .true => return try simpleInstruction("OP_TRUE", offset, writer),
        .false => return try simpleInstruction("OP_FALSE", offset, writer),
        .add => return try simpleInstruction("OP_ADD", offset, writer),
        .subtract => return try simpleInstruction("OP_SUBTRACT", offset, writer),
        .multiply => return try simpleInstruction("OP_MULTIPLY", offset, writer),
        .divide => return try simpleInstruction("OP_DIVIDE", offset, writer),
        .not => return try simpleInstruction("OP_NOT", offset, writer),
        .negate => return try simpleInstruction("OP_NEGATE", offset, writer),
        .@"return" => return try simpleInstruction("OP_RETURN", offset, writer),
        _ => {
            try writer.print("Unknown opcode {d}\n", .{instruction});
            return offset + 1;
        },
    }
}

fn simpleInstruction(comptime name: []const u8, offset: usize, writer: anytype) !usize {
    try writer.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(comptime name: []const u8, c: *const Chunk, offset: usize, writer: anytype) !usize {
    const constant = c.code[offset + 1];
    try writer.print("{s:<16} {d:>4} '", .{ name, constant });
    try value.printValue(writer, c.constants.values[constant]);
    try writer.writeAll("'\n");
    return offset + 2;
}

fn constantLongInstruction(comptime name: []const u8, c: *const Chunk, offset: usize, writer: anytype) !usize {
    const hi = c.code[offset + 1];
    const md = c.code[offset + 2];
    const lo = c.code[offset + 3];
    const constant: u24 = (@as(u24, hi) << 16) | (@as(u24, md) << 8) | lo;
    try writer.print("{s:<16} {d:>4} '", .{ name, constant });
    try value.printValue(writer, c.constants.values[constant]);
    try writer.writeAll("'\n");
    return offset + 4;
}
