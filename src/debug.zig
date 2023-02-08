const std = @import("std");
const chunk = @import("chunk.zig");
const Chunk = chunk.Chunk;
const value = @import("value.zig");

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
