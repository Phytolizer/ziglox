const std = @import("std");
const chunkMod = @import("chunk.zig");
const Chunk = chunkMod.Chunk;
const OpCode = chunkMod.OpCode;
const valueMod = @import("value.zig");
const Writer = std.fs.File.Writer;

pub fn disassembleChunk(writer: Writer, chunk: *Chunk, name: []const u8) !void {
    try writer.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.count) {
        offset = try disassembleInstruction(writer, chunk, offset);
    }
}

pub fn disassembleInstruction(writer: Writer, chunk: *Chunk, offset: usize) !usize {
    try writer.print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines.?[offset] == chunk.lines.?[offset - 1]) {
        try writer.writeAll("   | ");
    } else {
        try writer.print("{d:>4} ", .{chunk.lines.?[offset]});
    }
    const instruction = @intToEnum(OpCode, chunk.code.?[offset]);
    switch (instruction) {
        .op_constant => return constantInstruction(writer, "OP_CONSTANT", chunk, offset),
        .op_negate => return simpleInstruction(writer, "OP_NEGATE", offset),
        .op_nil => return simpleInstruction(writer, "OP_NIL", offset),
        .op_true => return simpleInstruction(writer, "OP_TRUE", offset),
        .op_false => return simpleInstruction(writer, "OP_FALSE", offset),
        .op_equal => return simpleInstruction(writer, "OP_EQUAL", offset),
        .op_less => return simpleInstruction(writer, "OP_LESS", offset),
        .op_greater => return simpleInstruction(writer, "OP_GREATER", offset),
        .op_add => return simpleInstruction(writer, "OP_ADD", offset),
        .op_subtract => return simpleInstruction(writer, "OP_SUBTRACT", offset),
        .op_multiply => return simpleInstruction(writer, "OP_MULTIPLY", offset),
        .op_divide => return simpleInstruction(writer, "OP_DIVIDE", offset),
        .op_not => return simpleInstruction(writer, "OP_NOT", offset),
        .op_return => return simpleInstruction(writer, "OP_RETURN", offset),
    }
}

fn simpleInstruction(writer: Writer, comptime name: []const u8, offset: usize) !usize {
    try writer.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(writer: Writer, comptime name: []const u8, chunk: *Chunk, offset: usize) !usize {
    const constant = chunk.code.?[offset + 1];
    try writer.print("{s:<16} {d:4} '", .{ name, constant });
    try valueMod.printValue(writer, chunk.constants.values.?[constant]);
    try writer.writeAll("'\n");
    return offset + 2;
}
