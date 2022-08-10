const std = @import("std");
const chunkMod = @import("chunk.zig");
const Chunk = chunkMod.Chunk;
const OpCode = chunkMod.OpCode;
const debug = @import("debug.zig");

pub fn main() !void {
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    var chunk = Chunk.init(allocator.backing_allocator);
    defer chunk.deinit();

    const constant = try chunk.addConstant(1.2);
    try chunk.writeOp(OpCode.op_constant);
    try chunk.write(@intCast(u8, constant));
    try chunk.writeOp(OpCode.op_return);

    try debug.disassembleChunk(&chunk, "test chunk");
}
