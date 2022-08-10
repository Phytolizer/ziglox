const std = @import("std");
const chunkMod = @import("chunk.zig");
const Chunk = chunkMod.Chunk;
const OpCode = chunkMod.OpCode;
const debug = @import("debug.zig");

pub fn main() !void {
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    var chunk = Chunk.init(allocator.backing_allocator);
    defer chunk.deinit();
    try chunk.writeOp(OpCode.op_return);

    try debug.disassembleChunk(&chunk, "test chunk");
}
