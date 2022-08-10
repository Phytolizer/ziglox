const std = @import("std");
const chunkMod = @import("chunk.zig");
const Chunk = chunkMod.Chunk;
const debug = @import("debug.zig");

pub fn main() !void {
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    var chunk = Chunk.init(allocator.backing_allocator);
    defer chunk.deinit();

    const constant = try chunk.addConstant(1.2);
    try chunk.writeOp(.op_constant, 123);
    try chunk.write(@intCast(u8, constant), 123);
    try chunk.writeOp(.op_return, 123);

    try debug.disassembleChunk(&chunk, "test chunk");
}

test "chunk doesn't leak" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const constant = try chunk.addConstant(1.2);
    try chunk.writeOp(.op_constant, 123);
    try chunk.write(@intCast(u8, constant), 123);
    try chunk.writeOp(.op_return, 123);
}
