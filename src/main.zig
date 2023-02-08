const std = @import("std");
const g = @import("global.zig");
const chunk = @import("chunk.zig");
const Chunk = chunk.Chunk;
const debug = @import("debug.zig");

pub fn main() void {
    // Hack to allow `defer` to work properly.
    // std.process.exit does NOT run deferred code, so this is needed.
    run() catch std.process.exit(1);
}

fn run() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.detectLeaks();
    g.allocator = gpa.allocator();

    var c = Chunk.init();
    defer c.deinit();
    const constant = try c.addConstant(1.2);
    try c.writeOp(.constant, 123);
    try c.write(@intCast(u8, constant), 123);
    try c.writeOp(.@"return", 123);

    try debug.disassembleChunk(&c, "test chunk");
}
