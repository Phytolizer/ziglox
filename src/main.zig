const std = @import("std");
const g = @import("global.zig");
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const debug = @import("debug.zig");
const vm = @import("vm.zig");

pub fn main() void {
    // Hack to allow `defer` to work properly.
    // std.process.exit does NOT run deferred code, so this is needed.
    run() catch std.process.exit(1);
}

fn run() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.detectLeaks();
    g.allocator = gpa.allocator();

    vm.init();
    defer vm.deinit();

    var chunk = Chunk.init();
    defer chunk.deinit();
    try chunk.writeConstant(1.2, 123);
    try chunk.writeConstant(3.4, 123);
    try chunk.writeOp(.add, 123);
    try chunk.writeConstant(5.6, 123);
    try chunk.writeOp(.divide, 123);
    try chunk.writeOp(.negate, 123);
    try chunk.writeOp(.@"return", 123);

    try debug.disassembleChunk(&chunk, "test chunk");
    try vm.interpret(&chunk);
}
