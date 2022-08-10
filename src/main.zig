const std = @import("std");
const chunkMod = @import("chunk.zig");
const Chunk = chunkMod.Chunk;
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;
const Allocator = std.mem.Allocator;

const stdout = debug.stdout;
const gAllocator = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const args = try std.process.argsAlloc(gAllocator.backing_allocator);

    switch (args.len) {
        1 => try repl(),
        2 => try run(args[1]),
        else => {
            std.debug.print("Usage: clox [path]\n", .{});
            return error.InvalidUsage;
        },
    }
}

fn repl() !void {
    var line: [1024]u8 = undefined;
    const stdin = std.io.getStdIn().reader();

    var vm = VM.init();
    defer vm.deinit();

    while (true) {
        try stdout.writeAll("> ");
        const nread = try stdin.read(&line);
        if (nread == 0) {
            try stdout.writeAll("\n");
            break;
        }

        // try vm.interpret(&line);
    }
}

fn run(path: []const u8) !void {
    const source = try readFile(gAllocator.backing_allocator, path);
    defer gAllocator.backing_allocator.free(source);
    var vm = VM.init();
    defer vm.deinit();
    const result = // try vm.interpret(source);
        @import("vm.zig").InterpretResult.ok;

    switch (result) {
        .compile_error => return error.CompileError,
        .runtime_error => return error.RuntimeError,
        .ok => {},
    }
}

fn readFile(allocator: Allocator, path: []const u8) ![]u8 {
    var curDir = try std.process.getCwdAlloc(allocator);
    defer allocator.free(curDir);
    var absPath = if (std.fs.path.isAbsolute(path))
        try allocator.dupe(u8, path)
    else
        try std.fs.path.join(allocator, &.{ curDir, path });
    defer allocator.free(absPath);

    var file = std.fs.openFileAbsolute(absPath, .{}) catch {
        std.debug.print("Could not open file \"{s}\"\n", .{absPath});
        return error.FileNotFound;
    };
    defer file.close();

    const metadata = try file.metadata();
    const size = metadata.size();

    var buffer = try allocator.alloc(u8, @intCast(usize, size));
    const nread = try file.readAll(buffer);
    if (nread < size) {
        std.debug.print("Could not read file \"{s}\"\n", .{absPath});
        return error.FileNotRead;
    }
    return buffer;
}

test "chunk doesn't leak" {
    var vm = VM.init();
    defer vm.deinit();

    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    var constant = try chunk.addConstant(1.2);
    try chunk.writeOp(.op_constant, 123);
    try chunk.write(@intCast(u8, constant), 123);
    constant = try chunk.addConstant(3.4);
    try chunk.writeOp(.op_constant, 123);
    try chunk.write(@intCast(u8, constant), 123);
    try chunk.writeOp(.op_add, 123);
    constant = try chunk.addConstant(5.6);
    try chunk.writeOp(.op_constant, 123);
    try chunk.write(@intCast(u8, constant), 123);
    try chunk.writeOp(.op_divide, 123);
    try chunk.writeOp(.op_negate, 123);
    try chunk.writeOp(.op_return, 123);

    _ = try vm.interpret(&chunk);
}

test "can read file" {
    var text = try readFile(std.testing.allocator, "build.zig");
    defer std.testing.allocator.free(text);
}
