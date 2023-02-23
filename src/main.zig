const std = @import("std");
const g = @import("global.zig");
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const debug = @import("debug.zig");
const vm = @import("vm.zig");
const memory_mod = @import("memory.zig");
const GcAllocator = memory_mod.GcAllocator;

pub fn main() void {
    // Hack to allow `defer` to work properly.
    // std.process.exit does NOT run deferred code, so this is needed.
    run() catch |e| switch (e) {
        error.Usage => std.process.exit(64),
        error.Compile => std.process.exit(65),
        error.Runtime => std.process.exit(70),
        else => std.process.exit(1),
    };
}

fn repl() !void {
    var line_buf = try std.ArrayList(u8).initCapacity(g.allocator, 1024);
    defer line_buf.deinit();
    const stdin = std.io.getStdIn().reader();

    while (true) {
        try std.io.getStdOut().writeAll("> ");

        line_buf.clearRetainingCapacity();
        stdin.readUntilDelimiterArrayList(
            &line_buf,
            '\n',
            std.math.maxInt(usize),
        ) catch |e| switch (e) {
            error.EndOfStream => break,
            else => return e,
        };

        vm.interpret(line_buf.items) catch |e| switch (e) {
            error.Compile => {},
            error.Runtime => {},
            else => return e,
        };
    }
}

fn runFile(path: []const u8) !void {
    const source = std.fs.cwd().readFileAlloc(g.allocator, path, std.math.maxInt(usize)) catch |e| {
        std.debug.print("Could not read \"{s}\": {s}\n", .{ path, @errorName(e) });
        return e;
    };
    defer g.allocator.free(source);

    try vm.interpret(source);
}

fn run() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.detectLeaks();
    g.gpa = gpa;
    var gca = GcAllocator.init(gpa.allocator());
    g.allocator = gca.allocator();

    try vm.init();
    defer vm.deinit();

    const args = try std.process.argsAlloc(g.allocator);
    defer std.process.argsFree(g.allocator, args);

    switch (args.len) {
        1 => try repl(),
        2 => try runFile(args[1]),
        else => {
            std.debug.print("Usage: clox [path]\n", .{});
            return error.Usage;
        },
    }
}
