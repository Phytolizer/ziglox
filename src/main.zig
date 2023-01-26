const std = @import("std");
const errors = @import("errors.zig");
const Scanner = @import("Scanner.zig");

pub fn main() void {
    const exit_code: u8 = err: {
        run() catch |e| {
            break :err switch (e) {
                // Follow sysexits.h convention.
                error.Usage => 64,
                error.Data => 65,
                else => 70,
            };
        };
        break :err 0;
    };
    std.process.exit(exit_code);
}

fn run() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.detectLeaks();
    const a = gpa.allocator();
    const args = try std.process.argsAlloc(a);
    defer std.process.argsFree(a, args);

    switch (args.len) {
        1 => try runPrompt(a),
        2 => try runFile(a, args[1]),
        else => {
            std.debug.print("Usage: {s} [script]\n", .{args[0]});
            return error.Usage;
        },
    }
}

fn runPrompt(a: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    var buffer = std.ArrayList(u8).init(a);
    defer buffer.deinit();

    while (true) {
        try stdout.writeAll("> ");
        std.io.getStdIn().reader().readUntilDelimiterArrayList(
            &buffer,
            '\n',
            std.math.maxInt(usize),
        ) catch |e| switch (e) {
            error.EndOfStream => {
                if (buffer.items.len == 0) {
                    try stdout.writeByte('\n');
                    break;
                }
            },
            else => return e,
        };

        try runSource(a, buffer.items);
        errors.had_error = false;
    }
}

fn runFile(a: std.mem.Allocator, path: []const u8) !void {
    const contents = try std.fs.cwd().readFileAlloc(a, path, std.math.maxInt(usize));
    defer a.free(contents);
    try runSource(a, contents);
    if (errors.had_error) return error.Data;
}

fn runSource(a: std.mem.Allocator, source: []const u8) !void {
    var scanner = Scanner.init(a, source);
    defer scanner.deinit();

    const tokens = try scanner.scanTokens();
    defer {
        for (tokens) |token| {
            token.deinit();
        }
        a.free(tokens);
    }
    const stdout = std.io.getStdOut().writer();
    for (tokens) |token| {
        try stdout.print("{}\n", .{token.literal});
    }
}
