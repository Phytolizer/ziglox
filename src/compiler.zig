const std = @import("std");
const scanner = @import("scanner.zig");

pub fn compile(source: []const u8) !void {
    var bw = std.io.bufferedWriter(std.io.getStdOut().writer());
    var bww = bw.writer();

    scanner.init(source);
    var line: ?usize = null;
    while (true) {
        const token = scanner.scanToken();
        if (line == null or token.line != line.?) {
            try bww.print("{d:>4} ", .{token.line});
            line = token.line;
        } else {
            try bww.writeAll("   | ");
        }
        try bww.print("{d:>2} '{s}'\n", .{ @enumToInt(token.kind), token.text });

        if (token.kind == .eof) break;
    }

    try bw.flush();
}
