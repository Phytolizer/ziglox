const VM = @import("vm.zig").VM;
const Scanner = @import("scanner.zig").Scanner;
const stdout = @import("debug.zig").stdout;

pub fn compile(source: []const u8) !void {
    var scanner = Scanner.init(source);
    _ = scanner;
    var line: ?usize = null;
    while (true) {
        const token = scanner.scanToken();
        if (line == null or token.line != line.?) {
            try stdout.print("{d:4} ", .{token.line});
            line = token.line;
        } else {
            try stdout.writeAll("   | ");
        }
        try stdout.print("{d:2} '{s}'\n", .{ @enumToInt(token.kind), token.text });

        if (token.kind == .tk_eof) {
            break;
        }
    }
}
