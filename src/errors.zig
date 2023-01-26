const std = @import("std");

pub var had_error = false;

pub fn fail(line: usize, message: []const u8) void {
    report(line, "", message);
}

fn report(line: usize, where: []const u8, message: []const u8) void {
    std.debug.print("[line {d}] Error{s}: {s}\n", .{ line, where, message });
    had_error = true;
}
