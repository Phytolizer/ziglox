const std = @import("std");
const runTest = @import("lox").runTest;

pub fn main() !void {
    var iter_dir = try std.fs.cwd().openIterableDir("src/tests", .{});
    defer iter_dir.close();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.detectLeaks();
    const a = gpa.allocator();
    var it = try iter_dir.walk(a);
    defer it.deinit();

    var entries_list = std.ArrayList([]const u8).init(a);
    defer {
        for (entries_list.items) |ent| {
            a.free(ent);
        }
        entries_list.deinit();
    }

    while (try it.next()) |ent| {
        if (ent.kind != .File)
            continue;
        const full_path = try std.fs.path.join(a, &.{ "src", "tests", ent.path });
        errdefer a.free(full_path);
        try entries_list.append(full_path);
    }

    const entries = entries_list.items;

    var progress = std.Progress{};
    const root_node = progress.start("Test", entries.len);

    var counts = [_]usize{ 0, 0 };

    for (entries) |ent| {
        var test_node = root_node.start(ent, 0);
        test_node.activate();
        progress.refresh();
        std.debug.print("\n", .{});
        const result = runTest(a, ent) catch false;
        counts[@boolToInt(result)] += 1;
        test_node.end();
    }

    std.debug.print("{d} passed, {d} failed\n", .{ counts[1], counts[0] });
    if (counts[0] > 0) {
        return error.Test;
    }
}
