const std = @import("std");
const Allocator = std.mem.Allocator;
const memory = @import("memory.zig");
const Writer = std.fs.File.Writer;

pub const Value = f64;

pub const ValueArray = struct {
    allocator: Allocator,
    capacity: usize,
    count: usize,
    values: ?[]Value,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .allocator = allocator,
            .capacity = 0,
            .count = 0,
            .values = null,
        };
    }

    pub fn deinit(self: *Self) void {
        if (self.values != null) {
            memory.freeArray(Value, self.allocator, self.values.?);
        }
        self.* = Self.init(self.allocator);
    }

    pub fn write(self: *Self, value: Value) !void {
        if (self.capacity < self.count + 1) {
            self.capacity = memory.growCapacity(self.capacity);
            self.values = try memory.growArray(Value, self.allocator, self.values, self.capacity);
        }

        self.values.?[self.count] = value;
        self.count += 1;
    }
};

pub fn printValue(writer: Writer, value: Value) !void {
    return writer.print("{d}", .{value});
}
