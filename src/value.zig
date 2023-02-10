const std = @import("std");
const g = @import("global.zig");
const memory = @import("memory.zig");

pub const Value = union(Kind) {
    boolean: bool,
    nil,
    number: f64,

    pub const Kind = enum {
        boolean,
        nil,
        number,
    };

    pub fn isBoolean(self: @This()) bool {
        return @as(Kind, self) == .boolean;
    }

    pub fn isNil(self: @This()) bool {
        return @as(Kind, self) == .nil;
    }

    pub fn isNumber(self: @This()) bool {
        return @as(Kind, self) == .number;
    }
};

pub fn printValue(writer: anytype, v: Value) !void {
    switch (v) {
        .boolean => |b| try writer.print("{s}", .{if (b) "true" else "false"}),
        .nil => try writer.print("nil", .{}),
        .number => |n| try writer.print("{d}", .{n}),
    }
}

pub const ValueArray = struct {
    // same pattern as chunk.Chunk
    values_alloc: []Value,
    values: []Value,

    pub fn init() @This() {
        return .{
            .values_alloc = &.{},
            .values = &.{},
        };
    }

    pub fn deinit(self: @This()) void {
        g.allocator.free(self.values_alloc);
    }

    pub fn write(self: *@This(), value: Value) !void {
        if (self.values_alloc.len == self.values.len) {
            const capacity = memory.growCapacity(self.values_alloc.len);
            self.values_alloc = try g.allocator.realloc(self.values_alloc, capacity);
        }

        self.values_alloc[self.values.len] = value;
        self.values = self.values_alloc[0 .. self.values.len + 1];
    }
};
