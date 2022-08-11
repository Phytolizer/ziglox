const std = @import("std");
const Allocator = std.mem.Allocator;
const memory = @import("memory.zig");
const Writer = std.fs.File.Writer;

pub const Value = union(enum) {
    boolean: bool,
    number: f64,
    nil,

    const Self = @This();

    pub fn asBool(self: Self) !bool {
        return switch (self) {
            Self.boolean => |b| b,
            else => error.NotABool,
        };
    }

    pub fn asNumber(self: Self) !f64 {
        return switch (self) {
            Self.number => |n| n,
            else => error.NotANumber,
        };
    }

    pub fn isBool(self: Self) bool {
        return switch (self) {
            Self.boolean => true,
            else => false,
        };
    }

    pub fn isNumber(self: Self) bool {
        return switch (self) {
            Self.number => true,
            else => false,
        };
    }

    pub fn isNil(self: Self) bool {
        return switch (self) {
            Self.nil => true,
            else => false,
        };
    }
};

pub fn boolVal(b: bool) Value {
    return Value{ .boolean = b };
}

pub fn nilVal() Value {
    return Value.nil;
}

pub fn numberVal(n: f64) Value {
    return Value{ .number = n };
}

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
    switch (value) {
        Value.boolean => |b| {
            try writer.print("{}", .{b});
        },
        Value.nil => {
            try writer.writeAll("nil");
        },
        Value.number => |n| {
            try writer.print("{d}", .{n});
        },
    }
}
