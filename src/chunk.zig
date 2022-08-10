const std = @import("std");
const memory = @import("memory.zig");
const Allocator = std.mem.Allocator;
const valueMod = @import("value.zig");
const ValueArray = valueMod.ValueArray;
const Value = valueMod.Value;

pub const OpCode = enum(u8) {
    op_constant,
    op_return,
};

pub const Chunk = struct {
    allocator: Allocator,
    count: usize,
    capacity: usize,
    code: ?[]u8,
    constants: ValueArray,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .allocator = allocator,
            .count = 0,
            .capacity = 0,
            .code = null,
            .constants = ValueArray.init(allocator),
        };
    }

    pub fn write(self: *Self, byte: u8) !void {
        if (self.capacity < self.count + 1) {
            self.capacity = memory.growCapacity(self.capacity);
            self.code = try memory.growArray(u8, self.allocator, self.code, self.capacity);
        }

        self.code.?[self.count] = byte;
        self.count += 1;
    }

    pub fn writeOp(self: *Self, op: OpCode) !void {
        return self.write(@enumToInt(op));
    }

    pub fn deinit(self: *Self) void {
        if (self.code != null) {
            memory.freeArray(u8, self.allocator, self.code.?);
        }
        self.constants.deinit();
        self.* = Self.init(self.allocator);
    }

    pub fn addConstant(self: *Self, value: Value) !usize {
        try self.constants.write(value);
        return self.constants.count - 1;
    }
};
