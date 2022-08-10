const std = @import("std");
const memory = @import("memory.zig");
const Allocator = std.mem.Allocator;

pub const OpCode = enum(u8) {
    op_return,
};

pub const Chunk = struct {
    allocator: Allocator,
    count: usize,
    capacity: usize,
    code: []u8,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .allocator = allocator,
            .count = 0,
            .capacity = 0,
            .code = &.{},
        };
    }

    pub fn write(self: *Self, byte: u8) !void {
        if (self.capacity < self.count + 1) {
            self.capacity = memory.growCapacity(self.capacity);
            const newCode = try memory.growArray(u8, self.allocator, self.code, self.capacity);
            self.code = newCode.?;
        }

        self.code[self.count] = byte;
        self.count += 1;
    }

    pub fn writeOp(self: *Self, op: OpCode) !void {
        return self.write(@enumToInt(op));
    }

    pub fn deinit(self: *Self) void {
        memory.freeArray(u8, self.allocator, self.code);
        self.* = Self.init(self.allocator);
    }
};
