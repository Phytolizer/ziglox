const std = @import("std");
const memory = @import("memory.zig");
const Allocator = std.mem.Allocator;
const valueMod = @import("value.zig");
const ValueArray = valueMod.ValueArray;
const Value = valueMod.Value;

pub const OpCode = enum(u8) {
    op_constant,
    op_negate,
    op_nil,
    op_true,
    op_false,
    op_pop,
    op_get_global,
    op_define_global,
    op_equal,
    op_greater,
    op_less,
    op_add,
    op_subtract,
    op_multiply,
    op_divide,
    op_not,
    op_print,
    op_return,
};

pub const Chunk = struct {
    allocator: Allocator,
    count: usize,
    capacity: usize,
    code: ?[]u8,
    lines: ?[]usize,
    constants: ValueArray,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .allocator = allocator,
            .count = 0,
            .capacity = 0,
            .code = null,
            .lines = null,
            .constants = ValueArray.init(allocator),
        };
    }

    pub fn write(self: *Self, byte: u8, line: usize) !void {
        if (self.capacity < self.count + 1) {
            self.capacity = memory.growCapacity(self.capacity);
            self.code = try memory.growArray(u8, self.allocator, self.code, self.capacity);
            self.lines = try memory.growArray(usize, self.allocator, self.lines, self.capacity);
        }

        self.code.?[self.count] = byte;
        self.lines.?[self.count] = line;
        self.count += 1;
    }

    pub fn writeOp(self: *Self, op: OpCode, line: usize) !void {
        return self.write(@enumToInt(op), line);
    }

    pub fn deinit(self: *Self) void {
        if (self.code != null) {
            memory.freeArray(u8, self.allocator, self.code.?);
        }
        if (self.lines != null) {
            memory.freeArray(usize, self.allocator, self.lines.?);
        }
        self.constants.deinit();
        self.* = Self.init(self.allocator);
    }

    pub fn addConstant(self: *Self, value: Value) !usize {
        try self.constants.write(value);
        return self.constants.count - 1;
    }
};
