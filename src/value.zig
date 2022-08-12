const std = @import("std");
const Allocator = std.mem.Allocator;
const memory = @import("memory.zig");
const Writer = std.fs.File.Writer;
const objectMod = @import("object.zig");
const Obj = objectMod.Obj;

pub const Value = union(enum) {
    nil,
    boolean: bool,
    number: f64,
    obj: *Obj,

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

    pub fn asObj(self: Self) !*Obj {
        return switch (self) {
            Self.obj => |o| o,
            else => error.NotAnObj,
        };
    }

    pub fn isNil(self: Self) bool {
        return switch (self) {
            Self.nil => true,
            else => false,
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

    pub fn isObj(self: Self) bool {
        return switch (self) {
            Self.obj => true,
            else => false,
        };
    }

    pub fn maybeBool(self: Self) ?bool {
        return switch (self) {
            Self.boolean => |b| b,
            else => null,
        };
    }

    pub fn maybeNumber(self: Self) ?f64 {
        return switch (self) {
            Self.number => |n| n,
            else => null,
        };
    }

    pub fn maybeObj(self: Self) ?*Obj {
        return switch (self) {
            Self.obj => |o| o,
            else => null,
        };
    }

    pub fn isFalsey(self: Self) bool {
        return switch (self) {
            Self.nil => true,
            Self.boolean => |b| !b,
            else => false,
        };
    }

    pub fn equals(self: Self, other: Self) bool {
        return switch (self) {
            Self.boolean => |sb| if (other.maybeBool()) |ob|
                sb == ob
            else
                false,
            Self.number => |sn| if (other.maybeNumber()) |on|
                sn == on
            else
                false,
            Self.obj => |so| if (other.maybeObj()) |oo| {
                const a = so.asString();
                const b = oo.asString();
                return std.mem.eql(u8, a.data, b.data);
            } else false,
            Self.nil => other.isNil(),
        };
    }
};

pub fn nilVal() Value {
    return Value.nil;
}

pub fn boolVal(b: bool) Value {
    return Value{ .boolean = b };
}

pub fn numberVal(n: f64) Value {
    return Value{ .number = n };
}

pub fn objVal(o: *Obj) Value {
    return Value{ .obj = o };
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
        Value.obj => {
            try objectMod.printObj(writer, value);
        },
    }
}
