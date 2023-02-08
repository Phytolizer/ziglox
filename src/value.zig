const g = @import("global.zig");
const memory = @import("memory.zig");

pub const Value = f64;

pub fn printValue(writer: anytype, v: Value) !void {
    return try writer.print("{d}", .{v});
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
