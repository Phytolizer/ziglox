const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn growCapacity(capacity: usize) usize {
    if (capacity < 8) {
        return 8;
    }
    return capacity * 2;
}

pub fn growArray(
    comptime T: type,
    allocator: Allocator,
    array: []T,
    newCount: usize,
) !?[]T {
    const newSize = newCount * @sizeOf(T);
    if (newSize == 0) {
        allocator.free(array);
        return null;
    }

    const result = try allocator.realloc(array, newSize);
    return result;
}

pub fn freeArray(comptime T: type, allocator: Allocator, array: []T) void {
    return allocator.free(array);
}
