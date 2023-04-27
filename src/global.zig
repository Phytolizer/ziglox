const std = @import("std");

pub var allocator: std.mem.Allocator = undefined;
pub var gpa: std.heap.GeneralPurposeAllocator(.{}) = undefined;