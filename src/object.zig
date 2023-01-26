const std = @import("std");

pub const Object = struct {
    type: Type,
    allocator: std.mem.Allocator,
    deinit_func: *const fn (*@This()) void,

    const Type = enum {
        null,
        integer,
        string,
    };

    pub fn deinit(self: *@This()) void {
        self.deinit_func(self);

        self.allocator.destroy(self);
    }

    pub fn format(self: *const @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.type) {
            .null => {
                const actual = @fieldParentPtr(Null, "base", self);
                try actual.show(writer);
            },
            .integer => {
                const actual = @fieldParentPtr(Integer, "base", self);
                try actual.show(writer);
            },
            .string => {
                const actual = @fieldParentPtr(String, "base", self);
                try actual.show(writer);
            },
        }
    }
};

fn no_deinit(_: *Object) void {}

pub const Null = struct {
    base: Object,

    pub fn show(_: @This(), writer: anytype) !void {
        try writer.print("null", .{});
    }

    pub fn init(a: std.mem.Allocator) !*Object {
        const result = try a.create(@This());
        result.base.type = .null;
        result.base.allocator = a;
        result.base.deinit_func = no_deinit;
        return &result.base;
    }
};

pub const Integer = struct {
    base: Object,
    value: isize,

    pub fn show(self: @This(), writer: anytype) !void {
        try writer.print("{d}", .{self.value});
    }

    pub fn init(a: std.mem.Allocator, value: isize) !*Object {
        const result = try a.create(@This());
        result.base.type = .integer;
        result.base.allocator = a;
        result.base.deinit_func = no_deinit;
        result.value = value;
        return &result.base;
    }
};

pub const String = struct {
    base: Object,
    value: []const u8,

    pub fn show(self: @This(), writer: anytype) !void {
        try writer.print("{s}", .{self.value});
    }

    fn deinit(obj: *Object) void {
        const self = @fieldParentPtr(@This(), "base", obj);
        obj.allocator.free(self.value);
    }

    pub fn init(a: std.mem.Allocator, value: []const u8) !*Object {
        const result = try a.create(@This());
        result.base = .{
            .type = .string,
            .allocator = a,
            .deinit_func = deinit,
        };
        result.value = value;
        return &result.base;
    }
};
