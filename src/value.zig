const std = @import("std");
const g = @import("global.zig");
const memory = @import("memory.zig");
const obj_mod = @import("obj.zig");
const Obj = obj_mod.Obj;
const ObjString = obj_mod.ObjString;
const ObjFunction = obj_mod.ObjFunction;
const castObj = obj_mod.castObj;

pub const Value = union(Kind) {
    boolean: bool,
    nil,
    number: f64,
    obj: *Obj,

    pub const Kind = enum {
        boolean,
        nil,
        number,
        obj,
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

    pub fn isObj(self: @This()) bool {
        return @as(Kind, self) == .obj;
    }

    pub fn objKind(self: @This()) Obj.Kind {
        return self.obj.kind;
    }

    pub fn equals(a: @This(), b: @This()) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .boolean => a.boolean == b.boolean,
            .nil => true,
            .number => a.number == b.number,
            .obj => a.obj == b.obj,
        };
    }

    pub fn isString(self: @This()) bool {
        return isObjKind(self, .string);
    }

    pub fn isObjKind(self: @This(), kind: Obj.Kind) bool {
        return self.isObj() and self.obj.kind == kind;
    }

    pub fn asString(self: @This()) *ObjString {
        return @fieldParentPtr(ObjString, "obj", self.obj);
    }

    pub fn asCstring(self: @This()) []const u8 {
        return self.asString().text;
    }

    pub fn isFunction(self: @This()) bool {
        return isObjKind(self, .function);
    }

    pub fn asFunction(self: @This()) *ObjFunction {
        return @fieldParentPtr(ObjFunction, "obj", self.obj);
    }

    /// This method exists to allow passing a subclass of Obj directly, e.g. *ObjString.
    pub fn initObj(obj: anytype) @This() {
        return .{ .obj = castObj(obj) };
    }
};

pub fn printValue(writer: anytype, v: Value) !void {
    switch (v) {
        .boolean => |b| try writer.print("{s}", .{if (b) "true" else "false"}),
        .nil => try writer.print("nil", .{}),
        .number => |n| try writer.print("{d}", .{n}),
        .obj => try printObj(writer, v),
    }
}

fn printObj(writer: anytype, v: Value) !void {
    switch (v.objKind()) {
        .function => {
            const function = v.asFunction();
            if (function.name) |name| {
                try writer.print("<fn {s}>", .{name.text});
            } else {
                try writer.writeAll("<script>");
            }
        },
        .string => {
            try writer.print("{s}", .{v.asCstring()});
        },
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
