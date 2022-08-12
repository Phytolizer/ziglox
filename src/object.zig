const valueMod = @import("value.zig");
const Value = valueMod.Value;
const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.fs.File.Writer;

pub fn objKind(v: Value) !ObjKind {
    const o = try v.asObj();
    return o.kind;
}

pub fn isObjKind(v: Value, kind: ObjKind) bool {
    const valueKind = objKind(v) catch
        return false;
    return valueKind == kind;
}

pub fn isString(v: Value) bool {
    return isObjKind(v, .obj_string);
}

pub fn asString(v: Value) !*Obj.String {
    const o = try v.asObj();
    return o.asString();
}

pub fn asCString(v: Value) ![]u8 {
    const s = try asString(v);
    return s.data;
}

pub const ObjKind = enum {
    obj_string,
};

pub const Obj = struct {
    kind: ObjKind,

    const Self = @This();

    pub fn asString(self: *Self) *String {
        switch (self.kind) {
            .obj_string => return @fieldParentPtr(String, "obj", self),
            // else => return error.NotAString,
        }
    }
    pub const String = struct {
        obj: Obj,
        data: []u8,
    };
};

pub fn copyString(allocator: Allocator, chars: []const u8) !*Obj {
    const heapChars = try allocator.alloc(u8, chars.len);
    std.mem.copy(u8, heapChars, chars);
    const str = try allocateString(allocator, heapChars);
    return &str.obj;
}

fn allocateObj(comptime T: type, allocator: Allocator, objectKind: ObjKind) !*T {
    const obj = try allocator.create(T);
    obj.obj.kind = objectKind;
    return obj;
}

fn allocateString(allocator: Allocator, chars: []u8) !*Obj.String {
    const string = try allocateObj(Obj.String, allocator, .obj_string);
    string.data = chars;
    return string;
}

pub fn printObj(writer: Writer, value: Value) !void {
    const obj = value.asObj() catch unreachable;
    switch (obj.kind) {
        .obj_string => {
            try writer.print("{s}", .{asCString(value) catch unreachable});
        },
    }
}
