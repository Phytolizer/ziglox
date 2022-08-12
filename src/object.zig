const valueMod = @import("value.zig");
const Value = valueMod.Value;
const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.fs.File.Writer;
const VM = @import("vm.zig").VM;
const memoryMod = @import("memory.zig");

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
    next: ?*Obj = null,

    const Self = @This();

    pub fn deinit(self: *Self, allocator: Allocator) void {
        switch (self.kind) {
            .obj_string => {
                const s = self.asString();
                memoryMod.freeArray(u8, allocator, s.data);
                allocator.destroy(s);
            },
        }
    }

    pub fn asString(self: *Self) *String {
        switch (self.kind) {
            .obj_string => return @fieldParentPtr(String, "obj", self),
            // else => return error.NotAString,
        }
    }
    pub const String = struct {
        obj: Obj,
        data: []u8,
        hash: u32,
    };
};

fn hashString(chars: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (chars) |c| {
        hash ^= c;
        hash *%= 16777619;
    }
    return hash;
}

pub fn copyString(vm: *VM, chars: []const u8) !*Obj {
    const hash = hashString(chars);
    if (vm.strings.findString(chars, hash)) |interned| {
        return &interned.obj;
    }
    const heapChars = try vm.allocator.alloc(u8, chars.len);
    std.mem.copy(u8, heapChars, chars);
    const str = try allocateString(vm, heapChars, hash);
    return &str.obj;
}

fn allocateObj(comptime T: type, vm: *VM, objectKind: ObjKind) !*T {
    const obj = try vm.allocator.create(T);
    obj.obj.kind = objectKind;
    obj.obj.next = vm.objects;
    vm.objects = &obj.obj;
    return obj;
}

fn allocateString(vm: *VM, chars: []u8, hash: u32) !*Obj.String {
    const string = try allocateObj(Obj.String, vm, .obj_string);
    string.data = chars;
    string.hash = hash;
    _ = try vm.strings.set(string, valueMod.nilVal());
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

pub fn takeString(vm: *VM, chars: []u8) !*Obj.String {
    const hash = hashString(chars);
    if (vm.strings.findString(chars, hash)) |interned| {
        vm.allocator.free(chars);
        return interned;
    }
    const result = try allocateString(vm, chars, hash);
    return result;
}
