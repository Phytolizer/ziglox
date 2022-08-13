const valueMod = @import("value.zig");
const Value = valueMod.Value;
const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.fs.File.Writer;
const VM = @import("vm.zig").VM;
const memoryMod = @import("memory.zig");
const Chunk = @import("chunk.zig").Chunk;

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
    return try o.asString();
}

pub fn asCString(v: Value) ![]u8 {
    const s = try asString(v);
    return s.data;
}

pub fn isFunction(v: Value) bool {
    return isObjKind(v, .obj_function);
}

pub fn asFunction(v: Value) !*Obj.Function {
    const o = try v.asObj();
    return try o.asFunction();
}

pub fn isNative(v: Value) bool {
    return isObjKind(v, .obj_native);
}

pub fn asNative(v: Value) !*Obj.Native {
    const o = try v.asObj();
    return try o.asNative();
}

pub const ObjKind = enum {
    obj_string,
    obj_function,
    obj_native,
};

pub const Obj = struct {
    kind: ObjKind,
    next: ?*Obj = null,

    const Self = @This();

    pub fn deinit(self: *Self, allocator: Allocator) void {
        switch (self.kind) {
            .obj_string => {
                const s = self.asString() catch unreachable;
                memoryMod.freeArray(u8, allocator, s.data);
                allocator.destroy(s);
            },
            .obj_function => {
                const function = self.asFunction() catch unreachable;
                function.chunk.deinit();
                allocator.destroy(function);
            },
            .obj_native => {
                allocator.destroy(self.asNative() catch unreachable);
            },
        }
    }

    pub fn asString(self: *Self) !*String {
        switch (self.kind) {
            .obj_string => return @fieldParentPtr(String, "obj", self),
            else => return error.NotAString,
        }
    }
    pub const String = struct {
        obj: Obj,
        data: []u8,
        hash: u32,
    };

    pub fn asFunction(self: *Self) !*Function {
        switch (self.kind) {
            .obj_function => return @fieldParentPtr(Function, "obj", self),
            else => return error.NotAFunction,
        }
    }
    pub const Function = struct {
        obj: Obj,
        arity: usize,
        chunk: Chunk,
        name: ?*String,
    };

    pub fn asNative(self: *Self) !*NativeFn {
        switch (self.kind) {
            .obj_native => return @fieldParentPtr(NativeFn, "obj", self),
            else => return error.NotANative,
        }
    }
    pub const NativeFnImpl = fn (args: []Value) Value;
    pub const NativeFn = struct {
        obj: Obj,
        function: NativeFnImpl,
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
        .obj_function => {
            try printFunction(writer, obj.asFunction() catch unreachable);
        },
        .obj_native => {
            try writer.print("<native fn>", .{});
        },
    }
}

fn printFunction(writer: Writer, function: *Obj.Function) !void {
    if (function.name) |name| {
        try writer.print("<fn {s}>", .{name.data});
    } else {
        try writer.print("<script>", .{});
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

pub fn initFunction(vm: *VM) !*Obj.Function {
    const function = try allocateObj(Obj.Function, vm, .obj_function);
    function.arity = 0;
    function.name = null;
    function.chunk = Chunk.init(vm.allocator);
    return function;
}

pub fn initNative(vm: *VM, function: Obj.NativeFnImpl) !*Obj.NativeFn {
    const native = try allocateObj(Obj.NativeFn, vm, .obj_native);
    native.function = function;
    return native;
}
