const std = @import("std");
const g = @import("global.zig");
const memory = @import("memory.zig");
const vm_mod = @import("vm.zig");
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const debug = @import("debug.zig");

const vm = &vm_mod.vm;

pub const Obj = struct {
    kind: Kind,
    is_marked: bool = false,
    next: ?*Obj = null,

    pub const Kind = enum {
        closure,
        function,
        native,
        string,
        upvalue,
    };

    pub fn deinit(self: *@This()) void {
        if (debug.LOG_GC) {
            std.debug.print(
                "0x{x} free type {s}\n",
                .{ @intFromPtr(self), @tagName(self.kind) },
            );
        }

        switch (self.kind) {
            .closure => {
                const closure = @fieldParentPtr(ObjClosure, "obj", self);
                g.allocator.free(closure.upvalues);
                g.allocator.destroy(closure);
            },
            .function => {
                const function = @fieldParentPtr(ObjFunction, "obj", self);
                function.chunk.deinit();
                g.allocator.destroy(function);
            },
            .native => {
                const native = @fieldParentPtr(ObjNative, "obj", self);
                g.allocator.destroy(native);
            },
            .string => {
                const string = @fieldParentPtr(ObjString, "obj", self);
                std.debug.print("{s}\n", .{string.text});
                g.allocator.free(string.text);
                g.allocator.destroy(string);
            },
            .upvalue => {
                const upvalue = @fieldParentPtr(ObjUpvalue, "obj", self);
                g.allocator.destroy(upvalue);
            },
        }
    }
};

pub fn castObj(obj: anytype) *Obj {
    const type_info = @typeInfo(@TypeOf(obj));
    switch (type_info) {
        .Pointer => |info| if (info.child == Obj) {
            // *Obj
            return obj;
        } else {
            switch (@typeInfo(info.child)) {
                .Struct => |sinfo| {
                    if (sinfo.fields[0].type == Obj and std.mem.eql(u8, sinfo.fields[0].name, "obj")) {
                        // e.g. *ObjString or other subclass
                        return &obj.obj;
                    } else unreachable;
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

pub const ObjString = struct {
    obj: Obj,
    text: []u8,
    hash: u32,
};

fn hashString(text: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (text) |c| {
        hash ^= c;
        hash *%= 16777619;
    }
    return hash;
}

pub fn copyString(text: []const u8) !*ObjString {
    const hash = hashString(text);
    return vm.strings.findString(text, hash) orelse blk: {
        const heap_chars = try g.allocator.dupe(u8, text);
        break :blk try allocateString(heap_chars, hash);
    };
}

pub fn takeString(text: []u8) !*ObjString {
    const hash = hashString(text);
    if (vm.strings.findString(text, hash)) |interned| {
        g.allocator.free(text);
        return interned;
    }
    return try allocateString(text, hash);
}

pub const ObjFunction = struct {
    obj: Obj,
    arity: usize,
    upvalue_count: usize,
    chunk: Chunk,
    name: ?*ObjString,
};

fn allocateObj(comptime T: type, kind: Obj.Kind) !*T {
    const obj = try g.allocator.create(T);
    obj.obj = .{
        .kind = kind,
        .next = vm.objects,
    };
    vm.objects = castObj(obj);

    if (debug.LOG_GC) {
        std.debug.print(
            "0x{x} allocate {d} for {s}\n",
            .{ @intFromPtr(obj), @sizeOf(T), @tagName(kind) },
        );
    }

    return obj;
}

fn allocateString(text: []u8, hash: u32) !*ObjString {
    const obj = try allocateObj(ObjString, .string);
    obj.text = text;
    obj.hash = hash;
    _ = try vm.strings.set(obj, .nil);
    return obj;
}

pub fn newFunction() !*ObjFunction {
    const function = try allocateObj(ObjFunction, .function);
    function.arity = 0;
    function.upvalue_count = 0;
    function.name = null;
    function.chunk = Chunk.init();
    return function;
}

pub const NativeFn = fn (args: []const Value) Value;

pub const ObjNative = struct {
    obj: Obj,
    function: *const NativeFn,
};

pub fn newNative(function: *const NativeFn) !*ObjNative {
    const native = try allocateObj(ObjNative, .native);
    native.function = function;
    return native;
}

pub const ObjClosure = struct {
    obj: Obj,
    function: *ObjFunction,
    upvalues: []?*ObjUpvalue,
};

pub fn newClosure(function: *ObjFunction) !*ObjClosure {
    const upvalues = try g.allocator.alloc(?*ObjUpvalue, function.upvalue_count);
    @memset(upvalues, null);
    const closure = try allocateObj(ObjClosure, .closure);
    closure.function = function;
    closure.upvalues = upvalues;
    return closure;
}

pub const ObjUpvalue = struct {
    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*@This(),
};

pub fn newUpvalue(slot: *Value) !*ObjUpvalue {
    const upvalue = try allocateObj(ObjUpvalue, .upvalue);
    upvalue.closed = .nil;
    upvalue.location = slot;
    upvalue.next = null;
    return upvalue;
}

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

    pub fn isNative(self: @This()) bool {
        return isObjKind(self, .native);
    }

    pub fn asNative(self: @This()) *const NativeFn {
        return @fieldParentPtr(ObjNative, "obj", self.obj).function;
    }

    pub fn isClosure(self: @This()) bool {
        return isObjKind(self, .closure);
    }

    pub fn asClosure(self: @This()) *ObjClosure {
        return @fieldParentPtr(ObjClosure, "obj", self.obj);
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

fn printFunction(writer: anytype, function: *ObjFunction) !void {
    if (function.name) |name| {
        try writer.print("<fn {s}>", .{name.text});
    } else {
        try writer.writeAll("<script>");
    }
}

fn printObj(writer: anytype, v: Value) !void {
    switch (v.objKind()) {
        .closure => {
            try printFunction(writer, v.asClosure().function);
        },
        .function => {
            try printFunction(writer, v.asFunction());
        },
        .native => {
            try writer.writeAll("<native fn>");
        },
        .string => {
            try writer.print("{s}", .{v.asCstring()});
        },
        .upvalue => {
            try writer.writeAll("upvalue");
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
