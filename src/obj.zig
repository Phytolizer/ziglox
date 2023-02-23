const std = @import("std");
const g = @import("global.zig");
const vm_mod = @import("vm.zig");
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;

const vm = &vm_mod.vm;

pub const Obj = struct {
    kind: Kind,
    next: ?*Obj = null,

    pub const Kind = enum {
        function,
        string,
    };

    pub fn deinit(self: *@This()) void {
        switch (self.kind) {
            .function => {
                const function = @fieldParentPtr(ObjFunction, "obj", self);
                function.chunk.deinit();
                g.allocator.destroy(function);
            },
            .string => {
                const string = @fieldParentPtr(ObjString, "obj", self);
                g.allocator.free(string.text);
                g.allocator.destroy(string);
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
    function.name = null;
    function.chunk = Chunk.init();
    return function;
}
