const std = @import("std");
const g = @import("global.zig");
const vm_mod = @import("vm.zig");

const vm = &vm_mod.vm;

pub const Obj = struct {
    kind: Kind,
    next: ?*Obj = null,

    pub const Kind = enum {
        string,
    };

    pub fn deinit(self: *@This()) void {
        switch (self.kind) {
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
};

pub fn copyString(text: []const u8) !*ObjString {
    const heap_chars = try g.allocator.dupe(u8, text);
    return try allocateString(heap_chars);
}

pub fn takeString(text: []u8) !*ObjString {
    return try allocateString(text);
}

fn allocateObj(comptime T: type, kind: Obj.Kind) !*T {
    const obj = try g.allocator.create(T);
    obj.obj = .{
        .kind = kind,
        .next = vm.objects,
    };
    vm.objects = castObj(obj);
    return obj;
}

fn allocateString(text: []u8) !*ObjString {
    const obj = try allocateObj(ObjString, .string);
    obj.text = text;
    return obj;
}
