const std = @import("std");
const debug = @import("debug.zig");
const vm_mod = @import("vm.zig");
const vm = &vm_mod.vm;
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const ValueArray = value_mod.ValueArray;
const Obj = value_mod.Obj;
const ObjUpvalue = value_mod.ObjUpvalue;
const ObjFunction = value_mod.ObjFunction;
const ObjClosure = value_mod.ObjClosure;
const compiler = @import("compiler.zig");
const g = @import("global.zig");

pub const GcAllocator = struct {
    a: std.mem.Allocator,

    pub fn init(a: std.mem.Allocator) @This() {
        return .{ .a = a };
    }

    pub fn allocator(self: *@This()) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
            },
        };
    }

    fn alloc(p: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self = @as(*@This(), @ptrCast(@alignCast(p)));
        if (debug.STRESS_GC)
            collectGarbage();
        return self.a.rawAlloc(len, ptr_align, ret_addr);
    }

    fn resize(
        p: *anyopaque,
        old_mem: []u8,
        old_align: u8,
        new_size: usize,
        ret_addr: usize,
    ) bool {
        const self = @as(*@This(), @ptrCast(@alignCast(p)));
        return self.a.rawResize(
            old_mem,
            old_align,
            new_size,
            ret_addr,
        );
    }

    fn free(p: *anyopaque, old_mem: []u8, old_align: u8, ret_addr: usize) void {
        const self = @as(*@This(), @ptrCast(@alignCast(p)));
        self.a.rawFree(old_mem, old_align, ret_addr);
    }
};

pub fn growCapacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

pub fn markObject(object: *Obj) void {
    if (object.is_marked) return;

    if (debug.LOG_GC) {
        std.debug.print("0x{x} mark ", .{@intFromPtr(object)});
        value_mod.printValue(
            std.io.getStdErr().writer(),
            Value.initObj(object),
        ) catch unreachable;
        std.debug.print("\n", .{});
    }
    object.is_marked = true;

    if (vm.gray_stack.len < vm.gray_count + 1) {
        const new_capacity = growCapacity(vm.gray_stack.len);
        vm.gray_stack = g.gpa.allocator().realloc(vm.gray_stack, new_capacity) catch unreachable;
    }

    vm.gray_stack[vm.gray_count] = object;
    vm.gray_count += 1;
}

pub fn markValue(value: Value) void {
    if (value.isObj()) {
        markObject(value.obj);
    }
}

fn markArray(array: *ValueArray) void {
    for (array.values) |value| {
        markValue(value);
    }
}

fn blackenObject(object: *Obj) void {
    if (debug.LOG_GC) {
        std.debug.print("0x{x} blacken ", .{@intFromPtr(object)});
        value_mod.printValue(
            std.io.getStdErr().writer(),
            Value.initObj(object),
        ) catch unreachable;
        std.debug.print("\n", .{});
    }

    switch (object.kind) {
        .closure => {
            const closure = @fieldParentPtr(ObjClosure, "obj", object);
            markObject(value_mod.castObj(closure.function));
            for (closure.upvalues) |upvalue| {
                if (upvalue) |uv| {
                    markObject(value_mod.castObj(uv));
                }
            }
        },
        .function => {
            const function = @fieldParentPtr(ObjFunction, "obj", object);
            if (function.name) |name| {
                markObject(value_mod.castObj(name));
            }
            markArray(&function.chunk.constants);
        },
        .upvalue => {
            const upvalue = @fieldParentPtr(ObjUpvalue, "obj", object);
            markValue(upvalue.closed);
        },
        .native, .string => {},
    }
}

fn markRoots() void {
    for (vm.stack[0..vm.stack_top]) |slot| {
        markValue(slot);
    }

    for (vm.frames[0..vm.frame_count]) |frame| {
        markObject(value_mod.castObj(frame.closure));
    }

    var upvalue = vm.open_upvalues;
    while (upvalue) |uv| : (upvalue = uv.next) {
        markObject(value_mod.castObj(uv));
    }

    vm.globals.mark();
    compiler.markRoots();
}

fn traceReferences() void {
    while (vm.gray_count > 0) {
        vm.gray_count -= 1;
        const object = vm.gray_stack[vm.gray_count];
        blackenObject(object);
    }
}

fn sweep() void {
    var previous: ?*Obj = null;
    var object = vm.objects;
    while (object) |obj| {
        if (obj.is_marked) {
            obj.is_marked = false;
            previous = obj;
            object = obj.next;
        } else {
            const unreached = obj;
            object = obj.next;
            if (previous) |prev| {
                prev.next = object;
            } else {
                vm.objects = object;
            }

            unreached.deinit();
        }
    }
}

pub fn collectGarbage() void {
    if (debug.LOG_GC) {
        std.debug.print("-- gc begin\n", .{});
    }

    markRoots();
    traceReferences();
    vm.strings.removeWhite();
    sweep();

    if (debug.LOG_GC) {
        std.debug.print("-- gc end\n", .{});
    }
}
