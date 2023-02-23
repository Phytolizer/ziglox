const std = @import("std");
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const ObjString = value_mod.ObjString;
const g = @import("global.zig");
const memory = @import("memory.zig");

pub const Table = struct {
    entries: []Entry = &.{},
    count: usize = 0,

    pub fn deinit(self: @This()) void {
        g.allocator.free(self.entries);
    }

    const MAX_LOAD = 0.75;

    pub fn get(self: @This(), key: *ObjString) ?Value {
        if (self.entries.len == 0) return null;
        const entry = findEntry(self.entries, key);
        if (entry.key == null) return null;
        return entry.value;
    }

    pub fn findString(self: @This(), text: []const u8, hash: u32) ?*ObjString {
        if (self.entries.len == 0) return null;
        var index = hash % self.entries.len;
        while (true) {
            const entry = &self.entries[index];
            if (entry.key) |key| {
                if (key.hash == hash and std.mem.eql(u8, key.text, text))
                    return key;
            } else if (entry.value.isNil()) return null;
            index = (index + 1) % self.entries.len;
        }
    }

    pub fn set(self: *@This(), key: *ObjString, value: Value) !bool {
        if (self.count + 1 > @floatToInt(usize, @intToFloat(f64, self.entries.len) * MAX_LOAD)) {
            const capacity = memory.growCapacity(self.entries.len);
            try self.adjustCapacity(capacity);
        }
        const entry = findEntry(self.entries, key);
        const is_new_key = entry.key == null;
        if (is_new_key and entry.value.isNil()) self.count += 1;

        entry.key = key;
        entry.value = value;
        return is_new_key;
    }

    pub fn delete(self: *@This(), key: *ObjString) bool {
        if (self.entries.len == 0) return false;

        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        entry.key = null;
        entry.value = .{ .boolean = true };

        return true;
    }

    pub fn addAll(from: @This(), to: *@This()) !void {
        for (from.entries) |entry| {
            if (entry.key != null) {
                try to.set(entry.key, entry.value);
            }
        }
    }

    fn adjustCapacity(self: *@This(), capacity: usize) !void {
        const entries = try g.allocator.alloc(Entry, capacity);
        std.mem.set(Entry, entries, .{ .key = null, .value = .nil });

        self.count = 0;
        for (self.entries) |entry| {
            if (entry.key) |key| {
                const dest = findEntry(entries, key);
                dest.* = entry;
                self.count += 1;
            }
        }

        g.allocator.free(self.entries);
        self.entries = entries;
    }

    fn findEntry(entries: []Entry, key: *ObjString) *Entry {
        var index = key.hash % entries.len;
        var tombstone: ?*Entry = null;
        while (true) {
            const entry = &entries[index];
            if (entry.key == null) {
                if (entry.value.isNil()) {
                    return tombstone orelse entry;
                } else {
                    if (tombstone == null) tombstone = entry;
                }
            } else if (entry.key == key) {
                return entry;
            }
            index = (index + 1) % entries.len;
        }
    }
};

pub const Entry = struct {
    key: ?*ObjString,
    value: Value,
};
