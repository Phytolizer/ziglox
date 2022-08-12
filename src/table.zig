const std = @import("std");
const valueMod = @import("value.zig");
const objectMod = @import("object.zig");
const memoryMod = @import("memory.zig");

const Allocator = std.mem.Allocator;
const Value = valueMod.Value;
const Obj = objectMod.Obj;

pub const Table = struct {
    count: usize = 0,
    entries: ?[]Entry = null,
    allocator: Allocator,

    const Self = @This();
    const maxLoad: f64 = 0.75;

    pub const Entry = struct {
        key: ?*Obj.String,
        value: Value,

        pub fn init(key: *Obj.String, value: Value) Entry {
            return Entry{
                .key = key,
                .value = value,
            };
        }
    };

    pub fn init(allocator: Allocator) Self {
        return Self{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        if (self.entries) |entries| {
            memoryMod.freeArray(Entry, self.allocator, entries);
        }
        self.count = 0;
        self.entries = null;
    }

    pub fn get(self: *Self, key: *Obj.String) ?Value {
        if (!self.entries) {
            return null;
        }
        const entry = findEntry(self.entries, key);
        if (entry.key) {
            return entry.value;
        }

        return null;
    }

    fn getCapacity(self: *Self) usize {
        return if (self.entries) |entries|
            entries.len
        else
            0;
    }

    pub fn set(self: *Self, key: *Obj.String, value: Value) !bool {
        if (self.entries == null or @intToFloat(f64, self.count + 1) > @intToFloat(f64, self.entries.?.len) * maxLoad) {
            const capacity = memoryMod.growCapacity(self.getCapacity());
            try self.adjustCapacity(capacity);
        }

        const entry = findEntry(self.entries.?, key);
        const isNewKey = entry.key == null;
        if (isNewKey and entry.value.isNil()) {
            self.count += 1;
        }

        entry.key = key;
        entry.value = value;
        return isNewKey;
    }

    pub fn delete(self: *Self, key: *Obj.String) bool {
        if (self.entries == null) {
            return false;
        }

        const entry = findEntry(self.entries, key);
        if (entry.key == null) {
            return false;
        }

        entry.key = null;
        entry.value = valueMod.boolVal(true);
        return true;
    }

    pub fn addAll(from: *Self, to: *Self) !void {
        for (from.entries) |entry| {
            if (entry.key) |key| {
                try to.set(key, entry.value);
            }
        }
    }

    pub fn findString(self: *Self, chars: []const u8, hash: u32) ?*Obj.String {
        if (self.entries) |entries| {
            var index = @as(usize, hash) % entries.len;
            while (true) {
                const entry = &entries[index];
                if (entry.key) |entryKey| {
                    if (std.mem.eql(u8, entryKey.data, chars)) {
                        return entryKey;
                    }
                } else {
                    if (entry.value.isNil()) {
                        return null;
                    }
                }
                index = (index + 1) % entries.len;
            }
        } else {
            return null;
        }
    }

    fn findEntry(entries: []Entry, key: *Obj.String) *Entry {
        var index = @as(usize, key.hash) % entries.len;
        var tombstone: ?*Entry = null;
        while (true) {
            const entry = &entries[index];
            if (entry.key) |entryKey| {
                if (entryKey == key) {
                    return entry;
                }
            } else {
                if (entry.value.isNil()) {
                    return if (tombstone) |t|
                        t
                    else
                        entry;
                } else {
                    if (tombstone == null) {
                        tombstone = entry;
                    }
                }
            }

            index = (index + 1) % entries.len;
        }
    }

    fn adjustCapacity(self: *Self, capacity: usize) !void {
        const entries = try self.allocator.alloc(Entry, capacity);
        for (entries) |*entry| {
            entry.key = null;
            entry.value = valueMod.nilVal();
        }

        self.count = 0;
        if (self.entries) |oldEntries| {
            for (oldEntries) |entry| {
                if (entry.key) |key| {
                    const dest = findEntry(entries, key);
                    dest.key = key;
                    dest.value = entry.value;
                    self.count += 1;
                }
            }
        }

        if (self.entries) |oldEntries| {
            self.allocator.free(oldEntries);
        }
        self.entries = entries;
    }
};
