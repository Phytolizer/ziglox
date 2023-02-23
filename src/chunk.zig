const std = @import("std");
const g = @import("global.zig");
const memory = @import("memory.zig");
const value = @import("value.zig");

pub const OpCode = enum(u8) {
    constant,
    constant_long,
    nil,
    true,
    false,
    pop,
    get_local,
    get_local_long,
    get_global,
    get_global_long,
    define_global,
    define_global_long,
    set_local,
    set_local_long,
    set_global,
    set_global_long,
    equal,
    greater,
    less,
    add,
    subtract,
    multiply,
    divide,
    not,
    negate,
    print,
    jump,
    jump_if_false,
    loop,
    @"return",
    _,

    pub const names = blk: {
        const tags = std.meta.tags(@This());
        var result: []const []const u8 = &.{};
        for (tags) |tag| {
            const name_lower = @tagName(tag);
            var name: []const u8 = "OP_";
            for (name_lower) |c| {
                name = name ++ &[_]u8{std.ascii.toUpper(c)};
            }
            result = result ++ &[_][]const u8{name};
        }
        break :blk result;
    };
};

const Line = struct {
    num: usize,
    start_byte: usize,
    length: usize,

    fn init(num: usize, offset: usize) @This() {
        return .{
            .num = num,
            .start_byte = offset,
            .length = 1,
        };
    }
};

const LineBuffer = struct {
    lines_alloc: []Line,
    lines: []Line,

    pub fn init() @This() {
        return .{
            .lines_alloc = &.{},
            .lines = &.{},
        };
    }

    pub fn deinit(self: @This()) void {
        g.allocator.free(self.lines_alloc);
    }

    fn shouldAdd(self: @This(), num: usize) bool {
        return self.lines.len == 0 or self.lines[self.lines.len - 1].num != num;
    }

    pub fn addLine(self: *@This(), num: usize, offset: usize) !void {
        if (self.shouldAdd(num)) {
            if (self.lines_alloc.len == self.lines.len) {
                const capacity = memory.growCapacity(self.lines_alloc.len);
                self.lines_alloc = try g.allocator.realloc(self.lines_alloc, capacity);
            }
            self.lines_alloc[self.lines.len] = Line.init(num, offset);
            self.lines = self.lines_alloc[0 .. self.lines.len + 1];
        } else {
            self.lines[self.lines.len - 1].length += 1;
        }
    }
};

pub const Chunk = struct {
    // full buffer
    code_alloc: []u8,
    // smaller view
    code: []u8,
    lines: LineBuffer,
    constants: value.ValueArray,

    pub fn init() @This() {
        return .{
            .code_alloc = &.{},
            .code = &.{},
            .lines = LineBuffer.init(),
            .constants = value.ValueArray.init(),
        };
    }

    pub fn deinit(self: @This()) void {
        g.allocator.free(self.code_alloc);
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *@This(), byte: u8, line: usize) !void {
        if (self.code.len == self.code_alloc.len) {
            const capacity = memory.growCapacity(self.code_alloc.len);
            self.code_alloc = try g.allocator.realloc(self.code_alloc, capacity);
        }

        try self.lines.addLine(line, self.code.len);
        self.code_alloc[self.code.len] = byte;
        self.code = self.code_alloc[0 .. self.code.len + 1];
    }

    pub fn writeOp(self: *@This(), op: OpCode, line: usize) !void {
        return try self.write(@enumToInt(op), line);
    }

    pub fn writeConstant(self: *@This(), v: value.Value, line: usize) !void {
        const constant = try self.addConstant(v);
        if (constant <= std.math.maxInt(u8)) {
            try self.writeOp(OpCode.constant, line);
            try self.write(@intCast(u8, constant), line);
        } else if (constant <= std.math.maxInt(u24)) {
            // serialize 24-bit big-endian value
            try self.writeOp(OpCode.constant_long, line);
            try self.write(@truncate(u8, constant >> 16), line);
            try self.write(@truncate(u8, constant >> 8), line);
            try self.write(@truncate(u8, constant), line);
        } else unreachable;
    }

    pub fn getLine(self: *const @This(), offset: usize) usize {
        for (self.lines.lines) |line| {
            if (line.start_byte <= offset and line.start_byte + line.length > offset) {
                return line.num;
            }
        }
        unreachable;
    }

    pub fn addConstant(self: *@This(), v: value.Value) !usize {
        try self.constants.write(v);
        return self.constants.values.len - 1;
    }
};
