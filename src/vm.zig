const chunkMod = @import("chunk.zig");
const Chunk = chunkMod.Chunk;
const OpCode = chunkMod.OpCode;
const valueMod = @import("value.zig");
const Value = valueMod.Value;
const debug = @import("debug.zig");

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

pub const VM = struct {
    chunk: ?*Chunk,
    ip: usize,

    const Self = @This();

    pub fn init() Self {
        return Self{
            .chunk = null,
            .ip = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn interpret(self: *Self, chunk: *Chunk) !InterpretResult {
        self.chunk = chunk;
        self.ip = 0;
        return self.run();
    }

    fn readByte(self: *Self) u8 {
        const b = self.chunk.?.code.?[self.ip];
        self.ip += 1;
        return b;
    }

    fn readConstant(self: *Self) Value {
        return self.chunk.?.constants.values.?[self.readByte()];
    }

    fn run(self: *Self) !InterpretResult {
        while (true) {
            const instruction = @intToEnum(OpCode, self.readByte());
            switch (instruction) {
                .op_constant => {
                    const constant = self.readConstant();
                    try valueMod.printValue(debug.stdout, constant);
                    try debug.stdout.writeAll("\n");
                },
                .op_return => return .ok,
            }
        }
    }
};
