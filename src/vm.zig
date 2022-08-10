const chunkMod = @import("chunk.zig");
const Chunk = chunkMod.Chunk;
const OpCode = chunkMod.OpCode;
const compiler = @import("compiler.zig");
const valueMod = @import("value.zig");
const Value = valueMod.Value;
const debug = @import("debug.zig");
const common = @import("common.zig");
const math = @import("math.zig");

const stdout = debug.stdout;

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

pub const VM = struct {
    const stackMax = 256;

    chunk: ?*Chunk,
    ip: usize,
    stack: [stackMax]Value,
    stackTop: usize,

    const Self = @This();

    pub fn init() Self {
        return Self{
            .chunk = null,
            .ip = 0,
            .stack = undefined,
            .stackTop = 0,
        };
    }

    fn resetStack(self: *Self) void {
        self.stackTop = 0;
    }

    fn push(self: *Self, value: Value) void {
        self.stack[self.stackTop] = value;
        self.stackTop += 1;
    }

    fn pop(self: *Self) Value {
        self.stackTop -= 1;
        return self.stack[self.stackTop];
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn interpret(self: *Self, source: []const u8) !InterpretResult {
        _ = self;
        try compiler.compile(source);
        return .ok;
    }

    fn readByte(self: *Self) u8 {
        const b = self.chunk.?.code.?[self.ip];
        self.ip += 1;
        return b;
    }

    fn readConstant(self: *Self) Value {
        return self.chunk.?.constants.values.?[self.readByte()];
    }

    fn binaryOp(self: *Self, comptime f: fn (comptime T: type, Value, Value) Value) void {
        const b = self.pop();
        const a = self.pop();
        self.push(f(Value, a, b));
    }

    fn run(self: *Self) !InterpretResult {
        while (true) {
            if (common.debugTraceExecution) {
                try stdout.writeAll("          ");
                var slot: usize = 0;
                while (slot < self.stackTop) : (slot += 1) {
                    try stdout.writeAll("[ ");
                    try valueMod.printValue(stdout, self.stack[slot]);
                    try stdout.writeAll(" ]");
                }
                try stdout.writeAll("\n");
                _ = try debug.disassembleInstruction(self.chunk.?, self.ip);
            }
            const instruction = @intToEnum(OpCode, self.readByte());
            switch (instruction) {
                .op_constant => {
                    const constant = self.readConstant();
                    self.push(constant);
                },
                .op_negate => self.push(-self.pop()),
                .op_add => self.binaryOp(math.add),
                .op_subtract => self.binaryOp(math.sub),
                .op_multiply => self.binaryOp(math.mul),
                .op_divide => self.binaryOp(math.div),
                .op_return => {
                    try valueMod.printValue(stdout, self.pop());
                    try stdout.writeAll("\n");
                    return .ok;
                },
            }
        }
    }
};
