pub fn add(comptime T: type, a: T, b: T) T {
    return a + b;
}

pub fn sub(comptime T: type, a: T, b: T) T {
    return a - b;
}

pub fn mul(comptime T: type, a: T, b: T) T {
    return a * b;
}

pub fn div(comptime T: type, a: T, b: T) T {
    return a / b;
}
