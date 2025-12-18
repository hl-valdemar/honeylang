pub const TypeId = enum {
    // type could not be determined - will trap at runtime
    unresolved,

    // primitives
    void,
    bool,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    f16,
    f32,
    f64,

    pub fn isInteger(self: TypeId) bool {
        return switch (self) {
            .u8, .u16, .u32, .u64, .i8, .i16, .i32, .i64 => true,
            else => false,
        };
    }

    pub fn isFloat(self: TypeId) bool {
        return switch (self) {
            .f16, .f32, .f64 => true,
            else => false,
        };
    }

    pub fn isNumeric(self: TypeId) bool {
        return self.isInteger() or self.isFloat();
    }

    pub fn isSignedInteger(self: TypeId) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64 => true,
            else => false,
        };
    }
};

pub const TypeState = enum(u8) {
    /// Awaiting inference from context.
    pending,

    /// Type is known (use getResolved() on symbol table).
    resolved,
};
