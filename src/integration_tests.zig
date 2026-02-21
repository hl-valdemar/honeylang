// Integration tests — organized by category in integration_tests/
test {
    _ = @import("integration_tests/declarations.zig");
    _ = @import("integration_tests/functions.zig");
    _ = @import("integration_tests/arithmetic.zig");
    _ = @import("integration_tests/control_flow.zig");
    _ = @import("integration_tests/structs.zig");
    _ = @import("integration_tests/pointers.zig");
    _ = @import("integration_tests/arrays.zig");
    _ = @import("integration_tests/slices.zig");
    _ = @import("integration_tests/namespaces.zig");
    _ = @import("integration_tests/errors.zig");
    _ = @import("integration_tests/codegen.zig");
    _ = @import("integration_tests/bounds_checking.zig");
}
