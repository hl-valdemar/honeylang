const std = @import("std");

var enabled: bool = true;

pub fn init() void {
    // respect no_color environment variable (https://no-color.org/)
    if (std.posix.getenv("NO_COLOR")) |_| {
        enabled = false;
        return;
    }

    // respect force_color for ci environments
    if (std.posix.getenv("FORCE_COLOR")) |_| {
        enabled = true;
        return;
    }

    // check if stderr is a tty
    enabled = std.posix.isatty(std.posix.STDERR_FILENO);
}

pub fn setEnabled(value: bool) void {
    enabled = value;
}

// convenience functions
pub fn reset() []const u8 {
    return if (enabled) "\x1b[0m" else "";
}
pub fn bold() []const u8 {
    return if (enabled) "\x1b[1m" else "";
}
pub fn dim() []const u8 {
    return if (enabled) "\x1b[2m" else "";
}
pub fn red() []const u8 {
    return if (enabled) "\x1b[31m" else "";
}
pub fn green() []const u8 {
    return if (enabled) "\x1b[32m" else "";
}
pub fn yellow() []const u8 {
    return if (enabled) "\x1b[33m" else "";
}
pub fn blue() []const u8 {
    return if (enabled) "\x1b[34m" else "";
}
pub fn magenta() []const u8 {
    return if (enabled) "\x1b[35m" else "";
}
pub fn cyan() []const u8 {
    return if (enabled) "\x1b[36m" else "";
}
