package logger

import "../scope"

import "base:intrinsics"
import "core:fmt"
import "core:os"
import "core:strings"
import "core:time"

Scope :: scope.Scope

// = = = = = = = = = = = =
// Logger Utilities
// = = = = = = = = = = = =

Logger :: struct {
	file_handle:  os.Handle,
	file_enabled: bool,
	term_enabled: bool,
}

LoggerDesc :: struct {
	file_enabled: bool,
	term_enabled: bool,
}

default_logger := Logger {
	file_handle  = os.INVALID_HANDLE,
	file_enabled = false,
	term_enabled = true,
}

logger := default_logger

// ansi color codes
Color :: enum {
	reset,
	red,
	green,
	yellow,
	blue,
	cyan,
	gray,
	bright_red,
	dark_white,
}

color_codes := [Color]string {
	.reset      = "\x1b[0m",
	.red        = "\x1b[31m",
	.green      = "\x1b[32m",
	.yellow     = "\x1b[33m",
	.blue       = "\x1b[34m",
	.cyan       = "\x1b[36m",
	.dark_white = "\x1b[37m",
	.gray       = "\x1b[90m",
	.bright_red = "\x1b[91m",
}

level_colors := [Level]Color {
	.fatal = .bright_red,
	.error = .red,
	.warn  = .yellow,
	.info  = .blue,
	.debug = .green,
}

print_field :: proc(s: string, col_width: int) {
	fmt.printf("%s", s)
	pad := col_width - len(s)
	if pad > 0 {
		for _ in 0 ..< pad do fmt.printf(" ")
	}
}

write_field :: proc(handle: os.Handle, s: string, col_width: int) {
	os.write_string(handle, s)
	pad := col_width - len(s)
	if pad > 0 {
		for _ in 0 ..< pad {
			os.write_string(handle, " ")
		}
	}
}

level_max_width :: proc() -> int {
	max := 0
	for level in Level {
		if max < len(fmt.tprint(level)) {
			max = len(fmt.tprint(level))
		}
	}
	return max
}

scope_max_width :: proc() -> int {
	max := 0
	for scope in Scope {
		if max < len(fmt.tprint(scope)) {
			max = len(fmt.tprint(scope))
		}
	}
	return max
}

// enable file logging
enable_file_logging :: proc(filepath: string) -> bool {
	handle, err := os.open(filepath, os.O_CREATE | os.O_WRONLY | os.O_APPEND, 0o644)
	if err != os.ERROR_NONE {
		error(Scope.lexer, "failed to open log file:", filepath)
		return false
	}

	logger.file_handle = handle
	logger.file_enabled = true
	return true
}

disable_file_logging :: proc() {
	if logger.file_enabled && logger.file_handle != os.INVALID_HANDLE {
		os.close(logger.file_handle)
		logger.file_handle = os.INVALID_HANDLE
	}
	logger.file_enabled = false
}

set_term_logging :: proc(enabled: bool) {
	logger.term_enabled = enabled
}

// = = = = = = = = = = = =
// Logger Implementation
// = = = = = = = = = = = =

Level :: enum {
	fatal,
	error,
	warn,
	info,
	debug,
}

init :: proc(desc: LoggerDesc) {
	logger = Logger {
		file_handle  = os.INVALID_HANDLE,
		file_enabled = desc.file_enabled,
		term_enabled = desc.term_enabled,
	}
}

deinit :: proc() {
	if logger.file_enabled && logger.file_handle != os.INVALID_HANDLE {
		os.close(logger.file_handle)
	}
}

log :: proc(scope: string, level: Level, fmt_str: string, args: ..any) {
	msg := fmt.tprintf(fmt_str, ..args)

	// full bracketed field strings
	level_field := fmt.tprintf("[%s]", level)
	scope_field := fmt.tprintf("(%s)", scope)

	// set target column widths
	level_col := level_max_width() + len("[]") + 1
	scope_col := scope_max_width() + len("()") + 2

	// output to terminal
	if logger.term_enabled {
		level_color := level_colors[level]

		fmt.printf("%s", color_codes[level_color])
		print_field(level_field, level_col)
		fmt.printf("%s", color_codes[.reset])

		fmt.printf("%s", color_codes[.gray])
		print_field(scope_field, scope_col)
		fmt.printf("%s", color_codes[.reset])
		fmt.printf("%s\n", msg)
	}

	// output to file (no colors)
	if logger.file_enabled && logger.file_handle != os.INVALID_HANDLE {
		time_field := fmt.tprint(time.now())

		time_field = strings.join(strings.split(time_field, " ")[:2], " ") // keep only date and time
		time_field = strings.join(strings.split(time_field, ".")[:1], " ") // snip off milliseconds
		time_field = fmt.tprintf("%s *", time_field)
		time_col := len(time_field) + 1

		write_field(logger.file_handle, time_field, time_col)
		write_field(logger.file_handle, level_field, level_col)
		write_field(logger.file_handle, scope_field, scope_col)
		os.write_string(logger.file_handle, msg)
		os.write_string(logger.file_handle, "\n")
	}
}

fatal :: proc(scope: $T, fmt_str: string, args: ..any) where intrinsics.type_is_enum(T) {
	log(fmt.tprint(scope), .fatal, fmt_str, ..args)
}

error :: proc(scope: $T, fmt_str: string, args: ..any) where intrinsics.type_is_enum(T) {
	log(fmt.tprint(scope), .error, fmt_str, ..args)
}

warn :: proc(scope: $T, fmt_str: string, args: ..any) where intrinsics.type_is_enum(T) {
	log(fmt.tprint(scope), .warn, fmt_str, ..args)
}

info :: proc(scope: $T, fmt_str: string, args: ..any) where intrinsics.type_is_enum(T) {
	log(fmt.tprint(scope), .info, fmt_str, ..args)
}

debug :: proc(scope: $T, fmt_str: string, args: ..any) where intrinsics.type_is_enum(T) {
	when ODIN_DEBUG do log(fmt.tprint(scope), .debug, fmt_str, ..args)
}
