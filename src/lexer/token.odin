package lexer

import "core:fmt"

TokenKind :: enum {
	// arithmetic operators
	plus,
	minus,
	star,
	slash,

	// assignment operators
	double_colon,
	colon,
	equal,

	// comparison operators
	double_equal,
	less,
	greater,
	less_equal,
	greater_equal,

	// logical operators
	logical_and,
	logical_or,
	logical_not,

	// bit operators
	bit_and,
	bit_or,
	bit_not,
	bit_shift_left,
	bit_shift_right,

	// value types
	number,
	identifier,
	boolean,

	// keywords
	func,
	if_,
	else_,

	// enclosing operators
	left_paren,
	right_paren,
	left_curly_bracket,
	right_curly_bracket,
	left_square_bracket,
	right_square_bracket,
}

Token :: struct {
	kind:  TokenKind,
	value: Maybe([]rune),
}

token_to_string :: proc(tok: Token) -> string {
	if val, ok := tok.value.?; ok {
		return fmt.tprintf("%s: \"%s\"", tok.kind, val)
	} else {
		return fmt.tprintf("%s", tok.kind)
	}
}
