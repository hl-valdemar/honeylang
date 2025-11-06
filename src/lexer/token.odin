package lexer

import "../error"
import "core:fmt"

TokenKind :: enum {
	// arithmetic operators
	plus,
	minus,
	star,
	slash,

	// assignment operators
	colon,
	double_colon,
  equal,

	// comparison operators
  not_equal,
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

	// other
	eof,
}

Token :: struct {
	kind:  TokenKind,
	value: Maybe(string),
	loc:   error.SourceLocation,
}

token_to_string :: proc(tok: Token) -> string {
	if val, ok := tok.value.?; ok {
		return fmt.tprintf(
			"%s:%v:%v %s = \"%s\"",
			tok.loc.file,
			tok.loc.line,
			tok.loc.column,
			tok.kind,
			val,
		)
	} else {
		return fmt.tprintf("%s:%v:%v %s", tok.loc.file, tok.loc.line, tok.loc.column, tok.kind)
	}
}
