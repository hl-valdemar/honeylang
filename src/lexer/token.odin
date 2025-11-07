package lexer

import "../error"
import "../logger"

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
			"%s:%s%v%s:%s%v%s %s%s%s = %s%s%s",
			tok.loc.file,
      logger.color_codes[.blue],
			tok.loc.line,
      logger.color_codes[.reset],
      logger.color_codes[.blue],
			tok.loc.column,
      logger.color_codes[.reset],
      logger.color_codes[.cyan],
			tok.kind,
      logger.color_codes[.reset],
      logger.color_codes[.yellow],
			val,
      logger.color_codes[.reset],
		)
	} else {
		return fmt.tprintf(
			"%s:%s%v%s:%s%v%s %s%s%s",
			tok.loc.file,
      logger.color_codes[.blue],
			tok.loc.line,
      logger.color_codes[.reset],
      logger.color_codes[.blue],
			tok.loc.column,
      logger.color_codes[.reset],
      logger.color_codes[.cyan],
			tok.kind,
      logger.color_codes[.reset],
		)
	}
}
