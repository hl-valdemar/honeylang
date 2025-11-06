package parser

import "../error"

peek_offset :: proc(p: ^Parser, offset: int) -> Maybe(Token) {
	if p.next_token_idx + offset < len(p.tokens) {
		return p.tokens[p.next_token_idx + offset]
	}
	return nil
}

peek :: proc(p: ^Parser) -> Maybe(Token) {
	return peek_offset(p, 0)
}

check :: proc(p: ^Parser, tok_kind: TokenKind) -> bool {
	tok, ok := peek(p).?
	return ok && tok.kind == tok_kind
}

check_offset :: proc(p: ^Parser, offset: int, tok_kind: TokenKind) -> bool {
	tok, ok := peek_offset(p, offset).?
	return ok && tok.kind == tok_kind
}

match :: proc(p: ^Parser, tok_kind: TokenKind) -> bool {
	if check(p, tok_kind) {
		advance(p)
		return true
	}
	return false
}

advance :: proc(p: ^Parser) {
	p.next_token_idx += 1
}

report_error :: proc(p: ^Parser, kind: error.ErrorKind, fmt_str: string, args: ..any) {
	tok, ok := peek(p).?
	loc: Maybe(error.SourceLocation) = nil

	if ok {
		loc = tok.loc
	}

	error.add_error(p.errors, kind, loc, fmt_str, ..args)
}

report_error_at_token :: proc(
	p: ^Parser,
	tok: Token,
	kind: error.ErrorKind,
	fmt_str: string,
	args: ..any,
) {
	error.add_error(p.errors, kind, tok.loc, fmt_str, ..args)
}

// panic-mode recovery: skip tokens until we find a synchronization point
synchronize :: proc(p: ^Parser) {
	for {
		tok, ok := peek(p).?
		if !ok do return // end of file

		// look for start of a new declaration (identifier followed by : or ::)
		if tok.kind == .identifier {
			if check_offset(p, 1, .colon) || check_offset(p, 1, .double_colon) {
				return // found what looks like a new declaration
			}
		}

		advance(p)
	}
}

resolve_unary_op_kind :: proc(p: ^Parser, tok_kind: TokenKind) -> Maybe(UnaryOpKind) {
	#partial switch tok_kind {
	case .minus:
		return .negate
	case .logical_not:
		return .logical_not
	case:
		report_error(
			p,
			.parser_unexpected_token,
			"unexpected token '%v' in unary expression",
			tok_kind,
		)
		return nil
	}
	return nil
}

resolve_binary_op_kind :: proc(p: ^Parser, tok_kind: TokenKind) -> Maybe(BinaryOpKind) {
	#partial switch tok_kind {
	// arithmetic
	case .plus:
		return .add
	case .minus:
		return .sub
	case .star:
		return .mul
	case .slash:
		return .div

	// logical
	case .logical_and:
		return .logical_and
	case .logical_or:
		return .logical_or

	// comparative
	case .less:
		return .less
	case .greater:
		return .greater
	case .less_equal:
		return .less_equal
	case .greater_equal:
		return .greater_equal

	case:
		report_error(
			p,
			.parser_unexpected_token,
			"unexpected token '%v' in binary expression",
			tok_kind,
		)
		return nil
	}
	return nil
}
