// PARSING ORDER (from tight coupling to loose):
// 1. primary (literals, names, parens)
// 2. unary (-, not)
// 3. multiplicative (*, /)
// 4. additive (+, -)
// 5. comparison (<, >, <=, >=, ==, !=)
// 6. logical and
// 7. logical or
// 8. expression (pretty much just wraps logical or)
// 9. statement

package parser

import "../error"
import "../lexer"
import "../scope"

import "core:fmt"

Token :: lexer.Token
TokenKind :: lexer.TokenKind

LOG_SCOPE :: scope.Scope.parser

ParserError :: enum {}

Parser :: struct {
	ast:            ^AstNode,
	tokens:         []Token,
	next_token_idx: int,
	errors:         ^error.ErrorList,
}

init :: proc(tokens: []Token, errors: ^error.ErrorList) -> Parser {
	return Parser{ast = nil, tokens = tokens, next_token_idx = 0, errors = errors}
}

deinit :: proc(p: ^Parser) {
	ast_destroy(p.ast)
}

parse_primary :: proc(p: ^Parser) -> (^AstNode, bool) {
	tok, ok := peek(p).?
	if !ok {
		report_error(
			p,
			.parser_expected_primary_expr,
			fmt.tprintf("expected expression, found unexpected end of file"),
		)
		return {}, false
	}

	node: ^AstNode

	#partial switch tok.kind {
	case .identifier:
		node, ok = make_identifier(tok)

	case .boolean:
		node, ok = make_boolean(tok)

	case .number:
		node, ok = make_number(tok)

	case .left_paren:
		advance(p) // consume the left parenthesis
		node, ok = parse_expr(p)

	case:
		// unexpected token type
		report_error(p, .parser_unexpected_token, "unexpected token '%v' in expression", tok.kind)
		return nil, false
	}

	advance(p)
	return node, ok
}

parse_unary :: proc(p: ^Parser) -> (^AstNode, bool) {
	tok, ok := peek(p).?
	if !ok {
		report_error(
			p,
			.parser_expected_unary_expr,
			fmt.tprintf("expected expression, found unexpected end of file"),
		)
		return {}, false
	}

	if check(p, .minus) || check(p, .logical_not) {
		advance(p)

		// resolve operation
		op := resolve_unary_op_kind(p, tok.kind).?
		if !ok do return nil, false // error already reported in resolve_unary_op_kind

		// recursively call parse_unary to allow chaining
		operand, ok := parse_unary(p)
		if !ok do return nil, false

		return make_unary(operand, op), true
	}

	// else parse primary
	return parse_primary(p)
}

parse_multiplicative :: proc(p: ^Parser) -> (^AstNode, bool) {
	left, ok := parse_unary(p)
	if !ok do return nil, false // error already reported in parse_unary

	// expect multiplication or division
	for check(p, .star) || check(p, .slash) {
		tok, ok := peek(p).?
		if !ok do break

		op := resolve_binary_op_kind(p, tok.kind).?
		advance(p)

		right: ^AstNode
		right, ok = parse_unary(p)
		if !ok {
			// error already reported in parse_unary
			ast_destroy(left)
			return nil, false
		}

		left = make_binary(left, right, op)
	}

	return left, true
}

parse_additive :: proc(p: ^Parser) -> (^AstNode, bool) {
	left, ok := parse_multiplicative(p)
	if !ok do return nil, false // error already reported in parse_multiplicative

	// expect addition or subtraction
	for check(p, .plus) || check(p, .minus) {
		tok, ok := peek(p).?
		if !ok do break

		op := resolve_binary_op_kind(p, tok.kind).?
		advance(p)

		right: ^AstNode
		right, ok = parse_multiplicative(p)
		if !ok {
			// error already reported in parse_multiplicative
			ast_destroy(left)
			return nil, false
		}

		left = make_binary(left, right, op)
	}

	return left, true
}

parse_comparative :: proc(p: ^Parser) -> (^AstNode, bool) {
	left, ok := parse_additive(p)
	if !ok do return nil, false // error already reported in parse_additive

	// expect comparison
	for check(p, .double_equal) ||
	    check(p, .not_equal) ||
	    check(p, .less) ||
	    check(p, .greater) ||
	    check(p, .less_equal) ||
	    check(p, .greater_equal) {
		tok, ok := peek(p).?
		if !ok do break

		op, ok_op := resolve_binary_op_kind(p, tok.kind).?
		if !ok_op do return nil, false // error reported in resolve_binary_op_kind

		advance(p)

		right: ^AstNode
		right, ok = parse_additive(p)
		if !ok {
			// error already reported in parse_additive
			ast_destroy(left)
			return nil, false
		}

		left = make_binary(left, right, op)
	}

	return left, true
}

parse_logical_and :: proc(p: ^Parser) -> (^AstNode, bool) {
	left, ok := parse_comparative(p)
	if !ok do return nil, false // error already reported in parse_comparative

	// expect only logical and
	for check(p, .logical_and) {
		tok, ok := peek(p).?
		if !ok do break

		op, ok_op := resolve_binary_op_kind(p, tok.kind).?
		if !ok_op do return nil, false // error reported in resolve_binary_op_kind

		advance(p)

		right: ^AstNode
		right, ok = parse_comparative(p)
		if !ok {
			// error already reported in parse_comparative
			ast_destroy(left)
			return nil, false
		}

		left = make_binary(left, right, op)
	}

	return left, true
}

parse_logical_or :: proc(p: ^Parser) -> (^AstNode, bool) {
	left, ok := parse_logical_and(p)
	if !ok do return nil, false // error already reported in parse_logical_and

	// expect only logical or
	for check(p, .logical_or) {
		tok, ok := peek(p).?
		if !ok do break

		op, ok_op := resolve_binary_op_kind(p, tok.kind).?
		if !ok_op do return nil, false // error reported in resolve_binary_op_kind

		advance(p)

		right: ^AstNode
		right, ok = parse_logical_and(p)
		if !ok {
			// error already reported in parse_logical_and
			ast_destroy(left)
			return nil, false
		}

		left = make_binary(left, right, op)
	}

	return left, true
}

parse_expr :: proc(p: ^Parser) -> (^AstNode, bool) {
	return parse_logical_or(p)
}

parse_comptime_decl :: proc(p: ^Parser) -> (^AstNode, bool) {
	// expect name
	name, ok := parse_identifier(p, "expected identifier in comptime declaration")
	if !ok do return nil, false // error reported by parse_identifier

	// parse optional type
	type: ^TypeNode = nil
	if match(p, .colon) {
		type, ok = parse_type(p, "expected type in comptime declaration")
		if !ok do return nil, false // error reported by parse_type
	}

	// parse ::
	tok, ok_tok := peek(p).?
	if !match(p, .double_colon) {
		if ok_tok {
			report_error(
				p,
				.parser_unexpected_token,
				"expected '::' in comptime declaration, found '%v'",
				tok.kind,
			)
		} else {
			report_error(
				p,
				.parser_unexpected_token,
				"expected '::' in comptime declaration, found unexpected end of file",
			)
		}
		return nil, false
	}

	// parse value
	value: ^AstNode
	value, ok = parse_expr(p)
	if !ok {
		report_error(
			p,
			.parser_unexpected_token,
			"expected expression after '::' in comptime declaration",
		)
		return nil, false
	}

	// create node with all fields at once
	return make_declaration(name, type, value, .const), true
}

// expect identifier and return its value
parse_identifier :: proc(p: ^Parser, err_msg: string) -> (string, bool) {
	tok, ok := peek(p).?
	if !ok {
		report_error(
			p,
			.parser_expected_identifier,
			fmt.tprintf("%s, found unexpected end of file", err_msg),
		)
		return {}, false
	}

	if !match(p, .identifier) {
		report_error(
			p,
			.parser_expected_identifier,
			fmt.tprintf("%s, found '%v'", err_msg, tok.kind),
		)
		return {}, false
	}

	return tok.value.?, true
}

// parse a type node
parse_type :: proc(p: ^Parser, err_msg: string) -> (^TypeNode, bool) {
	tok, ok := peek(p).?
	if !ok {
		report_error(
			p,
			.parser_expected_type,
			fmt.tprintf("%s, found unexpected end of file", err_msg),
		)
		return {}, false
	}

	if !match(p, .identifier) {
		report_error(p, .parser_expected_type, fmt.tprintf("%s, found '%v'", err_msg, tok.kind))
		return nil, false
	}

	return make_type(tok)
}

parse_decl :: proc(p: ^Parser) -> (^AstNode, bool) {
	if check(p, .identifier) && (check_offset(p, 1, .colon) || check_offset(p, 1, .double_colon)) {
		return parse_comptime_decl(p)
	}

	// failed to parse declaration
	tok, ok := peek(p).?
	if ok {
		report_error(p, .parser_unexpected_token, "expected declaration, found '%v'", tok.kind)
	} else {
		report_error(
			p,
			.parser_unexpected_eof,
			"expected declaration, found unexpected end of file",
		)
	}

	return nil, false
}

parse_program :: proc(p: ^Parser) -> (^AstNode, bool) {
	declarations := make([dynamic]Declaration)

	for {
		if tok, ok := peek(p).?; tok.kind == .eof {
			advance(p) // consume for good measure
			break // successfully parsed all tokens
		} else if !ok {
			report_error(p, .parser_unexpected_eof, "unexpected end of file")
			return {}, false
		}

		node, ok := parse_decl(p)
		if !ok {
			return {}, false
			// // error already reported, try to recover by skipping to next declaration
			// synchronize(p)
			// continue
		}

		#partial switch n in node {
		case Declaration:
			append(&declarations, n)

		case:
			report_error(p, .parser_unexpected_token, "expected declaration in program")
			return {}, false
		// synchronize(p)
		// continue
		}
	}

	return make_program(declarations), true
}

parse :: proc(p: ^Parser) -> bool {
	program, ok := parse_program(p)
	p.ast = program
	return ok
}
