// PARSING ORDER (from tight to loose):
// 1. primary (literals, names, parens)
// 2. unary (-, !)
// 3. multiplicative (*, /)
// 4. additive (+, -)
// 5. comparison (<, >, <=, >=, ==, !=)
// 6. logical and (&&)
// 7. logical or (||)
// 8. expression (pretty much just wraps logical or)
// 9. statement

package parser

import "../lexer"
import "../logger"
import "../scope"

Token :: lexer.Token
TokenKind :: lexer.TokenKind

LOG_SCOPE :: scope.Scope.parser

ParserError :: enum {}

Parser :: struct {
	ast:            ^AstNode,
	tokens:         []Token,
	next_token_idx: int,
}

init :: proc(tokens: []Token) -> Parser {
	return Parser{ast = nil, tokens = tokens, next_token_idx = 0}
}

deinit :: proc(p: ^Parser) {
	ast_destroy(p.ast)
}

parse_primary :: proc(p: ^Parser) -> (^AstNode, bool) {
	tok, ok := peek(p).?
	if !ok do return nil, false

	node: ^AstNode

	#partial switch tok.kind {
	case .identifier:
		node, ok = make_identifier(tok)

	case .boolean:
		node, ok = make_boolean(tok)

	case .number:
		node, ok = make_number(tok)

	case:
		// unexpected token type
		logger.error(LOG_SCOPE, "unexpected token in primary expression: %v", tok.kind)
		return nil, false
	}

	advance(p)
	return node, ok
}

parse_unary :: proc(p: ^Parser) -> (^AstNode, bool) {
	tok, ok := peek(p).?
	if !ok do return nil, false

	if check(p, .minus) || check(p, .logical_not) {
		advance(p)

		// resolve operation
		op := resolve_unary_op_kind(tok.kind).?

		// parse the operand
		operand, ok := parse_primary(p)
		if !ok {
			logger.fatal(LOG_SCOPE, "failed to parse operand in unary")
			return nil, false
		}

		return make_unary(operand, op), true
	}

	// else parse primary
	return parse_primary(p)
}

resolve_unary_op_kind :: proc(tok_kind: TokenKind) -> Maybe(UnaryOpKind) {
	#partial switch tok_kind {
	case .minus:
		return .negate
	case .logical_not:
		return .logical_not
	case:
		logger.fatal(LOG_SCOPE, "unexpected token in unary expression: %v", tok_kind)
		return nil
	}
	return nil
}

parse_multiplicative :: proc(p: ^Parser) -> (^AstNode, bool) {
	left, ok := parse_unary(p)
	if !ok {
		logger.fatal(LOG_SCOPE, "failed to parse left-hand side in binary op")
		return nil, false
	}

	// expect multiplication or division
	for check(p, .star) || check(p, .slash) {
		tok, ok := peek(p).?
		if !ok do break

		op := resolve_binary_op_kind(tok.kind).?
		advance(p)

		right: ^AstNode
		right, ok = parse_unary(p)
		if !ok {
			logger.fatal(LOG_SCOPE, "failed to parse right-hand side in binary op")
			ast_destroy(left)
			return nil, false
		}

		left = make_binary(left, right, op)
	}

	return left, true
}

resolve_binary_op_kind :: proc(tok_kind: TokenKind) -> Maybe(BinaryOpKind) {
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
		logger.fatal(LOG_SCOPE, "unexpected token in unary expression: %v", tok_kind)
		return nil
	}
	return nil
}

parse_expr :: proc(p: ^Parser) -> (^AstNode, bool) {
	return parse_multiplicative(p)
}

parse_comptime_decl :: proc(p: ^Parser) -> (^AstNode, bool) {
	// expect name
	name, ok := parse_identifier(p, "expected identifier in comptime declaration")
	if !ok do return nil, false

	// parse optional type
	type: ^TypeNode = nil
	if match(p, .colon) {
		type, ok = parse_type(p, "expected type in comptime declaration")
		if !ok do return nil, false
	}

	// parse ::
	if !match(p, .double_colon) {
		logger.fatal(LOG_SCOPE, "expected \"::\" in comptime declaration")
		return nil, false
	}

	// parse value
	value: ^AstNode
	value, ok = parse_expr(p)
	if !ok do return nil, false

	// create node with all fields at once
	return make_declaration(name, type, value, .const), true
}

// expect identifier and return its value
parse_identifier :: proc(p: ^Parser, err_msg: string) -> (string, bool) {
	tok, ok := peek(p).?
	if !ok do return {}, false

	if !match(p, .identifier) {
		logger.fatal(LOG_SCOPE, err_msg)
		return {}, false
	}

	return tok.value.?, true
}

// parse a type node
parse_type :: proc(p: ^Parser, err_msg: string) -> (^TypeNode, bool) {
	tok, ok := peek(p).?
	if !ok do return nil, false

	if !match(p, .identifier) {
		logger.fatal(LOG_SCOPE, err_msg)
		return nil, false
	}

	return make_type(tok)
}

parse_decl :: proc(p: ^Parser) -> (^AstNode, bool) {
	if check(p, .identifier) && (check_offset(p, 1, .colon) || check_offset(p, 1, .double_colon)) {
		return parse_comptime_decl(p)
	}
	return nil, false
}

parse_program :: proc(p: ^Parser) -> (^AstNode, bool) {
	declarations := make([dynamic]Declaration)

	for {
		if _, ok := peek(p).?; !ok {
			break // successfully parsed all tokens
		}

		node, ok := parse_decl(p)
		if !ok {
			logger.error(LOG_SCOPE, "failed to parse declaration")
			return nil, false
		}

		#partial switch n in node {
		case Declaration:
			append(&declarations, n)

		case:
			logger.fatal(LOG_SCOPE, "expected declaration in program")
			return nil, false
		}
	}

	return make_program(declarations), true
}

parse :: proc(p: ^Parser) -> bool {
	program, ok := parse_program(p)
	p.ast = program
	return ok
}
