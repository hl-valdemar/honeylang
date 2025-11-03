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

Scope :: scope.Scope
Token :: lexer.Token
TokenKind :: lexer.TokenKind

LOG_SCOPE :: Scope.parser

ParserError :: enum {}

Parser :: struct {
	tokens:         []Token,
	next_token_idx: int,
	ast:            ^AstNode,
}

init :: proc(tokens: []Token) -> Parser {
	return Parser{tokens = tokens, next_token_idx = 0, ast = nil}
}

deinit :: proc(p: ^Parser) {
	destroy_ast(p.ast)
}

parse_primary :: proc(p: ^Parser) -> (^AstNode, bool) {
	tok, ok := peek(p).?
	if !ok do return nil, false

	node: ^AstNode

	#partial switch tok.kind {
	case .identifier:
		node, ok = make_identifier(tok)

	case .boolean:
		node, ok = make_boolean_literal(tok)

	case:
		// unexpected token type
		logger.error(LOG_SCOPE, "unexpected token in primary expression: %v", tok.kind)
		return nil, false
	}

	advance(p)
	return node, ok
}

parse_comptime_decl :: proc(p: ^Parser) -> (^AstNode, bool) {
	// parse name
	name, ok := parse_identifier(p, "expected identifier in comptime declaration")
	if !ok do return nil, false

	// parse optional type
	type: ^Type = nil
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
	value, ok = parse_primary(p)
	if !ok do return nil, false

	// create node with all fields at once
	return make_declaration(name, type, value, .comptime), true
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
parse_type :: proc(p: ^Parser, err_msg: string) -> (^Type, bool) {
	tok, ok := peek(p).?
	if !ok do return nil, false

	if !match(p, .identifier) {
		logger.fatal(LOG_SCOPE, err_msg)
		return nil, false
	}

	node := new(Type)
	node^ = NamedType {
		name = tok.value.?,
	}
	return node, true
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
		node, ok := parse_decl(p)
		if !ok do break

		#partial switch n in node {
		case Declaration:
			append(&declarations, n)

		case:
			logger.fatal(LOG_SCOPE, "expected declaration in program")
		}
	}

	return make_program(declarations), true
}

parse :: proc(p: ^Parser) -> bool {
	program, ok := parse_program(p)
	p.ast = program
	return ok
}
