package parser

import "../logger"

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

program_make :: proc(declarations: [dynamic]Declaration) -> ^AstNode {
	node := new(AstNode)
	node^ = Program {
		declarations = declarations,
	}
	return node
}

// create a complete declaration node
declaration_make :: proc(
	name: string,
	type: ^Type,
	value: ^AstNode,
	kind: DeclKind,
) -> ^AstNode {
	node := new(AstNode)
	node^ = Declaration {
		name  = name,
		type  = type,
		value = value,
		kind  = kind,
	}
	return node
}

identifier_make :: proc(tok: Token) -> (^AstNode, bool) {
	val, ok := tok.value.?
	if !ok {
		logger.fatal(LOG_SCOPE, "identifier has no value")
		return nil, false
	}

	node := new(AstNode)
	node^ = Identifier {
		name = val,
	}
	return node, true
}

boolean_make :: proc(tok: Token) -> (^AstNode, bool) {
	val, ok := tok.value.?
	if !ok {
		logger.fatal(LOG_SCOPE, "boolean has no value")
		return nil, false
	}

	if val != "true" && val != "false" {
		logger.fatal(LOG_SCOPE, "invalid boolean value: %s", val)
		return nil, false
	}

	node := new(AstNode)
	node^ = Literal {
		value = val == "true",
	}
	return node, true
}
