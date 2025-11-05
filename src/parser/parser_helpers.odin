package parser

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

