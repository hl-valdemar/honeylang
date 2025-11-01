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
}

init :: proc(tokens: []Token) -> Parser {
	return Parser{tokens = tokens, next_token_idx = 0}
}

deinit :: proc(p: ^Parser) {}

peek :: proc(p: ^Parser) -> Maybe(Token) {
	if p.next_token_idx < len(p.tokens) {
		return p.tokens[p.next_token_idx]
	}
	return nil
}

parse_primary :: proc(p: ^Parser) -> (^AstNode, bool) {
	tok, ok := peek(p).?
	if !ok do return {}, false

	node := new(AstNode)

	// TODO: remove the `#partial` directive
	#partial switch tok.kind {
	case .identifier:
		val, ok := tok.value.?
		if !ok {
			logger.fatal(LOG_SCOPE, "identifier has no name: %s", tok.kind)
			return {}, false
		}

		node^ = Identifier {
			name = val,
		}
	}

	return node, true
}

// parse_stmt :: proc(p: ^Parser) -> (^AstNode, bool) {
// }

parse :: proc(p: ^Parser) -> (^AstNode, bool) {
	node, ok := parse_primary(p)
	return node, ok
}
