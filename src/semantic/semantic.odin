package semantic

import "../parser"

Semantic :: struct {
	ast: ^parser.AstNode,
}

init :: proc(ast: ^parser.AstNode) -> Semantic {
	return Semantic{ast = ast}
}

deinit :: proc(s: ^Semantic) {}

analyze :: proc(s: ^Semantic) {}
