package semantic

import "../parser"

Symbol :: struct {}

SymbolTable :: struct {
	symbols: [dynamic]Symbol,
	count:   int,
}

symtab_make :: proc() -> SymbolTable {
	symbols := make([dynamic]Symbol, 0, 10)
	return SymbolTable{symbols = symbols, count = 0}
}

symtab_destroy :: proc(symtab: ^SymbolTable) {
	delete(symtab.symbols)
}

Semantic :: struct {
	program: ^parser.AstNode,
	symtab:  SymbolTable,
}

init :: proc(program: ^parser.AstNode) -> Semantic {
	return Semantic{program = program}
}

deinit :: proc(s: ^Semantic) {}

analyze :: proc(s: ^Semantic) {}
