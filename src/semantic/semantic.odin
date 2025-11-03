package semantic

import "../logger"
import "../parser"
import "../scope"

import "core:fmt"

LOG_SCOPE :: scope.Scope.semantic

SymbolType :: enum {
	unknown,
	bool,

	// integers
	u8,
	u64,
	i8,
	i64,

	// floats
	f16,
	f32,
	f64,
}

SymbolValue :: union {
	PendingValue,
	ComptimeValue,
}

PendingValue :: struct {} // just a tag

ComptimeValue :: union {
	bool,
	i64, // default int type
	f32,
	f64, // default float type
}

Symbol :: struct {
	name:  string,
	kind:  SymbolKind,
	type:  SymbolType,
	value: SymbolValue,
}

SymbolKind :: enum {
	comptime,
}

SymbolTable :: struct {
	symbols: [dynamic]Symbol,
}

symtab_make :: proc() -> SymbolTable {
	symbols := make([dynamic]Symbol, 0, 10)
	return SymbolTable{symbols = symbols}
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

collect_symbols :: proc(s: ^Semantic) -> bool {
	success := true

	p := s.program.(parser.Program)
	for &decl in p.declarations {
		#partial switch decl.kind {
		case .comptime:
			success = register_comptime_decl(s, &decl)
		case:
			logger.fatal(LOG_SCOPE, "unexpected AST node, expected declaration")
			success = false
		}

		// break only if something went wrong
		if !success do return false
	}

	return success
}

analyze :: proc(s: ^Semantic) -> bool {
	// first pass: collect symbols
	if !collect_symbols(s) do return false

	// TODO: second pass - evaluate comptime expressions in dependency order

	return true
}

register_comptime_decl :: proc(s: ^Semantic, decl: ^parser.Declaration) -> bool {
	for symbol in s.symtab.symbols {
		if symbol.name == decl.name {
			logger.fatal(LOG_SCOPE, "comptime constant \"%s\" is already defined", symbol.name)
			return false
		}
	}

	symbol := Symbol {
		name  = decl.name,
		kind  = .comptime,
		type  = .unknown,
		value = PendingValue{},
	}

	if type, ok := decl.type.?; ok {
		switch t in type {
		case parser.NamedType:
			symbol.type = resolve_type_name(t.name)
			if symbol.type == .unknown {
				logger.fatal(LOG_SCOPE, "unknown type \"%s\" in comptime expression", t.name)
				return false
			}

		case parser.PointerType:
			return false
		}
	}

	append(&s.symtab.symbols, symbol)
	return true
}

resolve_type_name :: proc(name: string) -> SymbolType {
	switch name {
	case "bool":
		return .bool
	case "i64":
		return .i64
	case "f32":
		return .f32
	case "f64":
		return .f64
	case:
		return .unknown
	}
}

print_symtab :: proc(symtab: ^SymbolTable) {
	for symbol in symtab.symbols {
		fmt.printf("(%s) %s: %s = %v\n", symbol.kind, symbol.name, symbol.type, symbol.value)
	}
}
