package semantic

import "../logger"
import "../parser"

import "core:fmt"

EvalState :: enum {
	unevaluated,
	evaluating,
	evaluated,
}

SymbolType :: enum {
	bool,

	// unsigned integers
	u8,
	u16,
	u32,
	u64,

	// signed integers
	i8,
	i16,
	i32,
	i64,

	// floats
	f16,
	f32,
	f64,
}

SymbolValue :: union {
	ComptimeValue,
}

PendingValue :: struct {} // just a tag

ComptimeValue :: union {
	bool,

	// unsigned integers
	u8,
	u16,
	u32,
	u64,

	// signed integers
	i8,
	i16,
	i32, // default int type
	i64,

	// floats
	f16,
	f32, // default float type
	f64,
}

Symbol :: struct {
	name:       string,
	kind:       SymbolKind,
	type:       Maybe(SymbolType),
	value:      Maybe(SymbolValue),
	decl:       ^parser.Declaration,
	eval_state: EvalState,
}

SymbolKind :: enum {
	const,
	func,
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

symtab_lookup :: proc(s: ^SymbolTable, name: string) -> Maybe(^Symbol) {
	for &symbol in s.symbols {
		if symbol.name == name do return &symbol
	}
	return nil
}

symbol_kind_from_decl :: proc(kind: parser.DeclKind) -> Maybe(SymbolKind) {
	#partial switch kind {
	case .const:
		return .const
	case .func:
		return .func
	case:
		logger.fatal(LOG_SCOPE, "unexpected const declaration kind")
		return nil
	}
}

print_symtab :: proc(symtab: ^SymbolTable) {
	for symbol in symtab.symbols {
		fmt.printf(
			"[%s%s%s] %s%s%s: %s%s%s = %s%v%s\n",
			logger.color_codes[.red],
			symbol.kind,
			logger.color_codes[.reset],
			logger.color_codes[.cyan],
			symbol.name,
			logger.color_codes[.reset],
			logger.color_codes[.blue],
			symbol.type,
			logger.color_codes[.reset],
			logger.color_codes[.yellow],
			symbol.value,
			logger.color_codes[.reset],
		)
	}
}
