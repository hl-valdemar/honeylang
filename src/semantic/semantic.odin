package semantic

import "../logger"
import "../parser"
import "../scope"

import "core:fmt"

LOG_SCOPE :: scope.Scope.semantic

EvalState :: enum {
	unevaluated,
	evaluating,
	evaluated,
}

SymbolType :: enum {
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

Semantic :: struct {
	program: ^parser.AstNode,
	symtab:  SymbolTable,
}

init :: proc(program: ^parser.AstNode) -> Semantic {
	return Semantic{program = program}
}

deinit :: proc(s: ^Semantic) {}

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

resolve_type :: proc(type_node: Maybe(^parser.TypeNode)) -> Maybe(SymbolType) {
	if type, ok := type_node.?; ok {
		#partial switch t in type {
		case parser.NamedType:
			return resolve_type_name(t.name)
		case:
			logger.fatal(LOG_SCOPE, "unsupported type in comptime declaration")
			return nil
		}
	}
	return nil
}

resolve_type_name :: proc(name: string) -> Maybe(SymbolType) {
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
		logger.fatal(LOG_SCOPE, "unknown type \"%s\" in comptime constant", name)
		return nil
	}
}

print_symtab :: proc(symtab: ^SymbolTable) {
	for symbol in symtab.symbols {
		fmt.printf("(%s) %s: %s = %v\n", symbol.kind, symbol.name, symbol.type, symbol.value)
	}
}

collect_symbols :: proc(s: ^Semantic) -> bool {
	program := s.program.(parser.Program)

	for &decl in program.declarations {
		// check for duplicates
		if symtab_lookup(&s.symtab, decl.name) != nil {
			logger.error(LOG_SCOPE, "duplicate declaration: %s", decl.name)
			return false
		}

		kind, ok := symbol_kind_from_decl(decl.kind).?
		if !ok do return false

		symbol := Symbol {
			name       = decl.name,
			kind       = kind,
			type       = resolve_type(decl.type),
			value      = nil,
			decl       = &decl,
			eval_state = .unevaluated,
		}

		append(&s.symtab.symbols, symbol)
	}

	return true
}

evaluate_constants :: proc(s: ^Semantic) -> bool {
	for &symbol in s.symtab.symbols {
		if symbol.kind == .const {
			if !evaluate_symbol(s, &symbol) do return false
		}
	}
	return true
}

evaluate_symbol :: proc(s: ^Semantic, symbol: ^Symbol) -> bool {
	switch symbol.eval_state {
	case .evaluated:
		return true

	case .unevaluated:
		// mark as evaluating
		symbol.eval_state = .evaluating

		value, ok := evaluate_expr(s, symbol.decl.value)
		if !ok do return false

		// store and mark as done
		symbol.value = value
		symbol.eval_state = .evaluated
		return true

	case .evaluating:
		// already being evaluated, cycle encountered!
		logger.fatal(
			LOG_SCOPE,
			"circular dependency detected in comptime constant: %s",
			symbol.name,
		)
		return false
	}
	return false
}

evaluate_expr :: proc(s: ^Semantic, expr: ^parser.AstNode) -> (SymbolValue, bool) {
	#partial switch node in expr {
	case parser.Literal:
		// base case
		#partial switch v in node.value {
		case bool:
			return v, true
		case i64:
			return v, true
		case f64:
			return v, true
		}

	case parser.Identifier:
		// look up the identifier in the symtab
		symbol, ok := symtab_lookup(&s.symtab, node.name).?
		if !ok {
			logger.fatal(LOG_SCOPE, "undefined identifier: %s", node.name)
			return {}, false
		}

		// recursively evaluate
		if !evaluate_symbol(s, symbol) do return {}, false

		// symbol value must not be nil at this point
		return symbol.value.?, true

	case parser.UnaryOp:
		// recursively evaluate the operand
		operand_value, ok := evaluate_expr(s, node.operand)
		if !ok do return {}, false

		// extract ComptimeValue
		comptime_val: ComptimeValue
		comptime_val, ok = operand_value.(ComptimeValue)
		if !ok {
			logger.fatal(LOG_SCOPE, "expected comptime value in unary operation")
			return {}, false
		}

		// apply the unary operation
		#partial switch node.op {
		case .negate:
			#partial switch v in comptime_val {
			case i64:
				return -v, true
			case f64:
				return -v, true
			case f32:
				return -v, true
			case:
				logger.fatal(LOG_SCOPE, "cannot negate non-numeric value")
				return {}, false
			}
		case .logical_not:
			#partial switch v in comptime_val {
			case bool:
				return !v, true
			case:
				logger.fatal(LOG_SCOPE, "cannot apply logical not to non-boolean value")
				return {}, false
			}
		}

	case:
		logger.fatal(LOG_SCOPE, "unsupported expression in constant")
		return {}, false
	}
	return {}, false
}

analyze :: proc(s: ^Semantic) -> bool {
	// first pass: collect symbols
	if !collect_symbols(s) do return false

	// second pass: evaluate comptime expressions in dependency order
	if !evaluate_constants(s) do return false

	return true
}
