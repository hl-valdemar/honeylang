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

	// signed integers
	case "u8":
		return .u8
	case "u16":
		return .u16
	case "u32":
		return .u32
	case "u64":
		return .u64

	// signed integers
	case "i8":
		return .i8
	case "i16":
		return .i16
	case "i32":
		return .i32
	case "i64":
		return .i64

	// floats
	case "f16":
		return .f16
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

		value, ok := evaluate_expr(s, symbol.decl.value, symbol.type)
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

evaluate_expr :: proc(
	s: ^Semantic,
	expr: ^parser.AstNode,
	context_type: Maybe(SymbolType) = nil,
) -> (
	SymbolValue,
	bool,
) {
	#partial switch node in expr {
	case parser.Literal:
		// base case
		#partial switch v in node.value {
		case bool:
			return v, true
		case u64:
			return v, true
		case i64:
			return v, true
		case f64:
			return v, true

		case:
			logger.fatal(LOG_SCOPE, "unexpected data type from parsing stage when parsing literal")
			return {}, false
		}

	case parser.Identifier:
		// look up the identifier in the symtab
		symbol, ok := symtab_lookup(&s.symtab, node.name).?
		if !ok {
			logger.fatal(LOG_SCOPE, "undefined identifier: %s", node.name)
			return {}, false
		}

		// infer type from context
		if context_type != nil && symbol.type == nil {
			symbol.type = context_type
		}

		// recursively evaluate
		if !evaluate_symbol(s, symbol) do return {}, false

		// symbol value must not be nil at this point
		return symbol.value.?, true

	case parser.UnaryOp:
		// recursively evaluate the operand
		operand_val, ok := evaluate_expr(s, node.operand, context_type)
		if !ok do return {}, false

		// extract ComptimeValue
		comptime_val: ComptimeValue
		comptime_val, ok = operand_val.(ComptimeValue)
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

	case parser.BinaryOp:
		// recursively evaluate both operands
		left_val, ok := evaluate_expr(s, node.left, context_type)
		if !ok do return {}, false

		right_val: SymbolValue
		right_val, ok = evaluate_expr(s, node.right, context_type)
		if !ok do return {}, false

		// extract ComptimeValues
		left_ct, ok_left := left_val.(ComptimeValue)
		right_ct, ok_right := right_val.(ComptimeValue)
		if !ok_left || !ok_right {
			logger.fatal(LOG_SCOPE, "expected comptime values in binary operation")
			return {}, false
		}

		// match on left type, ensure right matches
		switch l in left_ct {
		case u8:
			r, ok := right_ct.(u8)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_u8(l, r, node.op)

		case u16:
			r, ok := right_ct.(u16)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_u16(l, r, node.op)

		case u32:
			r, ok := right_ct.(u32)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_u32(l, r, node.op)

		case u64:
			r, ok := right_ct.(u64)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_u64(l, r, node.op)

		case i8:
			r, ok := right_ct.(i8)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_i8(l, r, node.op)

		case i16:
			r, ok := right_ct.(i16)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_i16(l, r, node.op)

		case i32:
			r, ok := right_ct.(i32)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_i32(l, r, node.op)

		case i64:
			r, ok := right_ct.(i64)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_i64(l, r, node.op)

		case f16:
			r, ok := right_ct.(f16)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_f16(l, r, node.op)

		case f32:
			r, ok := right_ct.(f32)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_f32(l, r, node.op)

		case f64:
			r, ok := right_ct.(f64)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_f64(l, r, node.op)

		case bool:
			r, ok := right_ct.(bool)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_bool(l, r, node.op)
		}

	case:
		logger.fatal(LOG_SCOPE, "unsupported expression in constant")
		return {}, false
	}

	return {}, false
}

apply_default_types :: proc(s: ^Semantic) -> bool {
	for &symbol in s.symtab.symbols {
		if symbol.kind != .const do continue
		if symbol.type != nil do continue // already has a type

		value, ok := symbol.value.?
		if !ok {
			logger.fatal(LOG_SCOPE, "symbol has no value: %s", symbol.name)
			return false
		}

		comptime_val, ok_ct := value.(ComptimeValue)
		if !ok_ct {
			logger.fatal(LOG_SCOPE, "expected comptime value for symbol: %s", symbol.name)
			return false
		}

		// assign default type based on value and range
		#partial switch v in comptime_val {
		case bool:
			symbol.type = .bool

		case u64:
			// check if fits in u32
			u32_val := u32(v)
			if u64(u32_val) == v {
				symbol.type = .u32
			} else {
				symbol.type = .u64
			}

		case i64:
			// check if fits in i32
			i32_val := i32(v)
			if i64(i32_val) == v {
				symbol.type = .i32
			} else {
				symbol.type = .i64
			}

		case f64:
			// check if can be represented exactly in f32
			f32_val := f32(v)
			if f64(f32_val) == v {
				symbol.type = .f32
			} else {
				symbol.type = .f64
			}

		case:
			logger.fatal(LOG_SCOPE, "unexpected data type from parser when applying default types")
			return false
		}
	}

	return true
}

analyze :: proc(s: ^Semantic) -> bool {
	// first pass: collect symbols
	if !collect_symbols(s) do return false

	// second pass: evaluate comptime expressions in dependency order
	if !evaluate_constants(s) do return false

	// third pass: apply default types to remainin untyped constants
	if !apply_default_types(s) do return false

	return true
}
