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
		// base case - convert to context type if provided
		#partial switch v in node.value {
		case bool:
			return v, true

		case u64:
			// if context type is specified, convert to that type
			if ctx_type, ok := context_type.?; ok {
				return convert_uint_to_type(v, ctx_type)
			}
			return v, true

		case i64:
			// if context type is specified, convert to that type
			if ctx_type, ok := context_type.?; ok {
				return convert_int_to_type(v, ctx_type)
			}
			return v, true

		case f64:
			// if context type is specified, convert to that type
			if ctx_type, ok := context_type.?; ok {
				return convert_float_to_type(v, ctx_type)
			}
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

			// if symbol is already evaluated, convert the stored value to match the new type
			if symbol.eval_state == .evaluated {
				if old_value, ok_val := symbol.value.?; ok_val {
					if comptime_val, ok_ct := old_value.(ComptimeValue); ok_ct {
						new_value, ok_conv := convert_comptime_value_to_type(
							comptime_val,
							context_type.?,
						)
						if !ok_conv {
							logger.fatal(
								LOG_SCOPE,
								"failed to convert symbol value to inferred type for: %s",
								node.name,
							)
							return {}, false
						}
						symbol.value = new_value
					}
				}
			}
		}

		// recursively evaluate
		if !evaluate_symbol(s, symbol) do return {}, false

		// symbol value must not be nil at this point
		value := symbol.value.?

		// get the actual ComptimeValue
		comptime_val, val_ok := value.(ComptimeValue)
		if !val_ok {
			logger.fatal(LOG_SCOPE, "expected comptime value for symbol: %s", node.name)
			return {}, false
		}

		// if context type is specified and different from symbol's type, convert
		if ctx_type, ctx_ok := context_type.?; ctx_ok {
			symbol_type := infer_type_from_value(comptime_val)
			if symbol.type != nil {
				symbol_type = symbol.type.?
			}

			// only convert if types differ
			if ctx_type != symbol_type {
				return convert_comptime_value_to_type(comptime_val, ctx_type)
			}
		}

		return value, true

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
			case i8:
				return -v, true
			case i16:
				return -v, true
			case i32:
				return -v, true
			case i64:
				return -v, true
			case f16:
				return -v, true
			case f32:
				return -v, true
			case f64:
				return -v, true

			case:
				logger.fatal(LOG_SCOPE, "cannot negate non-numeric / unsigned value")
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
		// first, evaluate both operands without context to get their natural types
		left_val_untyped, ok_left := evaluate_expr(s, node.left)
		if !ok_left do return {}, false

		right_val_untyped, ok_right := evaluate_expr(s, node.right)
		if !ok_right do return {}, false

		// get the comptime values
		left_ct_untyped, ok_left_ct := left_val_untyped.(ComptimeValue)
		right_ct_untyped, ok_right_ct := right_val_untyped.(ComptimeValue)
		if !ok_left_ct || !ok_right_ct {
			logger.fatal(LOG_SCOPE, "expected comptime values in binary operation")
			return {}, false
		}

		// infer the types from the values
		left_type := infer_type_from_value(left_ct_untyped)
		right_type := infer_type_from_value(right_ct_untyped)

		// find the common type (with type promotion)
		operation_type, ok_common := find_common_type(left_type, right_type).?
		if !ok_common {
			logger.fatal(
				LOG_SCOPE,
				"incompatible types in binary operation: %v and %v",
				left_type,
				right_type,
			)
			return {}, false
		}

		// if context type is provided and wider, use that instead
		if ctx_type, ok_ctx := context_type.?; ok_ctx {
			if common, ok_common_ctx := find_common_type(operation_type, ctx_type).?;
			   ok_common_ctx {
				operation_type = common
			}
		}

		// now evaluate both operands with the common type
		left_val, ok_left_val := evaluate_expr(s, node.left, operation_type)
		if !ok_left_val do return {}, false

		right_val, ok_right_val := evaluate_expr(s, node.right, operation_type)
		if !ok_right_val do return {}, false

		// extract ComptimeValues
		left_ct, ok_left_ct2 := left_val.(ComptimeValue)
		right_ct, ok_right_ct2 := right_val.(ComptimeValue)
		if !ok_left_ct2 || !ok_right_ct2 {
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

		case u8:
			symbol.type = .u8
		case u16:
			symbol.type = .u16
		case u32:
			symbol.type = .u32
		case u64:
			// check if fits in u32
			u32_val := u32(v)
			if u64(u32_val) == v {
				symbol.type = .u32
			} else {
				symbol.type = .u64
			}

		case i8:
			symbol.type = .i8
		case i16:
			symbol.type = .i16
		case i32:
			symbol.type = .i32
		case i64:
			// check if fits in i32
			i32_val := i32(v)
			if i64(i32_val) == v {
				symbol.type = .i32
			} else {
				symbol.type = .i64
			}

		case f16:
			symbol.type = .f16
		case f32:
			symbol.type = .f32
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

// type annotate ast
update_ast :: proc(s: ^Semantic) -> bool {
	program := s.program.(parser.Program)

	for &decl in program.declarations {
		// skip typed declarations
		if decl.type != nil do continue

		// find the symbol
		symbol, ok_lookup := symtab_lookup(&s.symtab, decl.name).?
		if !ok_lookup {
			logger.fatal(LOG_SCOPE, "found unregistered declaration in AST (update_ast)")
			return false
		}

		// get the type
		sym_type, ok_symtype := symbol.type.?
		if !ok_symtype {
			logger.fatal(LOG_SCOPE, "found symbol without type in (update_ast)")
			return false
		}

		// match the type
		switch sym_type {
		case .bool:
			decl.type = new(parser.TypeNode)
			decl.type.?^ = .bool

		case .u8:
			decl.type = new(parser.TypeNode)
			decl.type.?^ = .u8
		case .u16:
			decl.type = new(parser.TypeNode)
			decl.type.?^ = .u16
		case .u32:
			decl.type = new(parser.TypeNode)
			decl.type.?^ = .u32
		case .u64:
			decl.type = new(parser.TypeNode)
			decl.type.?^ = .u64

		case .i8:
			decl.type = new(parser.TypeNode)
			decl.type.?^ = .i8
		case .i16:
			decl.type = new(parser.TypeNode)
			decl.type.?^ = .i16
		case .i32:
			decl.type = new(parser.TypeNode)
			decl.type.?^ = .i32
		case .i64:
			decl.type = new(parser.TypeNode)
			decl.type.?^ = .i64

		case .f16:
			decl.type = new(parser.TypeNode)
			decl.type.?^ = .f16
		case .f32:
			decl.type = new(parser.TypeNode)
			decl.type.?^ = .f32
		case .f64:
			decl.type = new(parser.TypeNode)
			decl.type.?^ = .f64
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

	// complete ast type annotation
	if !update_ast(s) do return false

	return true
}
