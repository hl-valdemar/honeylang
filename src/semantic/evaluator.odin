package semantic

import "../logger"
import "../parser"

import "base:intrinsics"

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
				return uint_to_type(v, ctx_type)
			}
			return v, true

		case i64:
			// if context type is specified, convert to that type
			if ctx_type, ok := context_type.?; ok {
				return int_to_type(v, ctx_type)
			}
			return v, true

		case f64:
			// if context type is specified, convert to that type
			if ctx_type, ok := context_type.?; ok {
				return float_to_type(v, ctx_type)
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
			report_error(
				s,
				nil,
				.semantic_undefined_identifier,
				"undefined identifier '%s'",
				node.name,
			)
			return {}, false
		}

		// infer type from context
		if context_type != nil && symbol.type == nil {
			symbol.type = context_type

			// if symbol is already evaluated, convert the stored value to match the new type
			if symbol.eval_state == .evaluated {
				if old_value, ok_val := symbol.value.?; ok_val {
					if comptime_val, ok_ct := old_value.(ComptimeValue); ok_ct {
						new_value, ok_conv := comptime_value_to_type(comptime_val, context_type.?)
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
			symbol_type, ok_symtype := infer_type_from_value(comptime_val).?
			if !ok_symtype {
				logger.fatal(LOG_SCOPE, "failed to infer type from value")
				return {}, false
			}

			if symbol.type != nil {
				symbol_type = symbol.type.?
			}

			// only convert if types differ
			if ctx_type != symbol_type {
				return comptime_value_to_type(comptime_val, ctx_type)
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
		left_type, ok_left_type := infer_type_from_value(left_ct_untyped).?
		if !ok_left_type {
			logger.fatal(LOG_SCOPE, "failed to infer type from value")
			return {}, false
		}

		right_type, ok_right_type := infer_type_from_value(right_ct_untyped).?
		if !ok_right_type {
			logger.fatal(LOG_SCOPE, "failed to infer type from value")
			return {}, false
		}

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
			return eval_binary_numeric(u8, l, r, node.op)

		case u16:
			r, ok := right_ct.(u16)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_numeric(u16, l, r, node.op)

		case u32:
			r, ok := right_ct.(u32)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_numeric(u32, l, r, node.op)

		case u64:
			r, ok := right_ct.(u64)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_numeric(u64, l, r, node.op)

		case i8:
			r, ok := right_ct.(i8)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_numeric(i8, l, r, node.op)

		case i16:
			r, ok := right_ct.(i16)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_numeric(i16, l, r, node.op)

		case i32:
			r, ok := right_ct.(i32)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_numeric(i32, l, r, node.op)

		case i64:
			r, ok := right_ct.(i64)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_numeric(i64, l, r, node.op)

		case f16:
			r, ok := right_ct.(f16)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_numeric(f16, l, r, node.op)

		case f32:
			r, ok := right_ct.(f32)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_numeric(f32, l, r, node.op)

		case f64:
			r, ok := right_ct.(f64)
			if !ok {
				logger.fatal(LOG_SCOPE, "type mismatch in binary operation")
				return {}, false
			}
			return eval_binary_numeric(f64, l, r, node.op)

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

eval_binary_bool :: proc(left, right: bool, op: parser.BinaryOpKind) -> (SymbolValue, bool) {
	#partial switch op {
	case .logical_and:
		return left && right, true
	case .logical_or:
		return left || right, true
	case:
		logger.fatal(LOG_SCOPE, "unsupported operation for bool: %v", op)
		return {}, false
	}
}

eval_binary_numeric :: proc(
	$T: typeid,
	left, right: T,
	op: parser.BinaryOpKind,
) -> (
	SymbolValue,
	bool,
) where intrinsics.type_is_numeric(T) {
	#partial switch op {
	case .add:
		return left + right, true
	case .sub:
		return left - right, true
	case .mul:
		return left * right, true
	case .div:
		// division by zero
		if right == 0 {
			return {}, false // TODO: handle via error list
		}
		return left / right, true
	case .equal:
		return left == right, true
	case .different:
		return left != right, true
	case .less:
		return left < right, true
	case .greater:
		return left > right, true
	case .less_equal:
		return left <= right, true
	case .greater_equal:
		return left >= right, true
	}
	return {}, false
}
