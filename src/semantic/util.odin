package semantic

import "../logger"
import "../parser"

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

eval_binary_u8 :: proc(left, right: u8, op: parser.BinaryOpKind) -> (SymbolValue, bool) {
	#partial switch op {
	case .add:
		return left + right, true
	case .sub:
		return left - right, true
	case .mul:
		return left * right, true
	case .div:
		if right == 0 {
			logger.fatal(LOG_SCOPE, "division by zero")
			return {}, false
		}
		return left / right, true
	case .less:
		return left < right, true
	case .greater:
		return left > right, true
	case .less_equal:
		return left <= right, true
	case .greater_equal:
		return left >= right, true
	case:
		logger.fatal(LOG_SCOPE, "unsupported operation for u8: %v", op)
		return {}, false
	}
}

eval_binary_u16 :: proc(left, right: u16, op: parser.BinaryOpKind) -> (SymbolValue, bool) {
	#partial switch op {
	case .add:
		return left + right, true
	case .sub:
		return left - right, true
	case .mul:
		return left * right, true
	case .div:
		if right == 0 {
			logger.fatal(LOG_SCOPE, "division by zero")
			return {}, false
		}
		return left / right, true
	case .less:
		return left < right, true
	case .greater:
		return left > right, true
	case .less_equal:
		return left <= right, true
	case .greater_equal:
		return left >= right, true
	case:
		logger.fatal(LOG_SCOPE, "unsupported operation for u16: %v", op)
		return {}, false
	}
}

eval_binary_u32 :: proc(left, right: u32, op: parser.BinaryOpKind) -> (SymbolValue, bool) {
	#partial switch op {
	case .add:
		return left + right, true
	case .sub:
		return left - right, true
	case .mul:
		return left * right, true
	case .div:
		if right == 0 {
			logger.fatal(LOG_SCOPE, "division by zero")
			return {}, false
		}
		return left / right, true
	case .less:
		return left < right, true
	case .greater:
		return left > right, true
	case .less_equal:
		return left <= right, true
	case .greater_equal:
		return left >= right, true
	case:
		logger.fatal(LOG_SCOPE, "unsupported operation for u32: %v", op)
		return {}, false
	}
}

eval_binary_u64 :: proc(left, right: u64, op: parser.BinaryOpKind) -> (SymbolValue, bool) {
	#partial switch op {
	case .add:
		return left + right, true
	case .sub:
		return left - right, true
	case .mul:
		return left * right, true
	case .div:
		if right == 0 {
			logger.fatal(LOG_SCOPE, "division by zero")
			return {}, false
		}
		return left / right, true
	case .less:
		return left < right, true
	case .greater:
		return left > right, true
	case .less_equal:
		return left <= right, true
	case .greater_equal:
		return left >= right, true
	case:
		logger.fatal(LOG_SCOPE, "unsupported operation for u64: %v", op)
		return {}, false
	}
}

eval_binary_i8 :: proc(left, right: i8, op: parser.BinaryOpKind) -> (SymbolValue, bool) {
	#partial switch op {
	case .add:
		return left + right, true
	case .sub:
		return left - right, true
	case .mul:
		return left * right, true
	case .div:
		if right == 0 {
			logger.fatal(LOG_SCOPE, "division by zero")
			return {}, false
		}
		return left / right, true
	case .less:
		return left < right, true
	case .greater:
		return left > right, true
	case .less_equal:
		return left <= right, true
	case .greater_equal:
		return left >= right, true
	case:
		logger.fatal(LOG_SCOPE, "unsupported operation for i8: %v", op)
		return {}, false
	}
}

eval_binary_i16 :: proc(left, right: i16, op: parser.BinaryOpKind) -> (SymbolValue, bool) {
	#partial switch op {
	case .add:
		return left + right, true
	case .sub:
		return left - right, true
	case .mul:
		return left * right, true
	case .div:
		if right == 0 {
			logger.fatal(LOG_SCOPE, "division by zero")
			return {}, false
		}
		return left / right, true
	case .less:
		return left < right, true
	case .greater:
		return left > right, true
	case .less_equal:
		return left <= right, true
	case .greater_equal:
		return left >= right, true
	case:
		logger.fatal(LOG_SCOPE, "unsupported operation for i16: %v", op)
		return {}, false
	}
}

eval_binary_i32 :: proc(left, right: i32, op: parser.BinaryOpKind) -> (SymbolValue, bool) {
	#partial switch op {
	case .add:
		return left + right, true
	case .sub:
		return left - right, true
	case .mul:
		return left * right, true
	case .div:
		if right == 0 {
			logger.fatal(LOG_SCOPE, "division by zero")
			return {}, false
		}
		return left / right, true
	case .less:
		return left < right, true
	case .greater:
		return left > right, true
	case .less_equal:
		return left <= right, true
	case .greater_equal:
		return left >= right, true
	case:
		logger.fatal(LOG_SCOPE, "unsupported operation for i32: %v", op)
		return {}, false
	}
}

eval_binary_i64 :: proc(left, right: i64, op: parser.BinaryOpKind) -> (SymbolValue, bool) {
	#partial switch op {
	case .add:
		return left + right, true
	case .sub:
		return left - right, true
	case .mul:
		return left * right, true
	case .div:
		if right == 0 {
			logger.fatal(LOG_SCOPE, "division by zero")
			return {}, false
		}
		return left / right, true
	case .less:
		return left < right, true
	case .greater:
		return left > right, true
	case .less_equal:
		return left <= right, true
	case .greater_equal:
		return left >= right, true
	case:
		logger.fatal(LOG_SCOPE, "unsupported operation for i64: %v", op)
		return {}, false
	}
}

eval_binary_f16 :: proc(left, right: f16, op: parser.BinaryOpKind) -> (SymbolValue, bool) {
	#partial switch op {
	case .add:
		return left + right, true
	case .sub:
		return left - right, true
	case .mul:
		return left * right, true
	case .div:
		return left / right, true
	case .less:
		return left < right, true
	case .greater:
		return left > right, true
	case .less_equal:
		return left <= right, true
	case .greater_equal:
		return left >= right, true
	case:
		logger.fatal(LOG_SCOPE, "unsupported operation for f16: %v", op)
		return {}, false
	}
}

eval_binary_f32 :: proc(left, right: f32, op: parser.BinaryOpKind) -> (SymbolValue, bool) {
	#partial switch op {
	case .add:
		return left + right, true
	case .sub:
		return left - right, true
	case .mul:
		return left * right, true
	case .div:
		return left / right, true
	case .less:
		return left < right, true
	case .greater:
		return left > right, true
	case .less_equal:
		return left <= right, true
	case .greater_equal:
		return left >= right, true
	case:
		logger.fatal(LOG_SCOPE, "unsupported operation for f32: %v", op)
		return {}, false
	}
}

eval_binary_f64 :: proc(left, right: f64, op: parser.BinaryOpKind) -> (SymbolValue, bool) {
	#partial switch op {
	case .add:
		return left + right, true
	case .sub:
		return left - right, true
	case .mul:
		return left * right, true
	case .div:
		return left / right, true
	case .less:
		return left < right, true
	case .greater:
		return left > right, true
	case .less_equal:
		return left <= right, true
	case .greater_equal:
		return left >= right, true
	case:
		logger.fatal(LOG_SCOPE, "unsupported operation for f64: %v", op)
		return {}, false
	}
}
