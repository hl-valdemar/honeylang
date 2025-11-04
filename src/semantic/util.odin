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


convert_uint_to_type :: proc(v: u64, target_type: SymbolType) -> (SymbolValue, bool) {
	#partial switch target_type {
	case .u8:
		return u8(v), true
	case .u16:
		return u16(v), true
	case .u32:
		return u32(v), true
	case .u64:
		return v, true
	case .i8:
		return i8(v), true
	case .i16:
		return i16(v), true
	case .i32:
		return i32(v), true
	case .i64:
		return i64(v), true

	case:
		logger.fatal(LOG_SCOPE, "cannot convert uint to type: %v", target_type)
		return {}, false
	}
}

convert_int_to_type :: proc(v: i64, target_type: SymbolType) -> (SymbolValue, bool) {
	#partial switch target_type {
	case .u8:
		return u8(v), true
	case .u16:
		return u16(v), true
	case .u32:
		return u32(v), true
	case .u64:
		return u64(v), true
	case .i8:
		return i8(v), true
	case .i16:
		return i16(v), true
	case .i32:
		return i32(v), true
	case .i64:
		return v, true

	case:
		logger.fatal(LOG_SCOPE, "cannot convert int to type: %v", target_type)
		return {}, false
	}
}

convert_float_to_type :: proc(v: f64, target_type: SymbolType) -> (SymbolValue, bool) {
	#partial switch target_type {
	case .f16:
		return f16(v), true
	case .f32:
		return f32(v), true
	case .f64:
		return v, true
	case:
		logger.fatal(LOG_SCOPE, "cannot convert float to type: %v", target_type)
		return {}, false
	}
}

infer_type_from_value :: proc(value: ComptimeValue) -> SymbolType {
	switch v in value {
	case bool:
		return .bool
	case u8:
		return .u8
	case u16:
		return .u16
	case u32:
		return .u32
	case u64:
		return .u64
	case i8:
		return .i8
	case i16:
		return .i16
	case i32:
		return .i32
	case i64:
		return .i64
	case f16:
		return .f16
	case f32:
		return .f32
	case f64:
		return .f64
	}
	return .i32 // fallback
}

// convert a ComptimeValue to a target type
convert_comptime_value_to_type :: proc(
	value: ComptimeValue,
	target_type: SymbolType,
) -> (
	SymbolValue,
	bool,
) {
	switch v in value {
	case u8:
		return convert_uint_to_type(u64(v), target_type)
	case u16:
		return convert_uint_to_type(u64(v), target_type)
	case u32:
		return convert_uint_to_type(u64(v), target_type)
	case u64:
		return convert_uint_to_type(v, target_type)
	case i8:
		return convert_int_to_type(i64(v), target_type)
	case i16:
		return convert_int_to_type(i64(v), target_type)
	case i32:
		return convert_int_to_type(i64(v), target_type)
	case i64:
		return convert_int_to_type(v, target_type)
	case f16:
		return convert_float_to_type(f64(v), target_type)
	case f32:
		return convert_float_to_type(f64(v), target_type)
	case f64:
		return convert_float_to_type(v, target_type)
	case bool:
		return v, true
	}
	return {}, false
}
