package semantic

import "../logger"
import "../parser"

uint_to_type :: proc(v: u64, target_type: SymbolType) -> (SymbolValue, bool) {
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
	case .f16:
		return f16(v), true
	case .f32:
		return f32(v), true
	case .f64:
		return f64(v), true

	case:
		logger.fatal(LOG_SCOPE, "cannot convert uint to type: %v", target_type)
		return {}, false
	}
}

int_to_type :: proc(v: i64, target_type: SymbolType) -> (SymbolValue, bool) {
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
	case .f16:
		return f16(v), true
	case .f32:
		return f32(v), true
	case .f64:
		return f64(v), true

	case:
		logger.fatal(LOG_SCOPE, "cannot convert int to type: %v", target_type)
		return {}, false
	}
}

float_to_type :: proc(v: f64, target_type: SymbolType) -> (SymbolValue, bool) {
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

// convert a ComptimeValue to a target type
comptime_value_to_type :: proc(
	value: ComptimeValue,
	target_type: SymbolType,
) -> (
	SymbolValue,
	bool,
) {
	switch v in value {
	case u8:
		return uint_to_type(u64(v), target_type)
	case u16:
		return uint_to_type(u64(v), target_type)
	case u32:
		return uint_to_type(u64(v), target_type)
	case u64:
		return uint_to_type(v, target_type)
	case i8:
		return int_to_type(i64(v), target_type)
	case i16:
		return int_to_type(i64(v), target_type)
	case i32:
		return int_to_type(i64(v), target_type)
	case i64:
		return int_to_type(v, target_type)
	case f16:
		return float_to_type(f64(v), target_type)
	case f32:
		return float_to_type(f64(v), target_type)
	case f64:
		return float_to_type(v, target_type)
	case bool:
		return v, true
	}
	return {}, false
}

// convert ComptimeValue to LiteralValue
comptime_to_literal :: proc(val: ComptimeValue) -> parser.LiteralValue {
	switch v in val {
	case bool:
		return v
	case u8:
		return v
	case u16:
		return v
	case u32:
		return v
	case u64:
		return v
	case i8:
		return v
	case i16:
		return v
	case i32:
		return v
	case i64:
		return v
	case f16:
		return v
	case f32:
		return v
	case f64:
		return v
	}
	return {} // unreachable
}
