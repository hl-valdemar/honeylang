package semantic

import "../logger"

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
