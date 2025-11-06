package semantic

import "../logger"
import "../parser"

infer_type_from_value :: proc(value: ComptimeValue) -> Maybe(SymbolType) {
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
		// apply default type policy: parser uses u64 as intermediate representation
		// default to u32 if the value fits, otherwise use u64
		u32_val := u32(v)
		if u64(u32_val) == v {
			return .u32
		}
		return .u64
	case i8:
		return .i8
	case i16:
		return .i16
	case i32:
		return .i32
	case i64:
		// apply default type policy: parser uses i64 as intermediate representation
		// default to i32 if the value fits, otherwise use i64
		i32_val := i32(v)
		if i64(i32_val) == v {
			return .i32
		}
		return .i64
	case f16:
		return .f16
	case f32:
		return .f32
	case f64:
		// apply default type policy: parser uses f64 as intermediate representation
		// always default to f32 (the parser's f64 is just for precision, not user intent)
		return .f32
	}
	return nil // fallback
}

// find the common type between two types (the wider one that both can promote to)
find_common_type :: proc(type1: SymbolType, type2: SymbolType) -> Maybe(SymbolType) {
	if type1 == type2 do return type1

	if can_promote_type(type1, type2) do return type2
	if can_promote_type(type2, type1) do return type1

	return nil
}

// type promotion rules: can we promote from_type to to_type?
can_promote_type :: proc(from_type: SymbolType, to_type: SymbolType) -> bool {
	// same type - always ok
	if from_type == to_type do return true

	// integer to float promotion (always allowed)
	#partial switch to_type {
	case .f16, .f32, .f64:
		#partial switch from_type {
		case .u8, .u16, .u32, .u64, .i8, .i16, .i32, .i64:
			return true
		}
	}

	// signed integer to wider signed integer
	#partial switch from_type {
	case .i8:
		#partial switch to_type {
		case .i16, .i32, .i64:
			return true
		}
	case .i16:
		#partial switch to_type {
		case .i32, .i64:
			return true
		}
	case .i32:
		#partial switch to_type {
		case .i64:
			return true
		}
	}

	// unsigned integer to wider unsigned integer
	#partial switch from_type {
	case .u8:
		#partial switch to_type {
		case .u16, .u32, .u64:
			return true
		}
	case .u16:
		#partial switch to_type {
		case .u32, .u64:
			return true
		}
	case .u32:
		#partial switch to_type {
		case .u64:
			return true
		}
	}

	// float to wider float
	#partial switch from_type {
	case .f16:
		#partial switch to_type {
		case .f32, .f64:
			return true
		}
	case .f32:
		#partial switch to_type {
		case .f64:
			return true
		}
	}

	return false
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
