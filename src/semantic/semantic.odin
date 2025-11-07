package semantic

import "../error"
import "../logger"
import "../parser"
import "../scope"

LOG_SCOPE :: scope.Scope.semantic

Semantic :: struct {
	program: ^parser.AstNode,
	symtab:  SymbolTable,
	errors:  ^error.ErrorList,
}

init :: proc(program: ^parser.AstNode, errors: ^error.ErrorList) -> Semantic {
	return Semantic{program = program, symtab = symtab_make(), errors = errors}
}

deinit :: proc(s: ^Semantic) {
	symtab_destroy(&s.symtab)
}

collect_symbols :: proc(s: ^Semantic) -> bool {
	program := s.program.(parser.Program)

	for &decl in program.declarations {
		// check for duplicates
		if symtab_lookup(&s.symtab, decl.name) != nil {
			report_error(
				s,
				decl.loc,
				.semantic_duplicate_definition,
				"duplicate definition '%s'",
				decl.name,
			)
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

apply_default_types :: proc(s: ^Semantic) -> bool {
	for &symbol in s.symtab.symbols {
		if symbol.kind != .const do continue
		if symbol.type != nil do continue // already has a type

		value, ok := symbol.value.?
		if !ok {
			logger.debug(LOG_SCOPE, "symbol has no value: %s", symbol.name)
			return false
		}

		comptime_val, ok_ct := value.(ComptimeValue)
		if !ok_ct {
			logger.debug(LOG_SCOPE, "expected comptime value for symbol: %s", symbol.name)
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
			logger.debug(LOG_SCOPE, "unexpected data type from parser when applying default types")
			return false
		}
	}

	return true
}

// type annotate ast
update_ast_types :: proc(s: ^Semantic) -> bool {
	program := s.program.(parser.Program)

	for &decl in program.declarations {
		// skip typed declarations
		if decl.type != nil do continue

		// find the symbol
		symbol, ok_lookup := symtab_lookup(&s.symtab, decl.name).?
		if !ok_lookup {
			logger.debug(LOG_SCOPE, "found unregistered declaration in AST (update_ast)")
			return false
		}

		// get the type
		sym_type, ok_symtype := symbol.type.?
		if !ok_symtype {
			logger.debug(LOG_SCOPE, "found symbol without type in (update_ast)")
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

inline_constants :: proc(s: ^Semantic) -> bool {
	program := s.program.(parser.Program)

	for &decl in program.declarations {
		// TODO: also inline constants in function bodies when the time comes
		if decl.kind == .const {
			// decl.value = inline_expr(s, decl.value) or_return

			// look up in symbol table
			symbol, ok := symtab_lookup(&s.symtab, decl.name).?
			if !ok do continue // not in symtab, leave as-is

			// only inline compile-time constants
			if symbol.kind != .const do continue

			// get the evaluated value
			symbol_val, ok_val := symbol.value.?
			if !ok_val {
				logger.debug(LOG_SCOPE, "constant has no value: %s", decl.name)
				return false
			}

			comptime_val, ok_ct := symbol_val.(ComptimeValue)
			if !ok_ct {
				logger.debug(LOG_SCOPE, "expected comptime value")
				return false
			}

			// create a literal node with the constant's value
			new_node := new(parser.AstNode)
			new_node^ = parser.Literal {
				value = comptime_to_literal(comptime_val),
			}

			// free the old identifier node
			parser.ast_destroy(decl.value)

			decl.value = new_node
		}
	}

	return true
}

analyze :: proc(s: ^Semantic) -> bool {
	// first pass: collect symbols (symtab)
	if !collect_symbols(s) do return false

	// second pass: evaluate comptime expressions in dependency order (symtab)
	if !evaluate_constants(s) do return false

	// third pass: apply default types to remaining untyped constants (symtab)
	if !apply_default_types(s) do return false

	// fourth pass: complete ast type annotation (ast)
	if !update_ast_types(s) do return false

	// inline comptime constants (ast)
	if !inline_constants(s) do return false

	return true
}

report_error :: proc(
	s: ^Semantic,
	loc: error.SourceLocation,
	kind: error.ErrorKind,
	fmt_str: string,
	args: ..any,
) {
	error.add_error(s.errors, kind, loc, fmt_str, ..args)
}
