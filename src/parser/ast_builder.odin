package parser

import "../error"
import "../logger"

import "core:strconv"
import "core:strings"

make_program :: proc(declarations: [dynamic]Declaration) -> ^AstNode {
	node := new(AstNode)
	node^ = Program {
		declarations = declarations,
	}
	return node
}

// create a complete declaration node
make_declaration :: proc(
	name: string,
	type: ^TypeNode,
	value: ^AstNode,
	kind: DeclKind,
	loc: error.SourceLocation,
) -> ^AstNode {
	node := new(AstNode)
	node^ = Declaration {
		name  = name,
		type  = type,
		value = value,
		kind  = kind,
		loc   = loc,
	}
	return node
}

make_unary :: proc(operand: ^AstNode, op: UnaryOpKind) -> ^AstNode {
	node := new(AstNode)
	node^ = UnaryOp {
		operand = operand,
		op      = op,
	}
	return node
}

make_binary :: proc(left: ^AstNode, right: ^AstNode, op: BinaryOpKind) -> ^AstNode {
	node := new(AstNode)
	node^ = BinaryOp {
		left  = left,
		right = right,
		op    = op,
	}
	return node
}

make_identifier :: proc(tok: Token) -> (^AstNode, bool) {
	val, ok := tok.value.?
	if !ok {
		logger.fatal(LOG_SCOPE, "identifier has no value")
		return nil, false
	}

	node := new(AstNode)
	node^ = Identifier {
		name = val,
	}
	return node, true
}

make_type :: proc(tok: Token) -> (^TypeNode, bool) {
	val, ok := tok.value.?
	if !ok {
		logger.fatal(LOG_SCOPE, "type has no value")
		return nil, false
	}

	node := new(TypeNode)
	node^ = NamedType {
		name = val,
	}

	return node, true
}

make_boolean :: proc(tok: Token) -> (^AstNode, bool) {
	val, ok := tok.value.?
	if !ok {
		logger.fatal(LOG_SCOPE, "boolean has no value")
		return nil, false
	}

	if val != "true" && val != "false" {
		logger.fatal(LOG_SCOPE, "invalid boolean value: %s", val)
		return nil, false
	}

	node := new(AstNode)
	node^ = Literal {
		value = val == "true",
	}
	return node, true
}

make_number :: proc(tok: Token) -> (^AstNode, bool) {
	val, ok := tok.value.?
	if !ok {
		logger.fatal(LOG_SCOPE, "number has no value")
		return nil, false
	}

	node := new(AstNode)

	has_decimal := strings.contains(val, ".")
	if has_decimal {
		// parse as f64 (default)
		num, ok := strconv.parse_f64(val) // always encode as f64 in parsing stage
		if !ok {
			logger.error(LOG_SCOPE, "failed to parse float: %s", val)
			return nil, false
		}
		node^ = Literal {
			value = num,
		}
	} else {
		// parse as i64 (default)
		num, ok := strconv.parse_i64(val) // always encode as i64 in parsing stage
		if !ok {
			logger.error(LOG_SCOPE, "failed to parse integer: %s", val)
			return nil, false
		}
		node^ = Literal {
			value = num,
		}
	}

	return node, true
}

ast_destroy :: proc(node: ^AstNode) {
	if node == nil do return

	switch n in node {
	case Program:
		for &decl in n.declarations {
			ast_destroy(decl.value)
			if type, ok := decl.type.?; ok {
				type_destroy(type)
			}
		}
		delete(n.declarations)

	case Declaration:
		ast_destroy(n.value)
		if type, ok := n.type.?; ok {
			type_destroy(type)
		}

	case UnaryOp:
		ast_destroy(n.operand)

	case BinaryOp:
		ast_destroy(n.left)
		ast_destroy(n.right)

	case Identifier, Literal:
	// no children
	}

	free(node)
}

type_destroy :: proc(node: ^TypeNode) {
	if node == nil do return

	switch n in node {
	case PointerType:
		type_destroy(n.pointee)

	case LiteralType, NamedType:
	// no children
	}

	free(node)
}
