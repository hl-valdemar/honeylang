package parser

import "core:fmt"

AstNode :: union {
	Program,
	Declaration,
	Identifier,
	Literal,
	UnaryOp,
	BinaryOp,
}

Program :: struct {
	declarations: [dynamic]Declaration,
}

Declaration :: struct {
	kind:  DeclKind,
	name:  string,
	type:  Maybe(^TypeNode),
	value: ^AstNode,
}

DeclKind :: enum {
	func,
	// compile-time constant
	const,
	// runtime constant
	immutable,
	// runtime mutable
	mutable,
}

TypeNode :: union {
	LiteralType,
	NamedType,
	PointerType,
	// ArrayType,
	// etc...
}

LiteralType :: enum {
	bool,

	// unsigned integers
	u8,
	u16,
	u32,
	u64, // default value when parsing

	// signed integers
	i8,
	i16,
	i32,
	i64, // default value when parsing

	// floats
	f16,
	f32,
	f64, // default value when parsing
}

NamedType :: struct {
	name: string, // "i32", "u8", "f64", etc
}

PointerType :: struct {
	pointee: ^TypeNode,
}

Identifier :: struct {
	name: string,
}

Literal :: struct {
	value: LiteralValue,
}

LiteralValue :: union {
	bool,

	// unsigned integers
	u8,
	u16,
	u32,
	u64, // default value when parsing

	// signed integers
	i8,
	i16,
	i32,
	i64, // default value when parsing

	// floats
	f16,
	f32,
	f64, // default value when parsing
}

UnaryOp :: struct {
	operand: ^AstNode,
	op:      UnaryOpKind,
}

UnaryOpKind :: enum {
	negate,
	logical_not,
}

BinaryOp :: struct {
	left:  ^AstNode,
	right: ^AstNode,
	op:    BinaryOpKind,
}

BinaryOpKind :: enum {
	// arithmetic
	add,
	sub,
	mul,
	div,

	// logical
	logical_and,
	logical_or,

	// comparative
	less,
	greater,
	less_equal,
	greater_equal,
}

print_indent :: proc(indent: int, is_last: []bool) {
	if indent == 0 do return

	fmt.print(" ")

	// print connectors for ancestor levels
	for i := 0; i < indent - 1; i += 1 {
		if i < len(is_last) && is_last[i] {
			fmt.print("    ") // ancestor was last child, no vertical bar
		} else {
			fmt.print("││  ") // ancestor has more siblings, show vertical bar
		}
	}

	fmt.print("└─ ")
}

ast_print :: proc(node: ^AstNode, indent := 0, is_last: []bool = {}) {
	switch &n in node {
	case Program:
		print_indent(indent, is_last)
		fmt.print("program:\n")
		for &decl, i in n.declarations {
			is_last_decl := (i == len(n.declarations) - 1)
			new_is_last := make([dynamic]bool, len(is_last))
			defer delete(new_is_last)
			copy(new_is_last[:], is_last)
			append(&new_is_last, is_last_decl)
			print_decl(&decl, indent + 1, new_is_last[:])
		}

	case Declaration:
		print_decl(&n, indent, is_last)

	case UnaryOp:
		print_indent(indent, is_last)
		fmt.printf("unary:\n")

		new_is_last := make([dynamic]bool, len(is_last))
		defer delete(new_is_last)
		copy(new_is_last[:], is_last)
		append(&new_is_last, false) // op is not last

		print_indent(indent + 1, new_is_last[:])
		fmt.printf("op: %v\n", n.op)

		new_is_last[len(new_is_last) - 1] = true // operand is last
		ast_print(n.operand, indent + 1, new_is_last[:])

	case BinaryOp:
		print_indent(indent, is_last)
		fmt.printf("binary:\n")

		new_is_last := make([dynamic]bool, len(is_last))
		defer delete(new_is_last)
		copy(new_is_last[:], is_last)
		append(&new_is_last, false) // op is not last

		print_indent(indent + 1, new_is_last[:])
		fmt.printf("op: %v\n", n.op)

		ast_print(n.left, indent + 1, new_is_last[:])

		new_is_last[len(new_is_last) - 1] = true // right is last
		ast_print(n.right, indent + 1, new_is_last[:])

	case Identifier:
		print_indent(indent, is_last)
		fmt.printf("identifier: %v\n", n.name)

	case Literal:
		print_indent(indent, is_last)
		fmt.printf("literal: %v\n", n.value)
	}
}

print_decl :: proc(decl: ^Declaration, indent := 0, is_last: []bool = {}) {
	print_indent(indent, is_last)
	fmt.printf("declaration (%v):\n", decl.kind)

	new_is_last := make([dynamic]bool, len(is_last))
	defer delete(new_is_last)
	copy(new_is_last[:], is_last)
	append(&new_is_last, false) // not last yet

	print_indent(indent + 1, new_is_last[:])
	fmt.printf("name: %s\n", decl.name)

	print_type(decl.type, indent + 1, new_is_last[:])

	new_is_last[len(new_is_last) - 1] = true // Value is last child
	ast_print(decl.value, indent + 1, new_is_last[:])
}

print_type :: proc(type_node: Maybe(^TypeNode), indent := 0, is_last: []bool = {}) {
	print_indent(indent, is_last)

	node, ok := type_node.?
	if !ok {
		fmt.printf("type: %v\n", node)
		return
	}

	switch n in node {
	case LiteralType:
		fmt.printf("type: %v\n", n)
	case NamedType:
		fmt.printf("type: %v\n", n.name)
	case PointerType:
		fmt.print("type: %v\n", n.pointee)
	}
}

ast_destroy :: proc(node: ^AstNode) {
	if node == nil do return

	switch n in node {
	case Program:
		for &decl in n.declarations {
			decl_destroy(&decl)
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

decl_destroy :: proc(decl: ^Declaration) {
	if decl == nil do return
	ast_destroy(decl.value)
	if type, ok := decl.type.?; ok {
		type_destroy(type)
	}
	free(decl)
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
