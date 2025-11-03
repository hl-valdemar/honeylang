package parser

import "core:fmt"

AstNode :: union {
	Program,
	Declaration,
	Identifier,
	Literal,
}

Program :: struct {
	declarations: [dynamic]Declaration,
}

Declaration :: struct {
	kind:  DeclKind,
	name:  string,
	type:  Maybe(^Type),
	value: ^AstNode,
}

DeclKind :: enum {
	// compile-time constant
	comptime,
	// runtime constant
	immutable,
	// runtime mutable
	mutable,
}

Type :: union {
	NamedType,
	PointerType,
	// ArrayType,
	// etc...
}

NamedType :: struct {
	name: string, // "i32", "u8", "f64", etc
}

PointerType :: struct {
	pointee: ^Type,
}

Identifier :: struct {
	name: string,
}

Literal :: struct {
	value: LiteralValue,
}

LiteralValue :: union {
	bool,
}

print_indent :: proc(indent: int) {
	if indent == 0 do return

	fmt.print("└")
	for i := 0; i < indent; i += 1 {
		fmt.print("─")
	}
	fmt.print(" ")
}

print_ast :: proc(node: ^AstNode, indent := 0) {
	print_indent(indent)

	// TODO: remove the `#partial` directive
	#partial switch &n in node {
	case Program:
		for &decl in n.declarations {
			print_decl(&decl, indent)
		}

	case Declaration:
		print_decl(&n, indent)

	case Identifier:
		print_indent(indent)
		fmt.printf("identifier: %v\n", n.name)

	case Literal:
		fmt.printf("literal: %v\n", n.value)
	}
}

print_decl :: proc(decl: ^Declaration, indent := 0) {
	print_indent(indent)

	fmt.printf("declaration (%v):\n", decl.kind)

	print_indent(indent + 1)
	fmt.printf("name: %s\n", decl.name)

	print_type(decl.type, indent + 1)

	print_ast(decl.value, indent + 1)
}

print_type :: proc(type_node: Maybe(^Type), indent := 0) {
	print_indent(indent)

	node, ok := type_node.?
	if !ok {
		fmt.printf("type: %v\n", node)
		return
	}

	switch n in node {
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

type_destroy :: proc(node: ^Type) {
	if node == nil do return

	switch n in node {
	case PointerType:
		type_destroy(n.pointee)

	case NamedType:
	// no children
	}

	free(node)
}
