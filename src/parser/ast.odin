package parser

import "core:fmt"

AstNode :: union {
	Declaration,
	Identifier,
	Literal,
}

Declaration :: struct {
	kind:  DeclKind,
	name:  string,
	type:  Maybe(^TypeNode),
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

TypeNode :: union {
	NamedType,
	PointerType,
	// ArrayType,
	// etc...
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
	#partial switch n in node {
	case Declaration:
		fmt.printf("declaration (%v):\n", n.kind)

		print_indent(indent + 1)
		fmt.printf("name: %s\n", n.name)

		print_type_node(n.type, indent + 1)

		print_ast(n.value, indent + 1)

	case Identifier:
		print_indent(indent)
		fmt.printf("identifier: %v\n", n.name)

	case Literal:
		fmt.printf("literal: %v\n", n.value)
	}
}

print_type_node :: proc(type_node: Maybe(^TypeNode), indent := 0) {
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

destroy_ast :: proc(node: ^AstNode) {
	if node == nil do return

	switch n in node {
	case Declaration:
		destroy_ast(n.value)
		if type_node, ok := n.type.?; ok {
			destroy_type(type_node)
		}

	case Identifier, Literal:
	// no children
	}

	free(node)
}

destroy_type :: proc(node: ^TypeNode) {
	if node == nil do return

	switch n in node {
	case PointerType:
		destroy_type(n.pointee)

	case NamedType:
	// no children
	}

	free(node)
}
