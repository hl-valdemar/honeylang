package parser

import "core:fmt"

AstNode :: union {
	Declaration,
	Identifier,
	Literal,
}

Declaration :: struct {
	kind:  DeclKind,
	name:  []rune,
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
	name: []rune,
}

Literal :: struct {
	value: LiteralValue,
}

LiteralValue :: union {
	bool,
}

print_indent :: proc(indent: int) {
	for i := 0; i < indent; i += 1 {
		fmt.print("  ")
	}
}

print_ast :: proc(node: ^AstNode, indent := 0) {
	print_indent(indent)

	// TODO: remove the `#partial` directive
	#partial switch n in node {
	case Declaration:
		fmt.printf("declaration [%s] %s", n.kind, n.name)
		if type_node, ok := n.type.?; ok {
			fmt.printf(": %v\n", type_node)
		}
		print_ast(n.value, indent + 2)

	case Identifier:
		fmt.printf("identifier: %s\n", n.name)
	}
}
