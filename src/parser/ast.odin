package parser

import "../error"

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
	loc:   SourceSpan,
}

SourceSpan :: struct {
	start: error.SourceLocation,
	end:   error.SourceLocation,
}

DeclKind :: enum {
	// compile-time constant
	const,
	func,

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
