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
	loc:   error.SourceLocation,
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
	equal,
	different,
	less,
	greater,
	less_equal,
	greater_equal,
}
