package parser

import "../logger"
import "core:path/slashpath"
import "core:unicode"

import "core:fmt"

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
		fmt.printf("%sprogram%s:\n", logger.color_codes[.cyan], logger.color_codes[.reset])
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

	case Function:
		print_indent(indent, is_last)

		fmt.printf("func:\n")

		print_indent(indent + 1, is_last)
		fmt.printf("parameters:")

		if len(n.parameters) > 0 {
			fmt.println()

			for param in n.parameters {
				print_indent(indent + 2, is_last)
				fmt.printf("name: %v\n", param.name)

				print_indent(indent + 2, is_last)
				print_type(param.type, indent + 2, is_last)
			}
		} else {
			fmt.printf(" %svoid%s\n", logger.color_codes[.cyan], logger.color_codes[.reset])
		}

		print_indent(indent + 1, is_last)
		fmt.printf("statements: %d\n", len(n.body.statements))
		print_indent(indent + 1, is_last)
		fmt.printf("deferred: %d\n", len(n.body.deferred))

	case CallExpr:
		print_indent(indent, is_last)
		fmt.printf("call_expr:\n")

		print_indent(indent + 1, is_last)
		fmt.printf("func: %v\n", n.name)

		print_indent(indent + 1, is_last)
		fmt.printf("arguments: %d\n", len(n.arguments))

	case UnaryOp:
		print_indent(indent, is_last)
		fmt.printf("%sunary%s:\n", logger.color_codes[.cyan], logger.color_codes[.reset])

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
		fmt.printf("%sbinary%s:\n", logger.color_codes[.cyan], logger.color_codes[.reset])

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
		fmt.printf(
			"identifier: %s%v%s\n",
			logger.color_codes[.yellow],
			n.name,
			logger.color_codes[.reset],
		)

	case Literal:
		print_indent(indent, is_last)
		fmt.printf(
			"literal: %s%v%s\n",
			logger.color_codes[.cyan],
			n.value,
			logger.color_codes[.reset],
		)

	case ReturnStmt, DeferStmt: // do nothing
	}
}

print_decl :: proc(decl: ^Declaration, indent := 0, is_last: []bool = {}) {
	print_indent(indent, is_last)
	fmt.printf(
		"%sdeclaration%s (%s%v%s):\n",
		logger.color_codes[.purple],
		logger.color_codes[.reset],
		logger.color_codes[.green],
		decl.kind,
		logger.color_codes[.reset],
	)

	new_is_last := make([dynamic]bool, len(is_last))
	defer delete(new_is_last)
	copy(new_is_last[:], is_last)
	append(&new_is_last, false) // not last yet

	print_indent(indent + 1, new_is_last[:])
	fmt.printf("name: %s\n", decl.name)

	print_type(decl.type, indent + 1, new_is_last[:])

	new_is_last[len(new_is_last) - 1] = true // value is last child
	ast_print(decl.value, indent + 1, new_is_last[:])
}

print_func :: proc(node: ^AstNode, indent := 0, is_last: []bool = {}) {
}

print_type :: proc(type_node: Maybe(^TypeNode), indent := 0, is_last: []bool = {}) {
	print_indent(indent, is_last)

	node, ok := type_node.?
	if !ok {
		fmt.printf("type: %s%v%s\n", logger.color_codes[.blue], node, logger.color_codes[.reset]) // prints <nil>
		return
	}

	switch n in node {
	case LiteralType:
		fmt.printf("type: %s%v%s\n", logger.color_codes[.blue], n, logger.color_codes[.reset])
	case NamedType:
		fmt.printf("type: %s%v%s\n", logger.color_codes[.blue], n.name, logger.color_codes[.reset])
	case PointerType:
		fmt.printf(
			"type: %s%v%s\n",
			logger.color_codes[.blue],
			n.pointee,
			logger.color_codes[.reset],
		)
	}
}
