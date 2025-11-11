// PARSING ORDER (from tight coupling to loose):
// 1. primary (literals, names, parens)
// 2. unary (-, not)
// 3. multiplicative (*, /)
// 4. additive (+, -)
// 5. comparison (<, >, <=, >=, ==, !=)
// 6. logical and
// 7. logical or
// 8. expression (pretty much just wraps logical or)
// 9. statement

package parser

import "../error"
import "../lexer"
import "../logger"
import "../scope"

import "core:fmt"

Token :: lexer.Token
TokenKind :: lexer.TokenKind

LOG_SCOPE :: scope.Scope.parser

ParserError :: enum {}

Parser :: struct {
	ast:            ^AstNode,
	tokens:         []Token,
	next_token_idx: int,
	errors:         ^error.ErrorList,
}

init :: proc(tokens: []Token, errors: ^error.ErrorList) -> Parser {
	return Parser{ast = nil, tokens = tokens, next_token_idx = 0, errors = errors}
}

deinit :: proc(p: ^Parser) {
	ast_destroy(p.ast)
}

parse_primary :: proc(p: ^Parser) -> (^AstNode, bool) {
	tok, ok := peek(p).?
	if !ok {
		report_error(
			p,
			.parser_expected_primary_expr,
			"expected expression, found unexpected end of file",
		)
		return {}, false
	}

	node: ^AstNode

	#partial switch tok.kind {
	case .identifier:
		// check if function call
		if check(p, .left_paren) {
			advance(p)
			node, ok = parse_expr(p)
		} else {
			node, ok = make_identifier(tok)
		}


	case .boolean:
		node, ok = make_boolean(tok)

	case .number:
		node, ok = make_number(tok)

	case .left_paren:
		advance(p) // consume the left parenthesis
		node, ok = parse_expr(p)

	case:
		// unexpected token type
		report_error(p, .parser_unexpected_token, "unexpected token '%v' in expression", tok.kind)
		return nil, false
	}

	advance(p)
	return node, ok
}

parse_unary :: proc(p: ^Parser) -> (^AstNode, bool) {
	tok, ok := peek(p).?
	if !ok {
		report_error(
			p,
			.parser_expected_unary_expr,
			"expected expression, found unexpected end of file",
		)
		return {}, false
	}

	if check(p, .minus) || check(p, .logical_not) {
		advance(p)

		// resolve operation
		op := resolve_unary_op_kind(p, tok.kind).?
		if !ok do return nil, false // error already reported in resolve_unary_op_kind

		// recursively call parse_unary to allow chaining
		operand, ok := parse_unary(p)
		if !ok do return nil, false

		return make_unary(operand, op), true
	}

	// else parse primary
	return parse_primary(p)
}

parse_multiplicative :: proc(p: ^Parser) -> (^AstNode, bool) {
	left, ok := parse_unary(p)
	if !ok do return nil, false // error already reported in parse_unary

	// expect multiplication or division
	for check(p, .star) || check(p, .slash) {
		tok, ok := peek(p).?
		if !ok do break

		op := resolve_binary_op_kind(p, tok.kind).?
		advance(p)

		right: ^AstNode
		right, ok = parse_unary(p)
		if !ok {
			// error already reported in parse_unary
			ast_destroy(left)
			return nil, false
		}

		left = make_binary(left, right, op)
	}

	return left, true
}

parse_additive :: proc(p: ^Parser) -> (^AstNode, bool) {
	left, ok := parse_multiplicative(p)
	if !ok do return nil, false // error already reported in parse_multiplicative

	// expect addition or subtraction
	for check(p, .plus) || check(p, .minus) {
		tok, ok := peek(p).?
		if !ok do break

		op := resolve_binary_op_kind(p, tok.kind).?
		advance(p)

		right: ^AstNode
		right, ok = parse_multiplicative(p)
		if !ok {
			// error already reported in parse_multiplicative
			ast_destroy(left)
			return nil, false
		}

		left = make_binary(left, right, op)
	}

	return left, true
}

parse_comparative :: proc(p: ^Parser) -> (^AstNode, bool) {
	left, ok := parse_additive(p)
	if !ok do return nil, false // error already reported in parse_additive

	// expect comparison
	for check(p, .double_equal) ||
	    check(p, .not_equal) ||
	    check(p, .less) ||
	    check(p, .greater) ||
	    check(p, .less_equal) ||
	    check(p, .greater_equal) {
		tok, ok := peek(p).?
		if !ok do break

		op, ok_op := resolve_binary_op_kind(p, tok.kind).?
		if !ok_op do return nil, false // error reported in resolve_binary_op_kind

		advance(p)

		right: ^AstNode
		right, ok = parse_additive(p)
		if !ok {
			// error already reported in parse_additive
			ast_destroy(left)
			return nil, false
		}

		left = make_binary(left, right, op)
	}

	return left, true
}

parse_logical_and :: proc(p: ^Parser) -> (^AstNode, bool) {
	left, ok := parse_comparative(p)
	if !ok do return nil, false // error already reported in parse_comparative

	// expect only logical and
	for check(p, .logical_and) {
		tok, ok := peek(p).?
		if !ok do break

		op, ok_op := resolve_binary_op_kind(p, tok.kind).?
		if !ok_op do return nil, false // error reported in resolve_binary_op_kind

		advance(p)

		right: ^AstNode
		right, ok = parse_comparative(p)
		if !ok {
			// error already reported in parse_comparative
			ast_destroy(left)
			return nil, false
		}

		left = make_binary(left, right, op)
	}

	return left, true
}

parse_logical_or :: proc(p: ^Parser) -> (^AstNode, bool) {
	left, ok := parse_logical_and(p)
	if !ok do return nil, false // error already reported in parse_logical_and

	// expect only logical or
	for check(p, .logical_or) {
		tok, ok := peek(p).?
		if !ok do break

		op, ok_op := resolve_binary_op_kind(p, tok.kind).?
		if !ok_op do return nil, false // error reported in resolve_binary_op_kind

		advance(p)

		right: ^AstNode
		right, ok = parse_logical_and(p)
		if !ok {
			// error already reported in parse_logical_and
			ast_destroy(left)
			return nil, false
		}

		left = make_binary(left, right, op)
	}

	return left, true
}

parse_expr :: proc(p: ^Parser) -> (^AstNode, bool) {
	return parse_logical_or(p)
}

parse_const_decl :: proc(p: ^Parser) -> (^AstNode, bool) {
	tok_start, ok := peek(p).?
	if !ok {
		report_error(p, .parser_unexpected_eof, "unexpected end of file")
		return nil, false
	}
	loc := tok_start.loc

	// expect name
	name, ok_ident := parse_identifier(p, "expected identifier in comptime const declaration")
	if !ok_ident do return nil, false // error reported by parse_identifier

	// parse optional type
	type: ^TypeNode = nil
	if match(p, .colon) {
		type, ok = parse_type(p, "expected type in comptime const declaration")
		if !ok do return nil, false // error reported by parse_type
	}

	// parse '::'
	tok, ok_tok := peek(p).?
	if !match(p, .double_colon) {
		if ok_tok {
			report_error(
				p,
				.parser_unexpected_token,
				"expected '::' in comptime const declaration, found '%v'",
				tok.kind,
			)
		} else {
			report_error(
				p,
				.parser_unexpected_eof,
				"expected '::' in comptime const declaration, found unexpected end of file",
			)
		}
		free(type)
		return nil, false
	}

	// parse value
	value, ok_val := parse_expr(p)
	if !ok_val {
		report_error(
			p,
			.parser_unexpected_token,
			"expected expression after '::' in comptime const declaration",
		)
		free(type)
		return nil, false
	}

	// create node with all fields at once
	return make_decl(name, type, value, .const, loc), true
}

parse_func_decl :: proc(p: ^Parser) -> (^AstNode, bool) {
	tok_start, ok := peek(p).?
	if !ok {
		report_error(p, .parser_unexpected_eof, "unexpected end of file")
		return nil, false
	}
	loc := tok_start.loc

	// expect name
	name, ok_ident := parse_identifier(p, "expected identifier in function declaration")
	if !ok_ident do return nil, false // error reported by parse_identifier

	// parse '::'
	tok, ok_tok := peek(p).?
	if !match(p, .double_colon) {
		if ok_tok {
			report_error(
				p,
				.parser_unexpected_token,
				"expected '::' in function declaration, found '%v'",
				tok.kind,
			)
		} else {
			report_error(
				p,
				.parser_unexpected_eof,
				"expected '::' in function declaration, found unexpected end of file",
			)
		}
		return nil, false
	}

	// expect 'func' keyword
	tok, ok_tok = peek(p).?
	if !match(p, .func) {
		if ok_tok {
			report_error(
				p,
				.parser_unexpected_token,
				"expected 'func' in function declaration, found '%v'",
				tok.kind,
			)
		} else {
			report_error(
				p,
				.parser_unexpected_eof,
				"expected 'func' in function declaration, found unexpected end of file",
			)
		}
		return nil, false
	}

	// expect '('
	tok, ok_tok = peek(p).?
	if !match(p, .left_paren) {
		if ok_tok {
			report_error(
				p,
				.parser_unexpected_token,
				"expected '(' in function declaration, found '%v'",
				tok.kind,
			)
		} else {
			report_error(
				p,
				.parser_unexpected_eof,
				"expected '(' in function declaration, found unexpected end of file",
			)
		}
		return nil, false
	}

	// expect parameter list
	has_more := true
	parameters := make([dynamic]Parameter)
	for has_more && check(p, .identifier) {
		// expect parameter name
		param_name, ok_pname := parse_identifier(p, "expected identifier in function parameters")
		if !ok_pname {
			delete(parameters)
			for p in parameters do type_destroy(p.type)
			return nil, false
		}

		// parse ':'
		tok, ok_tok = peek(p).?
		if !match(p, .colon) {
			if ok_tok {
				report_error(
					p,
					.parser_unexpected_token,
					"expected ':' in function declaration, found '%v'",
					tok.kind,
				)
			} else {
				report_error(
					p,
					.parser_unexpected_eof,
					"expected ':' in function declaration, found unexpected end of file",
				)
			}
			return nil, false
		}

		// expect parameter type
		param_type, ok_ptype := parse_type(p, "expected type in function parameters")
		if !ok_ptype {
			delete(parameters)
			for p in parameters do type_destroy(p.type)
			return nil, false
		}

		append(&parameters, Parameter{name = param_name, type = param_type})

		has_more = check(p, .comma)
		if has_more do advance(p)
	}

	// expect ')'
	tok, ok_tok = peek(p).?
	if !match(p, .right_paren) {
		if ok_tok {
			report_error(
				p,
				.parser_unexpected_token,
				"expected ')' in function declaration, found '%v'",
				tok.kind,
			)
		} else {
			report_error(
				p,
				.parser_unexpected_eof,
				"expected ')' in function declaration, found unexpected end of file",
			)
		}
		delete(parameters)
		for p in parameters do type_destroy(p.type)
		return nil, false
	}

	// expect return type
	return_type, ok_type := parse_type(p, "expected return type in function declaration")
	if !ok_type {
		// error already reported in parse_type
		for p in parameters do type_destroy(p.type)
		delete(parameters)
		return nil, false
	}


	// parse function body
	block, ok_block := parse_block(p, "expected function body")
	if !ok_block {
		// error already reported in parse_block
		for p in parameters do type_destroy(p.type)
		delete(parameters)
		type_destroy(return_type)
		return nil, false
	}

	return make_decl(name, return_type, make_func(parameters, block), .func, loc), true
}

parse_block :: proc(p: ^Parser, err_msg: string) -> (^Block, bool) {
	// expect '{'
	tok, ok_tok := peek(p).?
	if !match(p, .left_curly) {
		if ok_tok {
			report_error(
				p,
				.parser_unexpected_token,
				"expected '{' in block, found '%v'",
				tok.kind,
			)
		} else {
			report_error(
				p,
				.parser_unexpected_eof,
				"expected '{' in block, found unexpected end of file",
			)
		}
		return nil, false
	}

	// parse all statements
	statements := make([dynamic]^AstNode)
	deferred := make([dynamic]^AstNode)
	for !check(p, .right_curly) && !check(p, .eof) {
		stmt, ok_stmt := parse_statement(p)
		if !ok_stmt {
			delete(statements)
			delete(deferred)
			return nil, false
		}

		#partial switch s in stmt {
		case DeferStmt:
			append(&deferred, stmt)
		case:
			append(&statements, stmt)
		}
	}

	// expect '}'
	tok, ok_tok = peek(p).?
	if !match(p, .right_curly) {
		if ok_tok {
			report_error(
				p,
				.parser_unexpected_token,
				"expected '}' in block, found '%v'",
				tok.kind,
			)
		} else {
			report_error(
				p,
				.parser_unexpected_eof,
				"expected '}' in block, found unexpected end of file",
			)
		}

		delete(statements)
		delete(deferred)
		return nil, false
	}

	return make_block(statements, deferred), true
}

parse_statement :: proc(p: ^Parser) -> (^AstNode, bool) {
	if check(p, .return_) do return parse_return_stmt(p)

	report_error(p, .parser_expected_statement, "expected statement")
	return nil, false
}

parse_return_stmt :: proc(p: ^Parser) -> (^AstNode, bool) {
	tok, ok_tok := peek(p).?
	if !match(p, .return_) {
		if ok_tok {
			logger.error(LOG_SCOPE, "expected 'return' in return statement, found '%v'", tok.kind)
		} else {
			logger.error(
				LOG_SCOPE,
				"expected 'return' in return statement, found unexpected end of file",
			)
		}
		return nil, false
	}

	expr, ok_expr := parse_expr(p)
	if !ok_expr {
		report_error(p, .parser_expected_expression, "expected expression in statement")
		return nil, false
	}

	return make_return_stmt(expr), true
}

parse_call_expr :: proc(p: ^Parser) -> (^AstNode, bool) {
	// parse function name
	func_name, ok_name := parse_identifier(p, "expected identifier in call expression")
	if !ok_name do return nil, false

	// expect '('
	tok, ok_tok := peek(p).?
	if !match(p, .left_paren) {
		if ok_tok {
			report_error(
				p,
				.parser_unexpected_token,
				"expected '(' in call expression, found '%v'",
				tok.kind,
			)
		} else {
			report_error(
				p,
				.parser_unexpected_eof,
				"expected '(' in call expression, found unexpected end of file",
			)
		}
		return nil, false
	}

	// parse argument list
	arguments := make([dynamic]^AstNode)

	// check if we have arguments
	if !check(p, .right_paren) {
		for {
			arg, ok_arg := parse_expr(p)
			if !ok_arg {
				// error already reported by parse_expr
				for a in arguments do ast_destroy(a)
				delete(arguments)
				return nil, false
			}

			append(&arguments, arg)

			if !check(p, .comma) do break // no comma, done with arguments
			advance(p)
		}
	}

	// expect ')'
	tok, ok_tok = peek(p).?
	if !match(p, .right_paren) {
		if ok_tok {
			report_error(
				p,
				.parser_unexpected_token,
				"expected ')' in call expression, found '%v'",
				tok.kind,
			)
		} else {
			report_error(
				p,
				.parser_unexpected_eof,
				"expected ')' in call expression, found unexpected end of file",
			)
		}
		for a in arguments do ast_destroy(a)
		delete(arguments)
		return nil, false
	}

	return make_call_expr(func_name, arguments), true
}

// expect identifier and return its value
parse_identifier :: proc(p: ^Parser, err_msg: string) -> (string, bool) {
	tok, ok := peek(p).?
	if !ok {
		report_error(
			p,
			.parser_expected_identifier,
			fmt.tprintf("%s, found unexpected end of file", err_msg),
		)
		return {}, false
	}

	if !match(p, .identifier) {
		report_error(
			p,
			.parser_expected_identifier,
			fmt.tprintf("%s, found '%v'", err_msg, tok.kind),
		)
		return {}, false
	}

	name, ok_name := tok.value.?
	if !ok_name {
		report_error(p, .parser_expected_identifier, "expected name in identifier, found nothing")
		return {}, false
	}

	return name, true
}

// parse a type node
parse_type :: proc(p: ^Parser, err_msg: string) -> (^TypeNode, bool) {
	tok, ok := peek(p).?
	if !ok {
		report_error(
			p,
			.parser_expected_type,
			fmt.tprintf("%s, found unexpected end of file", err_msg),
		)
		return {}, false
	}

	if !match(p, .identifier) {
		report_error(p, .parser_expected_type, fmt.tprintf("%s, found '%v'", err_msg, tok.kind))
		return nil, false
	}

	return make_type(tok)
}

parse_decl :: proc(p: ^Parser) -> (^AstNode, bool) {
	if check(p, .identifier) && check_offset(p, 1, .double_colon) && check_offset(p, 2, .func) {
		return parse_func_decl(p)
	} else if check(p, .identifier) &&
	   (check_offset(p, 1, .colon) || check_offset(p, 1, .double_colon)) {
		return parse_const_decl(p)
	}

	// failed to parse declaration
	tok, ok := peek(p).?
	if ok {
		report_error(p, .parser_unexpected_token, "expected declaration, found '%v'", tok.kind)
	} else {
		report_error(
			p,
			.parser_unexpected_eof,
			"expected declaration, found unexpected end of file",
		)
	}

	return nil, false
}

parse_program :: proc(p: ^Parser) -> (^AstNode, bool) {
	declarations := make([dynamic]Declaration)

	for {
		if tok, ok := peek(p).?; tok.kind == .eof {
			advance(p) // consume for good measure
			break // successfully parsed all tokens
		} else if !ok {
			report_error(p, .parser_unexpected_eof, "unexpected end of file")
			return {}, false
		}

		node, ok := parse_decl(p)
		if !ok {
			return {}, false
			// // error already reported, try to recover by skipping to next declaration
			// synchronize(p)
			// continue
		}

		#partial switch n in node {
		case Declaration:
			append(&declarations, n)

		case:
			report_error(p, .parser_unexpected_token, "expected declaration in program")
			return {}, false
		// synchronize(p)
		// continue
		}
	}

	return make_program(declarations), true
}

parse :: proc(p: ^Parser) -> bool {
	program, ok := parse_program(p)
	p.ast = program
	return ok
}
