package error

import "core:fmt"

ErrorKind :: enum {
	lexer_unrecognized_char,

	// parser errors
	parser_unexpected_token,
	parser_unexpected_eof,
	parser_expected_identifier,
	parser_expected_type,
	parser_expected_expression,
	parser_expected_primary_expr,
	parser_expected_unary_expr,
	parser_expected_multiplicative_expr,
	parser_expected_additive_expr,
	parser_expected_statement,

	// semantic errors
	semantic_undefined_identifier,
	semantic_duplicate_definition,
	semantic_type_mismatch,
	semantic_circular_dependency,
}

SourceLocation :: struct {
	file:   string,
	line:   int,
	column: int,
}

CompilerError :: struct {
	kind:     ErrorKind,
	message:  string,
	location: Maybe(SourceLocation),
	notes:    [dynamic]string, // additional context
}

ErrorList :: [dynamic]CompilerError

init :: proc() -> ErrorList {
	return make([dynamic]CompilerError)
}

deinit :: proc(errors: ErrorList) {
	delete(errors)
}

// add error to list
add_error :: proc(
	errors: ^ErrorList,
	kind: ErrorKind,
	loc: Maybe(SourceLocation),
	fmt_str: string,
	args: ..any,
) {
	err := CompilerError {
		kind     = kind,
		message  = fmt.tprintf(fmt_str, ..args),
		location = loc,
		notes    = make([dynamic]string),
	}
	append(errors, err)
}

// check if compilation should stop
has_errors :: proc(errors: ^ErrorList) -> bool {
	return len(errors) > 0
}

// pretty print errors
print_errors :: proc(errors: ^ErrorList) {
	for err in errors {
		if loc, ok := err.location.?; ok {
			fmt.printf("error: %s at %s:%d:%d\n", err.message, loc.file, loc.line, loc.column)
		} else {
			fmt.printf("error: %s\n", err.message)
		}
	}
}
