package error

import "core:fmt"

ErrorKind :: enum {
	lexer_unrecognized_char,
	parser_unexpected_token,
	semantic_undefined_identifier,
	semantic_type_mismatch,
	semantic_circular_dependency,
}

SourceLocation :: struct {
	line:   int,
	column: int,
	file:   string,
}

CompilerError :: struct {
	kind:     ErrorKind,
	message:  string,
	location: Maybe(SourceLocation),
	notes:    [dynamic]string, // Additional context
}

ErrorList :: [dynamic]CompilerError

init :: proc() -> ErrorList {
	return make([dynamic]CompilerError)
}

deinit :: proc(errors: ^ErrorList) {
	delete(errors^)
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
		// TODO:
		//   format like: "error: undefined identifier 'x' at main.hon:12:5"
		//   with source context and color coding
	}
}
