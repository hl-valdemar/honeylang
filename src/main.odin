package main

import "error"
import "lexer"
import "logger"
import "parser"
import "scope"
import "semantic"

import "core:fmt"
import "core:os"

LOG_SCOPE :: scope.Scope.pipeline

main :: proc() {
	logger.init({term_enabled = true})
	defer logger.deinit()

	when ODIN_DEBUG {
		logger.enable_file_logging("logs.txt")
	}

	// get source file
	if len(os.args) < 2 {
		logger.fatal(LOG_SCOPE, "usage: %s <source-file>", os.args[0])
		os.exit(1)
	}

	// read source file
	filepath := os.args[1]
	source_bytes, ok := os.read_entire_file(filepath)
	if !ok {
		logger.fatal(LOG_SCOPE, "failed to read file: %s", filepath)
		os.exit(1)
	}
	defer delete(source_bytes)

	source := string(source_bytes)

	fmt.printf("\nCompiling: %s\n", filepath)

	fmt.printf("\n%s::[[ SOURCE ]]::%s\n", logger.color_codes[.yellow], logger.color_codes[.reset])
	fmt.printf("%s%s%s", logger.color_codes[.cyan], source, logger.color_codes[.reset])

	// track errors
	errors := error.init()
	defer error.deinit(errors)

	fmt.printf("\n%s::[[ LEXING ]]::%s\n", logger.color_codes[.yellow], logger.color_codes[.reset])

	l := lexer.init(filepath, source, &errors)
	defer lexer.deinit(&l)

	lexer.scan(&l)
	if error.has_errors(&errors) {
		error.print_errors(&errors)
		os.exit(1)
	}

	fmt.printf("Generated %d tokens:\n\n", len(l.tokens))
	lexer.print_tokens(&l.tokens)

	fmt.printf(
		"\n%s::[[ PARSING ]]::%s\n",
		logger.color_codes[.yellow],
		logger.color_codes[.reset],
	)

	p := parser.init(l.tokens[:], &errors)
	defer parser.deinit(&p)

	parser.parse(&p)
	if error.has_errors(&errors) {
		error.print_errors(&errors)
		os.exit(1)
	}

	fmt.printf("Parsed %d declarations:\n\n", len(p.ast.(parser.Program).declarations))
	parser.ast_print(p.ast)

	fmt.printf(
		"\n%s::[[ SEMANTIC ANALYSIS ]]::%s\n",
		logger.color_codes[.yellow],
		logger.color_codes[.reset],
	)

	s := semantic.init(p.ast)
	defer semantic.deinit(&s)

	semantic.analyze(&s)

	fmt.printf("Collected %d symbols:\n\n", len(s.symtab.symbols[:]))
	semantic.print_symtab(&s.symtab)

	fmt.printf("\nResulting AST:\n\n")
	parser.ast_print(s.program)

	fmt.printf(
		"\n%s::[[ CODE EMISSION ]]::%s\n",
		logger.color_codes[.yellow],
		logger.color_codes[.reset],
	)
	fmt.printf("Nothing yet to see...\n\n")
}
