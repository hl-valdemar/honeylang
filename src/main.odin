package main

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

	l := lexer.init(source)
	defer lexer.deinit(&l)

	if ok := lexer.scan(&l); !ok {
		logger.fatal(LOG_SCOPE, "failed to scan honey source code")
		os.exit(1)
	}

	fmt.printf("\n::[[ LEXING ]]::\n")
	fmt.printf("Generated %d tokens:\n\n", len(l.tokens))
	for tok in l.tokens do fmt.println(lexer.token_to_string(tok))

	p := parser.init(l.tokens[:])
	defer parser.deinit(&p)

	if ok := parser.parse(&p); !ok {
		logger.fatal(LOG_SCOPE, "failed to parse tokens")
		os.exit(1)
	}

	fmt.printf("\n::[[ PARSING ]]::\n")
	fmt.printf("Parsed %d declarations:\n\n", len(p.ast.(parser.Program).declarations))
	parser.print_ast(p.ast)

	s := semantic.init(p.ast)
	defer semantic.deinit(&s)

	semantic.analyze(&s)

	fmt.printf("\n::[[ SEMANTIC ANALYSIS ]]::\n")
	fmt.printf("Collected %d symbols:\n\n", len(s.symtab.symbols[:]))
	semantic.print_symtab(&s.symtab)

	fmt.printf("\nTyped AST:\n\n")
	parser.print_ast(s.program)

	fmt.printf("\n::[[ CODE EMISSION ]]::\n")
	fmt.printf("Nothing yet to see...\n\n")
}
