package main

import "lexer"
import "logger"
import "parser"
import "scope"
import "semantic"

import "core:fmt"
import "core:os"

Scope :: scope.Scope

LOG_SCOPE :: Scope.pipeline

main :: proc() {
	logger.init({term_enabled = true})
	defer logger.deinit()

	when ODIN_DEBUG {
		logger.enable_file_logging("logs.txt")
	}

	l := lexer.init("DEBUG :: true")
	defer lexer.deinit(&l)

	if ok := lexer.scan(&l); !ok {
		logger.fatal(LOG_SCOPE, "failed to scan honey source code")
		os.exit(1)
	}

	fmt.printf("\n::[[ lexing ]]::\n")
	fmt.printf("generated %d tokens:\n\n", len(l.tokens))
	for tok in l.tokens do fmt.println(lexer.token_to_string(tok))

	p := parser.init(l.tokens[:])
	defer parser.deinit(&p)

	if ok := parser.parse(&p); !ok {
		logger.fatal(LOG_SCOPE, "failed to parse tokens")
		os.exit(1)
	}

	fmt.printf("\n::[[ parsing ]]::\n")
	fmt.printf("parsed %d declarations:\n\n", len(p.ast.(parser.Program).declarations))
	parser.print_ast(p.ast)

	s := semantic.init(p.ast)
	defer semantic.deinit(&s)

	fmt.printf("\n::[[ semantic analysis ]]::\n")
	fmt.printf("collected %d symbols:\n\n", 1)


	fmt.printf("\n::[[ code emission ]]::\n")
	fmt.printf("nothing yet to see...\n\n")
}
