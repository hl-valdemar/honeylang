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

	tokens, ok := lexer.scan(&l)
	if !ok {
		logger.fatal(LOG_SCOPE, "failed to scan honey source code")
		os.exit(1)
	}

	fmt.printf("\n::[[ lexing ]]::\n")
	fmt.printf("generated %d tokens:\n\n", len(tokens))
	for tok in tokens do fmt.println(lexer.token_to_string(tok))

	p := parser.init(tokens[:])

	ast: ^parser.AstNode
	ast, ok = parser.parse(&p)
	if !ok {
		logger.fatal(LOG_SCOPE, "failed to parse tokens")
		os.exit(1)
	}

	fmt.printf("\n::[[ parsing ]]::\n")
	fmt.printf("generated ast:\n\n")
  parser.print_ast(ast);
}
