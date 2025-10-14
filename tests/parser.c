#include <honey/ast.h>
#include <honey/codegen/arm64.h>
#include <honey/lexer.h>
#include <honey/log.h>
#include <honey/parser.h>
#include <honey/semantic.h>
#include <stdio.h>
#include <stdlib.h>

const char* test_program = "SOME_VALUE :: 42\n"
                           "\n"
                           "main : func : i32 :: {\n"
                           "  return SOME_VALUE\n"
                           "}\n";

int
main(void)
{
  printf("=== Source Code ===\n");
  printf("%s\n", test_program);

  // lexing
  printf("=== Lexing ===\n");
  struct honey_context* honey_ctx = honey_context_create();
  honey_scan(honey_ctx, test_program);
  printf("generated %d tokens:\n", honey_ctx->next_token_idx);
  for (int j = 0; j < honey_ctx->next_token_idx; j++) {
    struct honey_token* tok = &honey_ctx->tokens[j];
    printf("  [%d:%d] %s",
           tok->line,
           tok->column,
           honey_token_kind_to_text(tok->kind));

    if (tok->data.value) {
      printf(" = \"%s\"", tok->data.value);
    }
    printf("\n");
  }
  printf("\n");

  // parsing
  printf("=== Parsing ===\n");
  int ast_count = 0;
  struct honey_ast_node** declarations = honey_parse(honey_ctx, &ast_count);
  if (!declarations) {
    honey_error("parsing failed");
    honey_context_destroy(honey_ctx);
    return 1;
  }

  printf("parsed %d declarations:\n\n", ast_count);
  for (int i = 0; i < ast_count; i++) {
    honey_ast_print(declarations[i], 0);
    printf("\n");
  }

  // cleanup
  for (int i = 0; i < ast_count; i++) {
    honey_ast_destroy(declarations[i]);
  }
  free(declarations);
  honey_context_destroy(honey_ctx);

  return 0;
}
