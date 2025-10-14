#include "ast.h"
#include "honey.h"
#include "parser.h"
#include <stdio.h>

const char* test_cases[] = {
  "SOME_VALUE :: 42",

  "main : func : i32 :: {\n"
  "  return 42\n"
  "}",

  "add : func(a : i32, b : i32) : i32 :: {\n"
  "  return a\n"
  "}",

  "no_params : func : i32 :: {\n"
  "  return 10\n"
  "}",

  "no_return : func :: {\n"
  "  return\n"
  "}",

  "no_return : func :: { }",
};

int
main(void)
{
  int num_tests = sizeof(test_cases) / sizeof(test_cases[0]);

  for (int i = 0; i < num_tests; i++) {
    printf("=== TEST %d ===\n", i + 1);
    printf("source: %s\n\n", test_cases[i]);

    struct honey_context* honey_ctx = honey_create_context();
    honey_scan(honey_ctx, test_cases[i]);

    printf("tokens:\n");
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

    printf("\nAST:\n");
    struct honey_ast_node* ast = honey_parse(honey_ctx);
    if (ast) {
      honey_ast_print(ast, 1);
      honey_ast_destroy(ast);
    } else {
      printf("  (parsing failed)\n");
    }

    honey_destroy_context(honey_ctx);
    printf("\n");
  }

  return 0;
}
