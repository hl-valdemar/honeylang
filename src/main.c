#include "honey.h"
#include <stdio.h>

const char* test_src = "# This is a comment\n"
                       "SOME_NAME :: 10\n"
                       "OTHER_NAME :: 3.14\n"
                       "test_value : f32 = 2.5\n";

int
main(void)
{
  struct honey_context* honey_ctx = honey_create_context();

  honey_scan(honey_ctx, test_src);

  for (int i = 0; i < honey_ctx->next_token_idx; i += 1) {
    struct honey_token* tok = &honey_ctx->tokens[i];

    printf("[%d:%d] %s",
           tok->line,
           tok->column,
           honey_token_kind_to_text(tok->kind));

    if (tok->data.value) {
      printf("(\"%s\")", tok->data.value);
    }
    printf("\n");
  }

  honey_destroy_context(honey_ctx);
  return 0;
}
