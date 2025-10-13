#include "honey.h"
#include <stdio.h>

const char* test_src = "SOME_NAME :: 10";

int
main(void)
{
  struct honey_context* honey_ctx = honey_create_context();

  honey_scan(honey_ctx, test_src);

  for (int i = 0; i < honey_ctx->next_token_idx; i += 1) {
    enum honey_token_kind kind = honey_ctx->tokens[i].kind;

    printf("Token %d:\n", i);
    printf("  kind: %s\n", honey_token_kind_to_text(kind));
    switch (kind) {
      case HONEY_TOKEN_NAME:
      case HONEY_TOKEN_INT:
      case HONEY_TOKEN_FLOAT:
        printf("  data: %s\n", honey_ctx->tokens[i].data.value);
    }
  }

  honey_destroy_context(honey_ctx);
}
