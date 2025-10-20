#include "context.h"
#include <stdlib.h>

struct honey_context*
honey_context_create()
{
  struct honey_context* ctx = calloc(1, sizeof(struct honey_context));
  ctx->current_line = 1;
  ctx->current_column = 1;
  return ctx;
}

void
honey_context_destroy(struct honey_context* ctx)
{
  if (!ctx)
    return;

  for (int i = 0; i < ctx->next_token_idx; i += 1) {
    free(ctx->tokens[i].value);
  }
  free(ctx);
}
