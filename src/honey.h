#ifndef HONEY_H
#define HONEY_H

#include "token.h"

#define HONEY_MAX_TOKEN_COUNT 1024

struct honey_context
{
  const char* src;
  int next_src_idx;
  int next_token_idx;
  struct honey_token tokens[HONEY_MAX_TOKEN_COUNT];

  // for error reporting
  int current_line;
  int current_column;
};

void
honey_scan(struct honey_context* ctx, const char* src);

struct honey_context*
honey_create_context();

void
honey_destroy_context(struct honey_context* ctx);

#endif
