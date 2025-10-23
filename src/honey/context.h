#ifndef HONEY_CONTEXT_H
#define HONEY_CONTEXT_H

#include "honey/token.h"

#define HONEY_MAX_TOKENS 1024

struct honey_context
{
  const char* src;
  int next_src_idx;
  int next_token_idx;
  struct honey_token tokens[HONEY_MAX_TOKENS];

  // for error reporting
  int current_line;
  int current_column;
};

struct honey_context*
honey_context_create();

void
honey_context_destroy(struct honey_context* lexer);

#endif
