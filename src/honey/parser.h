#ifndef HONEY_PARSER_H
#define HONEY_PARSER_H

#include "ast.h"
#include <stdbool.h>

struct honey_parser
{
  struct honey_context* ctx;
  int current_token;
  bool had_error;
};

// parse the entire token stream into a list of declarations
// returns an array of AST nodes (caller must free)
struct honey_ast_node**
honey_parse(struct honey_context* ctx, int* out_count);

#endif
