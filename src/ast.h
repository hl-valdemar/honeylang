#ifndef HONEY_AST_H
#define HONEY_AST_H

#include <stdint.h>

enum honey_ast_kind
{
  AST_CONST_DECL,
  AST_LITERAL_INT,
  AST_LITERAL_FLOAT,
  AST_NAME,
};

struct honey_ast_node
{
  enum honey_ast_kind kind;

  union
  {
    struct
    {
      struct honey_ast_node* name;
      struct honey_ast_node* type;
      struct honey_ast_node* value;
    } comptime_decl;

    int64_t i64_literal;

    double f64_literal;

    char* name;
  } data;
};

struct honey_ast_node*
honey_ast_create(enum honey_ast_kind kind);

void
honey_ast_destroy(struct honey_ast_node* node);

#endif
