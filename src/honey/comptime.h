#ifndef HONEY_COMPTIME_H
#define HONEY_COMPTIME_H

#include "honey/ast.h"
#include "honey/semantic.h"
#include <stdbool.h>

struct honey_comptime_value
{
  enum
  {
    HONEY_COMPTIME_INT,
    HONEY_COMPTIME_FLOAT,
    HONEY_COMPTIME_INVALID, // couldn't evaluate at compile time
  } kind;

  union
  {
    int int_value;
    double float_value;
  };
};

// try to evaluate an expression at compile time
struct honey_comptime_value
honey_comptime_eval(struct honey_ast_node* expr,
                    struct honey_symbol_table* symtab);

// check if an expression comptime-evaluable
bool
honey_is_comptime_evaluable(struct honey_ast_node* expr,
                            struct honey_symbol_table* symtab);

#endif
