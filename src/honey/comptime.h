#ifndef HONEY_COMPTIME_H
#define HONEY_COMPTIME_H

#include "honey/ast.h"
#include <stdbool.h>
#include <stdint.h>

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
    int64_t int_value;
    double float_value;
  };
};

struct honey_comptime_local_var
{
  char* name;
  struct honey_comptime_value value;
};

#define MAX_COMPTIME_RECURSION 1000
#define MAX_COMPTIME_LOCALS 256

struct honey_comptime_execution_context
{
  struct honey_symbol_table* global_symtab;

  // local variables during execution
  struct honey_comptime_local_var* locals;
  int locals_count;
  int locals_capacity;

  // function parameters
  struct honey_comptime_local_var* params;
  int params_count;

  // return value (when return statement is hit)
  struct honey_comptime_value return_value;
  bool has_returned;

  // recursion depth tracking
  int recursion_depth;
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
