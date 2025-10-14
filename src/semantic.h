#ifndef HONEY_SEMANTIC_H
#define HONEY_SEMANTIC_H

#include "ast.h"
#include <stdbool.h>
#include <stdint.h>

enum honey_type
{
  TYPE_I8,
  TYPE_I16,
  TYPE_I32,
  TYPE_I64,
  TYPE_U8,
  TYPE_U16,
  TYPE_U32,
  TYPE_U64,
  TYPE_F32,
  TYPE_F64,
  TYPE_UNKNOWN,
};

struct honey_symbol
{
  char* name;
  enum honey_type type;

  // compile-time constant value
  bool is_comptime;
  union
  {
    int64_t int_value;
    double float_value;
  } comptime_value;
};

#define HONEY_MAX_SYMBOLS 256

struct honey_symbol_table
{
  struct honey_symbol symbols[HONEY_MAX_SYMBOLS];
  int count;
};

// analyze the ast and build symbol table
bool
honey_analyze(struct honey_ast_node* ast, struct honey_symbol_table* symtab);

void
honey_symbol_table_print(struct honey_symbol_table* symtab);

#endif
