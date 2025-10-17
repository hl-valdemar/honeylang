#ifndef HONEY_SEMANTIC_H
#define HONEY_SEMANTIC_H

#include "ast.h"
#include <stdbool.h>
#include <stdint.h>

enum honey_type_kind
{
  TYPE_VOID,
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
  TYPE_FUNCTION,
  TYPE_UNKNOWN,
};

struct honey_type
{
  enum honey_type_kind kind;

  // for function types
  struct
  {
    struct honey_type* return_type;
    struct honey_type** param_types;
    int param_count;
  } func;
};

enum honey_symbol_kind
{
  SYMBOL_COMPTIME,
  SYMBOL_FUNCTION,
};

struct honey_symbol
{
  char* name;
  enum honey_symbol_kind kind;
  struct honey_type type;

  // for constants
  union
  {
    int64_t int_value;
    double float_value;
  } comptime_value;

  // for functions, keep reference to ast node
  struct honey_ast_node* func_node;
};

#define HONEY_MAX_SYMBOLS 256

struct honey_symbol_table
{
  struct honey_symbol symbols[HONEY_MAX_SYMBOLS];
  int count;
};

// analyze the ast and build symbol table
bool
honey_analyze(struct honey_ast_node** declarations,
              int count,
              struct honey_symbol_table* symtab);

void
honey_symbol_table_print(struct honey_symbol_table* symtab);

// helper to resolve type names
enum honey_type_kind
honey_resolve_type_name(const char* name);

const char*
honey_type_kind_to_string(enum honey_type_kind kind);

#endif
