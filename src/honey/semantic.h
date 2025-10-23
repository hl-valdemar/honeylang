#ifndef HONEY_SEMANTIC_H
#define HONEY_SEMANTIC_H

#include "honey/ast.h"
#include "honey/comptime.h"
#include <stdbool.h>
#include <stdint.h>

enum honey_type_kind
{
  HONEY_TYPE_VOID,
  HONEY_TYPE_I8,
  HONEY_TYPE_I16,
  HONEY_TYPE_I32,
  HONEY_TYPE_I64,
  HONEY_TYPE_U8,
  HONEY_TYPE_U16,
  HONEY_TYPE_U32,
  HONEY_TYPE_U64,
  HONEY_TYPE_F32,
  HONEY_TYPE_F64,
  HONEY_TYPE_FUNCTION,
  HONEY_TYPE_UNKNOWN,
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
  HONEY_SYMBOL_COMPTIME,
  HONEY_SYMBOL_FUNCTION,
  HONEY_SYMBOL_TEST,
};

struct honey_symbol
{
  char* name;
  enum honey_symbol_kind kind;
  struct honey_type type;

  // for constants
  struct honey_comptime_value comptime_value;

  // for functions
  struct
  {
    struct honey_ast_node* func_node;
    bool is_comptime;
  } func;

  // for test scopes, keep reference to ast node
  struct honey_ast_node* test_node;
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
honey_type_kind_to_text(enum honey_type_kind kind);

// get the size in bytes for a type
int
honey_type_size(enum honey_type_kind type);

#endif
