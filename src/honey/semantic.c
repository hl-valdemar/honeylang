#define _POSIX_C_SOURCE 200809L
#include "semantic.h"
#include "honey/ast.h"
#include "log.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum honey_type_kind
honey_resolve_type_name(const char* name)
{
  if (strcmp(name, "void") == 0)
    return HONEY_TYPE_VOID;
  if (strcmp(name, "i8") == 0)
    return HONEY_TYPE_I8;
  if (strcmp(name, "i16") == 0)
    return HONEY_TYPE_I16;
  if (strcmp(name, "i32") == 0)
    return HONEY_TYPE_I32;
  if (strcmp(name, "i64") == 0)
    return HONEY_TYPE_I64;
  if (strcmp(name, "u8") == 0)
    return HONEY_TYPE_U8;
  if (strcmp(name, "u16") == 0)
    return HONEY_TYPE_U16;
  if (strcmp(name, "u32") == 0)
    return HONEY_TYPE_U32;
  if (strcmp(name, "u64") == 0)
    return HONEY_TYPE_U64;
  if (strcmp(name, "f32") == 0)
    return HONEY_TYPE_F32;
  if (strcmp(name, "f64") == 0)
    return HONEY_TYPE_F64;

  return HONEY_TYPE_UNKNOWN;
}

const char*
honey_type_kind_to_text(enum honey_type_kind kind)
{
  switch (kind) {
    case HONEY_TYPE_I8:
      return "i8";
    case HONEY_TYPE_I16:
      return "i16";
    case HONEY_TYPE_I32:
      return "i32";
    case HONEY_TYPE_I64:
      return "i64";
    case HONEY_TYPE_U8:
      return "u8";
    case HONEY_TYPE_U16:
      return "u16";
    case HONEY_TYPE_U32:
      return "u32";
    case HONEY_TYPE_U64:
      return "u64";
    case HONEY_TYPE_F32:
      return "f32";
    case HONEY_TYPE_F64:
      return "f64";
    case HONEY_TYPE_VOID:
      return "void";
    case HONEY_TYPE_FUNCTION:
      return "func";
    case HONEY_TYPE_UNKNOWN:
      return "unknown";
  }
  return "unknown";
}

static bool
analyze_comptime_decl(struct honey_ast_node* node,
                      struct honey_symbol_table* symtab)
{
  if (symtab->count >= HONEY_MAX_SYMBOLS) {
    honey_error("too many symbols");
    return false;
  }

  struct honey_symbol* sym = &symtab->symbols[symtab->count];
  symtab->count += 1;
  sym->name = strdup(node->data.comptime_decl.name);
  sym->kind = HONEY_SYMBOL_COMPTIME;

  // infer or check type from value
  if (node->data.comptime_decl.value->kind == HONEY_AST_LITERAL_INT) {
    // if explicit type given, use it; otherwise default to i64
    if (node->data.comptime_decl.explicit_type) {
      sym->type.kind =
        honey_resolve_type_name(node->data.comptime_decl.explicit_type);
      if (sym->type.kind == HONEY_TYPE_UNKNOWN) {
        honey_error("unknown type \"%s\"",
                    node->data.comptime_decl.explicit_type);
        return false;
      }
    } else {
      sym->type.kind = HONEY_TYPE_I64;
    }

    sym->comptime_value.int_value =
      node->data.comptime_decl.value->data.int_literal;

  } else if (node->data.comptime_decl.value->kind == HONEY_AST_LITERAL_FLOAT) {
    if (node->data.comptime_decl.explicit_type) {
      sym->type.kind =
        honey_resolve_type_name(node->data.comptime_decl.explicit_type);
      if (sym->type.kind == HONEY_TYPE_UNKNOWN) {
        honey_error("unknown type \"%s\"",
                    node->data.comptime_decl.explicit_type);
        return false;
      }
    } else {
      sym->type.kind = HONEY_TYPE_F64;
    }

    sym->comptime_value.float_value =
      node->data.comptime_decl.value->data.float_literal;
  } else {
    honey_error("const value must be a literal");
    return false;
  }

  return true;
}

static bool
analyze_func_decl(struct honey_ast_node* node,
                  struct honey_symbol_table* symtab)
{
  if (symtab->count >= HONEY_MAX_SYMBOLS) {
    honey_error("too many symbols");
    return false;
  }

  struct honey_symbol* sym = &symtab->symbols[symtab->count];
  symtab->count += 1;
  sym->name = strdup(node->data.func_decl.name);
  sym->kind = HONEY_SYMBOL_FUNCTION;
  sym->func_node = node;

  // build function type
  sym->type.kind = HONEY_TYPE_FUNCTION;

  // return type
  if (node->data.func_decl.return_type) {
    enum honey_type_kind ret_kind =
      honey_resolve_type_name(node->data.func_decl.return_type);
    if (ret_kind == HONEY_TYPE_UNKNOWN) {
      honey_error("unknown return type \"%s\"",
                  node->data.func_decl.return_type);
      return false;
    }

    sym->type.func.return_type = malloc(sizeof(struct honey_type));
    sym->type.func.return_type->kind = ret_kind;
  } else {
    sym->type.func.return_type = malloc(sizeof(struct honey_type));
    sym->type.func.return_type->kind = HONEY_TYPE_VOID;
  }

  // parameter types
  sym->type.func.param_count = node->data.func_decl.param_count;
  if (sym->type.func.param_count > 0) {
    sym->type.func.param_types =
      malloc(sizeof(struct honey_type*) * sym->type.func.param_count);

    for (int i = 0; i < sym->type.func.param_count; i += 1) {
      struct honey_ast_node* param = node->data.func_decl.params[i];

      if (!param->data.name.type) {
        honey_error("parameter \"%s\" missing type",
                    param->data.name.identifier);
        return false;
      }

      enum honey_type_kind param_kind =
        honey_resolve_type_name(param->data.name.type);
      if (param_kind == HONEY_TYPE_UNKNOWN) {
        honey_error("unknown parameter type \"%s\"", param->data.name.type);
        return false;
      }

      sym->type.func.param_types[i] = malloc(sizeof(struct honey_type));
      sym->type.func.param_types[i]->kind = param_kind;
    }
  } else {
    sym->type.func.param_types = NULL;
  }

  return true;
}

bool
analyze_test_decl(struct honey_ast_node* node,
                  struct honey_symbol_table* symtab)
{
  if (symtab->count >= HONEY_MAX_SYMBOLS) {
    honey_error("too many symbols");
    return false;
  }

  struct honey_symbol* sym = &symtab->symbols[symtab->count];
  symtab->count += 1;
  sym->name = strdup(node->data.test_decl.name);
  sym->kind = HONEY_SYMBOL_TEST;
  sym->test_node = node;

  // tests are effectively void -> void functions
  sym->type.kind = HONEY_TYPE_FUNCTION;
  sym->type.func.return_type = malloc(sizeof(struct honey_type));
  sym->type.func.return_type->kind = HONEY_TYPE_VOID;
  sym->type.func.param_count = 0;
  sym->type.func.param_types = NULL;

  return true;
}

bool
honey_analyze(struct honey_ast_node** declarations,
              int count,
              struct honey_symbol_table* symtab)
{
  symtab->count = 0;

  if (!declarations) {
    honey_error("null AST");
    return false;
  }

  // process all declarations
  for (int i = 0; i < count; i += 1) {
    struct honey_ast_node* ast = declarations[i];
    bool success = false;

    switch (ast->kind) {
      case HONEY_AST_COMPTIME_DECL:
        success = analyze_comptime_decl(ast, symtab);
        break;

      case HONEY_AST_FUNC_DECL:
        success = analyze_func_decl(ast, symtab);
        break;

      case HONEY_AST_TEST_DECL:
        success = analyze_test_decl(ast, symtab);
        break;

      default:
        honey_error("unexpected AST node kind");
        return false;
    }

    // only return on error
    if (!success) {
      return false;
    }
  }

  return true;
}

void
honey_symbol_table_print(struct honey_symbol_table* symtab)
{
  printf("Symbol Table (%d symbols):\n", symtab->count);

  for (int i = 0; i < symtab->count; i += 1) {
    struct honey_symbol* sym = &symtab->symbols[i];

    printf("  [%d] %s: ", i, sym->name);

    if (sym->kind == HONEY_SYMBOL_COMPTIME) {
      printf("const %s = ", honey_type_kind_to_text(sym->type.kind));

      if (sym->type.kind >= HONEY_TYPE_I8 && sym->type.kind <= HONEY_TYPE_U64) {
        printf("%lld", sym->comptime_value.int_value);
      } else if (sym->type.kind == HONEY_TYPE_F32 ||
                 sym->type.kind == HONEY_TYPE_F64) {
        printf("%f", sym->comptime_value.float_value);
      }
    } else if (sym->kind == HONEY_SYMBOL_FUNCTION) {
      printf("func(");
      for (int j = 0; j < sym->type.func.param_count; j += 1) {
        if (j > 0)
          printf(", ");
        printf("%s",
               honey_type_kind_to_text(sym->type.func.param_types[j]->kind));
      }
      printf(") : %s",
             honey_type_kind_to_text(sym->type.func.return_type->kind));
    } else if (sym->kind == HONEY_SYMBOL_TEST) {
      printf("test");
    }

    printf("\n");
  }
}

// get the size in bytes for a type
int
honey_type_size(enum honey_type_kind type)
{
  switch (type) {
    case HONEY_TYPE_VOID:
      return 0;
    case HONEY_TYPE_U8:
    case HONEY_TYPE_I8:
      return 1;
    case HONEY_TYPE_U16:
    case HONEY_TYPE_I16:
      return 2;
    case HONEY_TYPE_U32:
    case HONEY_TYPE_I32:
    case HONEY_TYPE_F32:
      return 4;
    case HONEY_TYPE_U64:
    case HONEY_TYPE_I64:
    case HONEY_TYPE_F64:
      return 8;

    // default to pointer size
    default:
      return 8;
  }
}
