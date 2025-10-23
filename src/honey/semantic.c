#include "honey/semantic.h"
#include "honey/ast.h"
#include "honey/comptime.h"
#include "honey/log.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct dependency_info
{
  char* name;        // declaration name
  char** depends_on; // array of symbol names this depends on
  int depends_on_count;
  bool evaluated; // has this been evaluated yet?
  int index;      // original index in declarations array
};

/**
 * Recursively finds all symbol names that an expression depends on
 * @param expr The expression to analyze
 * @param deps Pointer to array of dependency names (will be
 * allocated/reallocated)
 * @param dep_count Pointer to count of dependencies found
 */
static void
find_dependencies_in_expr(struct honey_ast_node* expr,
                          char*** deps,
                          int* dep_count);

/**
 * Adds a dependency name to the deps array if not already present
 * @param deps Pointer to array of dependency names
 * @param dep_count Pointer to count of dependencies
 * @param name The symbol name to add
 */
static void
add_dependency(char*** deps, int* dep_count, const char* name);

/**
 * Checks if a name is already in the dependencies array
 */
static bool
has_dependency(char** deps, int dep_count, const char* name);

/**
 * Builds dependency info for all comptime declarations
 * @param declarations All AST declarations
 * @param count Total number of declarations
 * @param symtab Symbol table (for checking if symbols exist)
 * @param out_comptime_count Output parameter for number of comptime decls found
 * @return Array of dependency_info structs (only for comptime decls)
 */
static struct dependency_info*
build_dependency_graph(struct honey_ast_node** declarations,
                       int count,
                       struct honey_symbol_table* symtab,
                       int* out_comptime_count);

/**
 * Performs topological sort using Kahn's algorithm
 * @param deps Array of dependency_info structs
 * @param count Number of items in deps array
 * @param has_cycle Output parameter - set to true if cycle detected
 * @return Array of indices in evaluation order, or NULL if cycle detected
 */
static int*
topological_sort(struct dependency_info* deps, int count, bool* has_cycle);

/**
 * Helper: finds the index of a declaration by name in the deps array
 * @return Index in deps array, or -1 if not found
 */
static int
find_dep_index(struct dependency_info* deps, int count, const char* name);

/**
 * Reports which declarations form a dependency cycle
 * @param deps Array of dependency_info structs
 * @param count Number of items
 */
static void
report_dependency_cycle(struct dependency_info* deps, int count);

/**
 * Frees all memory associated with dependency_info array
 */
static void
cleanup_dependency_info(struct dependency_info* deps, int count);

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
comptime_value_fits_type(struct honey_comptime_value value,
                         enum honey_type_kind type)
{
  switch (type) {
    case HONEY_TYPE_I8:
    case HONEY_TYPE_I16:
    case HONEY_TYPE_I32:
    case HONEY_TYPE_I64:
    case HONEY_TYPE_U8:
    case HONEY_TYPE_U16:
    case HONEY_TYPE_U32:
    case HONEY_TYPE_U64:
      // integer types accept only integers, no implicit conversion!
      // TODO: add range checking (e.g., i8 must be -128..127)
      return value.kind == HONEY_COMPTIME_INT;

    case HONEY_TYPE_F32:
    case HONEY_TYPE_F64:
      // float types accept only floats, no implicit conversion!
      return value.kind == HONEY_COMPTIME_FLOAT;

    default:
      return false;
  }
}

static bool
register_comptime_decl(struct honey_ast_node* node,
                       struct honey_symbol_table* symtab)
{
  if (symtab->count >= HONEY_MAX_SYMBOLS) {
    honey_error("too many symbols");
    return false;
  }

  // check for duplicate definitions
  for (int i = 0; i < symtab->count; i += 1) {
    if (strcmp(symtab->symbols[i].name, node->data.comptime_decl.name) == 0) {
      honey_error("comptime constant \"%s%s%s\" is already defined",
                  ANSI_COLOR_RED,
                  symtab->symbols[i].name,
                  ANSI_COLOR_RESET);
      return false;
    }
  }

  struct honey_symbol* sym = &symtab->symbols[symtab->count];
  symtab->count += 1;
  sym->name = strdup(node->data.comptime_decl.name);
  sym->kind = HONEY_SYMBOL_COMPTIME;

  // validate explicit type if provided
  if (node->data.comptime_decl.explicit_type) {
    enum honey_type_kind resolved_type =
      honey_resolve_type_name(node->data.comptime_decl.explicit_type);

    if (resolved_type == HONEY_TYPE_UNKNOWN) {
      honey_error("unknown type \"%s%s%s\" in comptime expression",
                  ANSI_COLOR_RED,
                  node->data.comptime_decl.explicit_type,
                  ANSI_COLOR_RESET);
      symtab->count -= 1;
      free(sym->name);
      return false;
    }

    sym->type.kind = resolved_type;
  } else {
    // type will be inferred during evaluation pass
    sym->type.kind = HONEY_TYPE_UNKNOWN;
  }

  return true;
}

static bool
evaluate_comptime_decl(struct honey_ast_node* node,
                       struct honey_symbol_table* symtab)
{
  // find the symbol
  struct honey_symbol* sym = NULL;
  for (int i = 0; i < symtab->count; i += 1) {
    if (strcmp(symtab->symbols[i].name, node->data.comptime_decl.name) == 0) {
      sym = &symtab->symbols[i];
      break;
    }
  }

  if (!sym) {
    honey_error("internal error: symbol \"%s\" not found",
                ANSI_COLOR_RED,
                node->data.comptime_decl.name,
                ANSI_COLOR_RESET);
    return false;
  }

  struct honey_comptime_value result =
    honey_comptime_eval(node->data.comptime_decl.value, symtab);

  if (result.kind == HONEY_COMPTIME_INVALID) {
    honey_error(
      "comptime declaration \"%s%s%s\" contains non-comptime expression",
      ANSI_COLOR_RED,
      node->data.comptime_decl.name,
      ANSI_COLOR_RESET);
    return false;
  }

  // determine the type
  enum honey_type_kind resolved_type;

  if (sym->type.kind != HONEY_TYPE_UNKNOWN) {
    // explicit type was provided
    resolved_type = sym->type.kind;

    // type validation check
    if (!comptime_value_fits_type(result, resolved_type)) {
      if (result.kind == HONEY_COMPTIME_INT) {
        honey_error("cannot assign integer value to type \"%s\"",
                    honey_type_kind_to_text(resolved_type));
      } else if (result.kind == HONEY_COMPTIME_FLOAT) {
        honey_error("cannot assign floating point value to type \"%s\"",
                    honey_type_kind_to_text(resolved_type));
      }
      return false;
    }
  } else {
    // infer type from computed value
    if (result.kind == HONEY_COMPTIME_INT) {
      resolved_type = HONEY_TYPE_I64; // default to i64 integers for now
    } else if (result.kind == HONEY_COMPTIME_FLOAT) {
      resolved_type = HONEY_TYPE_F64; // default to f64 floats for now
    } else {
      // this shouldn't happen if honey_eval_comptime works correctly
      honey_error("internal error: couldn't infer type for comptime constant");
      return false;
    }
  }

  // store type and value based on computed result
  sym->type.kind = resolved_type;
  if (result.kind == HONEY_COMPTIME_INT) {
    sym->comptime_value.int_value = result.int_value;
  } else if (result.kind == HONEY_COMPTIME_FLOAT) {
    sym->comptime_value.float_value = result.float_value;
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
  sym->func.func_node = node;
  sym->func.is_comptime = node->data.func_decl.is_comptime;

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

  // first pass: register all symbols (names and types only, no evaluation)
  for (int i = 0; i < count; i += 1) {
    struct honey_ast_node* ast = declarations[i];
    bool success = false;

    switch (ast->kind) {
      case HONEY_AST_COMPTIME_DECL:
        success = register_comptime_decl(ast, symtab);
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

    if (!success) {
      return false;
    }
  }

  // second pass: evaluate comptime expressions in dependency order
  int comptime_count = 0;
  struct dependency_info* dep_info =
    build_dependency_graph(declarations, count, symtab, &comptime_count);

  if (comptime_count == 0) {
    // no comptime declarations, we're done
    return true;
  }

  bool has_cycle = false;
  int* eval_order = topological_sort(dep_info, comptime_count, &has_cycle);

  if (has_cycle) {
    report_dependency_cycle(dep_info, comptime_count);
    cleanup_dependency_info(dep_info, comptime_count);
    return false;
  }

  // evaluate in dependency order
  for (int i = 0; i < comptime_count; i++) {
    int dep_idx = eval_order[i];
    int decl_idx = dep_info[dep_idx].index;

    if (!evaluate_comptime_decl(declarations[decl_idx], symtab)) {
      cleanup_dependency_info(dep_info, comptime_count);
      free(eval_order);
      return false;
    }
  }

  cleanup_dependency_info(dep_info, comptime_count);
  free(eval_order);

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

static bool
has_dependency(char** deps, int dep_count, const char* name)
{
  for (int i = 0; i < dep_count; i++) {
    if (strcmp(deps[i], name) == 0) {
      return true;
    }
  }
  return false;
}

static void
add_dependency(char*** deps, int* dep_count, const char* name)
{
  // don't add duplicates
  if (has_dependency(*deps, *dep_count, name)) {
    return;
  }

  // reallocate array
  *deps = realloc(*deps, sizeof(char*) * (*dep_count + 1));
  (*deps)[*dep_count] = strdup(name);
  (*dep_count)++;
}

static void
find_dependencies_in_expr(struct honey_ast_node* expr,
                          char*** deps,
                          int* dep_count)
{
  if (!expr)
    return;

  switch (expr->kind) {
    case HONEY_AST_NAME:
      // this is a reference to another symbol
      add_dependency(deps, dep_count, expr->data.name.identifier);
      break;

    case HONEY_AST_BINARY_OP:
      find_dependencies_in_expr(expr->data.binary_op.left, deps, dep_count);
      find_dependencies_in_expr(expr->data.binary_op.right, deps, dep_count);
      break;

    case HONEY_AST_UNARY_OP:
      find_dependencies_in_expr(expr->data.unary_op.operand, deps, dep_count);
      break;

    case HONEY_AST_LITERAL_INT:
    case HONEY_AST_LITERAL_FLOAT:
      // literals have no dependencies
      break;

    case HONEY_AST_CALL_EXPR:
      // add function name as dependency
      add_dependency(deps, dep_count, expr->data.call_expr.function_name);
      // also check arguments
      for (int i = 0; i < expr->data.call_expr.argument_count; i++) {
        find_dependencies_in_expr(
          expr->data.call_expr.arguments[i], deps, dep_count);
      }
      break;

    default:
      // for other node types, no dependencies (or not yet supported)
      break;
  }
}

static struct dependency_info*
build_dependency_graph(struct honey_ast_node** declarations,
                       int count,
                       struct honey_symbol_table* symtab,
                       int* out_comptime_count)
{
  // first pass: count comptime declarations
  int comptime_count = 0;
  for (int i = 0; i < count; i++) {
    if (declarations[i]->kind == HONEY_AST_COMPTIME_DECL) {
      comptime_count++;
    }
  }

  *out_comptime_count = comptime_count;

  if (comptime_count == 0) {
    return NULL;
  }

  // allocate dependency info array
  struct dependency_info* dep_info =
    malloc(sizeof(struct dependency_info) * comptime_count);

  // second pass: build dependency info for each comptime declaration
  int dep_idx = 0;
  for (int i = 0; i < count; i++) {
    if (declarations[i]->kind != HONEY_AST_COMPTIME_DECL) {
      continue;
    }

    struct dependency_info* info = &dep_info[dep_idx];
    info->name = strdup(declarations[i]->data.comptime_decl.name);
    info->depends_on = NULL;
    info->depends_on_count = 0;
    info->evaluated = false;
    info->index = i; // original index in declarations array

    // find dependencies in the value expression
    find_dependencies_in_expr(declarations[i]->data.comptime_decl.value,
                              &info->depends_on,
                              &info->depends_on_count);

    // filter out dependencies that aren't comptime constants
    // (keep only dependencies that refer to other comptime declarations)
    int filtered_count = 0;
    for (int j = 0; j < info->depends_on_count; j++) {
      bool is_comptime = false;

      // check if this dependency is a comptime constant
      for (int k = 0; k < symtab->count; k++) {
        if (strcmp(symtab->symbols[k].name, info->depends_on[j]) == 0) {
          if (symtab->symbols[k].kind == HONEY_SYMBOL_COMPTIME) {
            is_comptime = true;
          }
          break;
        }
      }

      // keep this dependency
      if (is_comptime) {
        info->depends_on[filtered_count] = info->depends_on[j];
        filtered_count++;
      } else {
        // free the string we're not keeping
        free(info->depends_on[j]);
      }
    }
    info->depends_on_count = filtered_count;

    dep_idx++;
  }

  return dep_info;
}

static int
find_dep_index(struct dependency_info* deps, int count, const char* name)
{
  for (int i = 0; i < count; i++) {
    if (strcmp(deps[i].name, name) == 0) {
      return i;
    }
  }
  return -1;
}

static int*
topological_sort(struct dependency_info* deps, int count, bool* has_cycle)
{
  if (count == 0) {
    *has_cycle = false;
    return NULL;
  }

  int* result = malloc(sizeof(int) * count);
  int result_count = 0;

  // calculate in-degrees (number of dependencies for each node)
  int* in_degree = calloc(count, sizeof(int));

  for (int i = 0; i < count; i++) {
    for (int j = 0; j < deps[i].depends_on_count; j++) {
      // find which declaration this dependency refers to
      int dep_idx = find_dep_index(deps, count, deps[i].depends_on[j]);

      if (dep_idx != -1) {
        // this is a dependency on another comptime constant
        in_degree[i]++;
      }
      // if dep_idx == -1, it's a dependency on something else (like a function)
      // which we can ignore for ordering purposes
    }
  }

  // queue all nodes with in-degree 0 (no dependencies)
  int* queue = malloc(sizeof(int) * count);
  int queue_head = 0;
  int queue_tail = 0;

  for (int i = 0; i < count; i++) {
    if (in_degree[i] == 0) {
      queue[queue_tail++] = i;
    }
  }

  // process queue
  while (queue_head < queue_tail) {
    int current = queue[queue_head++];
    result[result_count++] = current;

    // for each declaration that depends on 'current', reduce its in-degree
    for (int i = 0; i < count; i++) {
      if (i == current)
        continue;

      // check if declaration i depends on current
      for (int j = 0; j < deps[i].depends_on_count; j++) {
        if (strcmp(deps[i].depends_on[j], deps[current].name) == 0) {
          in_degree[i]--;
          if (in_degree[i] == 0) {
            queue[queue_tail++] = i;
          }
          break;
        }
      }
    }
  }

  free(in_degree);
  free(queue);

  // check if we processed all nodes
  if (result_count != count) {
    // cycle detected!
    *has_cycle = true;
    free(result);
    return NULL;
  }

  *has_cycle = false;
  return result;
}

static void
report_dependency_cycle(struct dependency_info* deps, int count)
{
  honey_error("circular dependency detected in comptime declarations:");

  // try to find and report at least one cycle
  // simple approach: find a node that's part of a cycle and trace it
  bool* visited = calloc(count, sizeof(bool));
  bool* rec_stack = calloc(count, sizeof(bool));

  // this is a simplified cycle finder - just reports that a cycle exists
  // a more sophisticated version would trace the actual cycle path
  for (int i = 0; i < count; i++) {
    if (deps[i].depends_on_count > 0) {
      honey_error("  '%s' depends on:", deps[i].name);
      for (int j = 0; j < deps[i].depends_on_count; j++) {
        honey_error("    - %s", deps[i].depends_on[j]);
      }
    }
  }

  free(visited);
  free(rec_stack);
}

static void
cleanup_dependency_info(struct dependency_info* deps, int count)
{
  if (!deps)
    return;

  for (int i = 0; i < count; i++) {
    free(deps[i].name);

    for (int j = 0; j < deps[i].depends_on_count; j++) {
      free(deps[i].depends_on[j]);
    }

    if (deps[i].depends_on) {
      free(deps[i].depends_on);
    }
  }

  free(deps);
}
