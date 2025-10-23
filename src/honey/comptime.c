#include "honey/comptime.h"
#include "honey/ast.h"
#include "honey/log.h"
#include "honey/semantic.h"
#include <string.h>

static struct honey_comptime_value
evaluate_comptime_function_call(struct honey_symbol* func_sym,
                                struct honey_comptime_value* arg_values,
                                int arg_count,
                                struct honey_symbol_table* symtab,
                                int recursion_depth);

static struct honey_comptime_value
make_invalid(void)
{
  return (struct honey_comptime_value){ .kind = HONEY_COMPTIME_INVALID };
}

static struct honey_comptime_value
make_int(int value)
{
  return (struct honey_comptime_value){
    .kind = HONEY_COMPTIME_INT,
    .int_value = value,
  };
}

static struct honey_comptime_value
make_float(double value)
{
  return (struct honey_comptime_value){
    .kind = HONEY_COMPTIME_FLOAT,
    .float_value = value,
  };
}

struct honey_comptime_value
honey_comptime_eval(struct honey_ast_node* expr,
                    struct honey_symbol_table* symtab)
{
  if (!expr) {
    return make_invalid();
  }

  switch (expr->kind) {
    case HONEY_AST_LITERAL_INT:
      return make_int(expr->data.int_literal);
    case HONEY_AST_LITERAL_FLOAT:
      return make_float(expr->data.float_literal);

    case HONEY_AST_UNARY_OP: {
      struct honey_comptime_value operand =
        honey_comptime_eval(expr->data.unary_op.operand, symtab);

      if (operand.kind == HONEY_COMPTIME_INVALID) {
        return make_invalid();
      }

      switch (expr->data.unary_op.op) {
        case HONEY_UNARY_OP_NEG:
          if (operand.kind == HONEY_COMPTIME_INT) {
            return make_int(-operand.int_value);
          } else if (operand.kind == HONEY_COMPTIME_FLOAT) {
            return make_float(-operand.float_value);
          }
          break;

        default:
          honey_error("unsupported comptime operation: %s%s%s",
                      ANSI_COLOR_RED,
                      honey_unary_op_kind_to_text(expr->data.unary_op.op),
                      ANSI_COLOR_RESET);
          break;
      }

      return make_invalid();
    }

    case HONEY_AST_BINARY_OP: {
      struct honey_comptime_value left =
        honey_comptime_eval(expr->data.binary_op.left, symtab);
      struct honey_comptime_value right =
        honey_comptime_eval(expr->data.binary_op.right, symtab);

      if (left.kind == HONEY_COMPTIME_INVALID ||
          right.kind == HONEY_COMPTIME_INVALID) {
        return make_invalid();
      }

      // for now, handle only int + int and float + float
      // later, type coercion rules here (but casting must be explicit! honey
      // doesn't and shouldn't allow implicit casting of data types!)
      if (left.kind == HONEY_COMPTIME_INT && right.kind == HONEY_COMPTIME_INT) {
        int result;
        switch (expr->data.binary_op.op) {
          case HONEY_BINARY_OP_ADD:
            result = left.int_value + right.int_value;
            break;
          case HONEY_BINARY_OP_SUB:
            result = left.int_value - right.int_value;
            break;
          case HONEY_BINARY_OP_MUL:
            result = left.int_value * right.int_value;
            break;
          case HONEY_BINARY_OP_DIV:
            if (right.int_value == 0) {
              return make_invalid(); // division by zero (maybe report an error?
                                     // though this is comptime...)
            }
            result = left.int_value / right.int_value;
            break;
        }
        return make_int(result);
      }

      if (left.kind == HONEY_COMPTIME_FLOAT &&
          right.kind == HONEY_COMPTIME_FLOAT) {
        double result;
        switch (expr->data.binary_op.op) {
          case HONEY_BINARY_OP_ADD:
            result = left.float_value + right.float_value;
            break;
          case HONEY_BINARY_OP_SUB:
            result = left.float_value - right.float_value;
            break;
          case HONEY_BINARY_OP_MUL:
            result = left.float_value * right.float_value;
            break;
          case HONEY_BINARY_OP_DIV:
            if (right.float_value == 0) {
              return make_invalid(); // division by zero (maybe report an error?
                                     // though this is comptime...)
            }
            result = left.float_value / right.float_value;
            break;
        }
        return make_float(result);
      }

      return make_invalid();
    }

    case HONEY_AST_NAME: {
      // look up name in symbol table
      if (!symtab) {
        return make_invalid();
      }

      for (int i = 0; i < symtab->count; i += 1) {
        if (strcmp(symtab->symbols[i].name, expr->data.name.identifier) == 0) {
          // only comptime constants can be used in comptime expressions (for
          // now)
          if (symtab->symbols[i].kind == HONEY_SYMBOL_COMPTIME) {
            if (symtab->symbols[i].type.kind >= HONEY_TYPE_I8 &&
                symtab->symbols[i].type.kind <= HONEY_TYPE_U64) {
              return make_int(symtab->symbols[i].comptime_value.int_value);
            } else if (symtab->symbols[i].type.kind >= HONEY_TYPE_F32 &&
                       symtab->symbols[i].type.kind <= HONEY_TYPE_F64) {
              return make_float(symtab->symbols[i].comptime_value.float_value);
            }
          }
          // runtime dependent functions and variables can't be used in comptime
          return make_invalid();
        }
      }
      // unknown symbol
      return make_invalid();
    }

    case HONEY_AST_CALL_EXPR: {
      // look up the function
      struct honey_symbol* func_sym = NULL;
      for (int i = 0; i < symtab->count; i++) {
        if (strcmp(symtab->symbols[i].name,
                   expr->data.call_expr.function_name) == 0) {
          func_sym = &symtab->symbols[i];
          break;
        }
      }

      if (!func_sym || func_sym->kind != HONEY_SYMBOL_FUNCTION) {
        honey_error("undefined function \"%s\"",
                    expr->data.call_expr.function_name);
        return make_invalid();
      }

      // check if function is comptime
      if (!func_sym->func.is_comptime) {
        honey_error("cannot call non-comptime function \"%s\" at compile time",
                    func_sym->name);
        return make_invalid();
      }

      // evaluate all arguments
      int arg_count = expr->data.call_expr.argument_count;
      struct honey_comptime_value* arg_values =
        malloc(sizeof(struct honey_comptime_value) * arg_count);

      for (int i = 0; i < arg_count; i++) {
        arg_values[i] =
          honey_comptime_eval(expr->data.call_expr.arguments[i], symtab);

        if (arg_values[i].kind == HONEY_COMPTIME_INVALID) {
          free(arg_values);
          return make_invalid();
        }
      }

      // call the function at comptime
      struct honey_comptime_value result = evaluate_comptime_function_call(
        func_sym, arg_values, arg_count, symtab, 0);

      free(arg_values);
      return result;
    }

      // variables are not comptime-evaluable
    case HONEY_AST_VAR_DECL:
    case HONEY_AST_ASSIGNMENT:
    default:
      return make_invalid();
  }
}

bool
honey_is_comptime_evaluable(struct honey_ast_node* expr,
                            struct honey_symbol_table* symtab)
{
  struct honey_comptime_value result = honey_comptime_eval(expr, symtab);
  return result.kind != HONEY_COMPTIME_INVALID;
}

static struct honey_comptime_execution_context*
create_comptime_context(struct honey_symbol_table* symtab)
{
  struct honey_comptime_execution_context* ctx =
    malloc(sizeof(struct honey_comptime_execution_context));

  ctx->global_symtab = symtab;
  ctx->locals =
    malloc(sizeof(struct honey_comptime_local_var) * MAX_COMPTIME_LOCALS);
  ctx->locals_count = 0;
  ctx->locals_capacity = MAX_COMPTIME_LOCALS;
  ctx->params = NULL;
  ctx->params_count = 0;
  ctx->return_value = make_invalid();
  ctx->has_returned = false;
  ctx->recursion_depth = 0;

  return ctx;
}

static void
destroy_comptime_context(struct honey_comptime_execution_context* ctx)
{
  if (!ctx)
    return;

  // free local variable names
  for (int i = 0; i < ctx->locals_count; i++) {
    free(ctx->locals[i].name);
  }
  free(ctx->locals);

  // free parameter names
  for (int i = 0; i < ctx->params_count; i++) {
    free(ctx->params[i].name);
  }
  free(ctx->params);

  free(ctx);
}

static struct honey_comptime_value*
lookup_local(struct honey_comptime_execution_context* ctx, const char* name)
{
  // check parameters first
  for (int i = 0; i < ctx->params_count; i++) {
    if (strcmp(ctx->params[i].name, name) == 0) {
      return &ctx->params[i].value;
    }
  }

  // then check locals
  for (int i = 0; i < ctx->locals_count; i++) {
    if (strcmp(ctx->locals[i].name, name) == 0) {
      return &ctx->locals[i].value;
    }
  }

  return NULL;
}

static bool
add_local(struct honey_comptime_execution_context* ctx,
          const char* name,
          struct honey_comptime_value value)
{
  if (ctx->locals_count >= ctx->locals_capacity) {
    honey_error("too many local variables in comptime function");
    return false;
  }

  ctx->locals[ctx->locals_count].name = strdup(name);
  ctx->locals[ctx->locals_count].value = value;
  ctx->locals_count++;

  return true;
}

static bool
evaluate_statement_at_comptime(struct honey_ast_node* stmt,
                               struct honey_comptime_execution_context* ctx);

static bool
evaluate_block_at_comptime(struct honey_ast_node* block,
                           struct honey_comptime_execution_context* ctx)
{
  if (!block || block->kind != HONEY_AST_BLOCK) {
    return false;
  }

  for (int i = 0; i < block->data.block.statement_count; i++) {
    if (!evaluate_statement_at_comptime(block->data.block.statements[i], ctx)) {
      return false;
    }

    // if we hit a return, stop executing
    if (ctx->has_returned) {
      return true;
    }
  }

  return true;
}

static struct honey_comptime_value
evaluate_expr_in_context(struct honey_ast_node* expr,
                         struct honey_comptime_execution_context* ctx)
{
  if (!expr) {
    return make_invalid();
  }

  switch (expr->kind) {
    case HONEY_AST_LITERAL_INT:
      return make_int(expr->data.int_literal);

    case HONEY_AST_LITERAL_FLOAT:
      return make_float(expr->data.float_literal);

    case HONEY_AST_NAME: {
      // first check if it's a local/parameter
      struct honey_comptime_value* local =
        lookup_local(ctx, expr->data.name.identifier);

      if (local) {
        return *local;
      }

      // fall back to checking global symbol table
      for (int i = 0; i < ctx->global_symtab->count; i++) {
        if (strcmp(ctx->global_symtab->symbols[i].name,
                   expr->data.name.identifier) == 0) {
          if (ctx->global_symtab->symbols[i].kind == HONEY_SYMBOL_COMPTIME) {
            if (ctx->global_symtab->symbols[i].type.kind >= HONEY_TYPE_I8 &&
                ctx->global_symtab->symbols[i].type.kind <= HONEY_TYPE_U64) {
              return make_int(
                ctx->global_symtab->symbols[i].comptime_value.int_value);
            } else if (ctx->global_symtab->symbols[i].type.kind >=
                         HONEY_TYPE_F32 &&
                       ctx->global_symtab->symbols[i].type.kind <=
                         HONEY_TYPE_F64) {
              return make_float(
                ctx->global_symtab->symbols[i].comptime_value.float_value);
            }
          }
          return make_invalid();
        }
      }
      return make_invalid();
    }

    case HONEY_AST_BINARY_OP: {
      // recursively evaluate both operands in context
      struct honey_comptime_value left =
        evaluate_expr_in_context(expr->data.binary_op.left, ctx);
      struct honey_comptime_value right =
        evaluate_expr_in_context(expr->data.binary_op.right, ctx);

      if (left.kind == HONEY_COMPTIME_INVALID ||
          right.kind == HONEY_COMPTIME_INVALID) {
        return make_invalid();
      }

      // handle integer operations
      if (left.kind == HONEY_COMPTIME_INT && right.kind == HONEY_COMPTIME_INT) {
        int64_t result;
        switch (expr->data.binary_op.op) {
          case HONEY_BINARY_OP_ADD:
            result = left.int_value + right.int_value;
            break;
          case HONEY_BINARY_OP_SUB:
            result = left.int_value - right.int_value;
            break;
          case HONEY_BINARY_OP_MUL:
            result = left.int_value * right.int_value;
            break;
          case HONEY_BINARY_OP_DIV:
            if (right.int_value == 0) {
              honey_error("division by zero in comptime expression");
              return make_invalid();
            }
            result = left.int_value / right.int_value;
            break;
          default:
            return make_invalid();
        }
        return make_int(result);
      }

      // handle float operations
      if (left.kind == HONEY_COMPTIME_FLOAT &&
          right.kind == HONEY_COMPTIME_FLOAT) {
        double result;
        switch (expr->data.binary_op.op) {
          case HONEY_BINARY_OP_ADD:
            result = left.float_value + right.float_value;
            break;
          case HONEY_BINARY_OP_SUB:
            result = left.float_value - right.float_value;
            break;
          case HONEY_BINARY_OP_MUL:
            result = left.float_value * right.float_value;
            break;
          case HONEY_BINARY_OP_DIV:
            if (right.float_value == 0.0) {
              honey_error("division by zero in comptime expression");
              return make_invalid();
            }
            result = left.float_value / right.float_value;
            break;
          default:
            return make_invalid();
        }
        return make_float(result);
      }

      return make_invalid();
    }

    case HONEY_AST_CALL_EXPR: {
      // look up the function in global symbol table
      struct honey_symbol* func_sym = NULL;
      for (int i = 0; i < ctx->global_symtab->count; i++) {
        if (strcmp(ctx->global_symtab->symbols[i].name,
                   expr->data.call_expr.function_name) == 0) {
          func_sym = &ctx->global_symtab->symbols[i];
          break;
        }
      }

      if (!func_sym || func_sym->kind != HONEY_SYMBOL_FUNCTION) {
        honey_error("undefined function '%s'",
                    expr->data.call_expr.function_name);
        return make_invalid();
      }

      if (!func_sym->func.is_comptime) {
        honey_error("cannot call non-comptime function '%s' at compile time",
                    func_sym->name);
        return make_invalid();
      }

      // evaluate all arguments in context
      int arg_count = expr->data.call_expr.argument_count;
      struct honey_comptime_value* arg_values =
        malloc(sizeof(struct honey_comptime_value) * arg_count);

      for (int i = 0; i < arg_count; i++) {
        arg_values[i] =
          evaluate_expr_in_context(expr->data.call_expr.arguments[i], ctx);

        if (arg_values[i].kind == HONEY_COMPTIME_INVALID) {
          free(arg_values);
          return make_invalid();
        }
      }

      // call the function at comptime
      struct honey_comptime_value result =
        evaluate_comptime_function_call(func_sym,
                                        arg_values,
                                        arg_count,
                                        ctx->global_symtab,
                                        ctx->recursion_depth);

      free(arg_values);
      return result;
    }

    default:
      return make_invalid();
  }
}

static bool
evaluate_statement_at_comptime(struct honey_ast_node* stmt,
                               struct honey_comptime_execution_context* ctx)
{
  switch (stmt->kind) {
    case HONEY_AST_RETURN_STMT: {
      if (stmt->data.return_stmt.value) {
        ctx->return_value =
          evaluate_expr_in_context(stmt->data.return_stmt.value, ctx);

        if (ctx->return_value.kind == HONEY_COMPTIME_INVALID) {
          honey_error("non-comptime expression in return statement");
          return false;
        }
      } else {
        // void return
        ctx->return_value = make_invalid();
      }

      ctx->has_returned = true;
      return true;
    }

    case HONEY_AST_VAR_DECL: {
      // evaluate initializer
      struct honey_comptime_value init_val = make_invalid();

      if (stmt->data.var_decl.value) {
        init_val = evaluate_expr_in_context(stmt->data.var_decl.value, ctx);

        if (init_val.kind == HONEY_COMPTIME_INVALID) {
          honey_error("non-comptime expression in variable initializer");
          return false;
        }
      }

      // add to locals
      return add_local(ctx, stmt->data.var_decl.name, init_val);
    }

    case HONEY_AST_ASSIGNMENT: {
      // evaluate new value
      struct honey_comptime_value new_val =
        evaluate_expr_in_context(stmt->data.assignment.value, ctx);

      if (new_val.kind == HONEY_COMPTIME_INVALID) {
        honey_error("non-comptime expression in assignment");
        return false;
      }

      // find and update the variable
      struct honey_comptime_value* var =
        lookup_local(ctx, stmt->data.assignment.name);

      if (!var) {
        honey_error("undefined variable '%s' in comptime function",
                    stmt->data.assignment.name);
        return false;
      }

      *var = new_val;
      return true;
    }

    default:
      honey_error("unsupported statement type in comptime function");
      return false;
  }
}

static struct honey_comptime_value
evaluate_comptime_function_call(struct honey_symbol* func_sym,
                                struct honey_comptime_value* arg_values,
                                int arg_count,
                                struct honey_symbol_table* symtab,
                                int recursion_depth)
{
  // check recursion depth
  if (recursion_depth > MAX_COMPTIME_RECURSION) {
    honey_error("comptime recursion depth exceeded (max: %d)",
                MAX_COMPTIME_RECURSION);
    return make_invalid();
  }

  struct honey_ast_node* func_node = func_sym->func.func_node;

  // verify argument count
  if (arg_count != func_node->data.func_decl.param_count) {
    honey_error("wrong number of arguments to comptime function '%s'",
                func_sym->name);
    return make_invalid();
  }

  // create execution context
  struct honey_comptime_execution_context* ctx =
    create_comptime_context(symtab);
  ctx->recursion_depth = recursion_depth + 1;

  // bind parameters
  ctx->params_count = arg_count;
  if (arg_count > 0) {
    ctx->params = malloc(sizeof(struct honey_comptime_local_var) * arg_count);

    for (int i = 0; i < arg_count; i++) {
      ctx->params[i].name =
        strdup(func_node->data.func_decl.params[i]->data.name.identifier);
      ctx->params[i].value = arg_values[i];
    }
  }

  // execute function body
  bool success =
    evaluate_block_at_comptime(func_node->data.func_decl.body, ctx);

  struct honey_comptime_value result;
  if (success && ctx->has_returned) {
    result = ctx->return_value;
  } else {
    result = make_invalid();
  }

  destroy_comptime_context(ctx);
  return result;
}
