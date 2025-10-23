#include "honey/comptime.h"
#include "honey/ast.h"
#include "honey/log.h"
#include "honey/semantic.h"
#include <string.h>

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
        make_invalid();
      }

      for (int i = 0; i < symtab->count; i += 1) {
        if (strcmp(symtab->symbols[i].name, expr->data.name.identifier) == 0) {
          // only comptime constants can be used in comptime expressions (for
          // now)
          if (symtab->symbols[i].kind == HONEY_SYMBOL_COMPTIME) {
            if (symtab->symbols[i].type.kind >= HONEY_TYPE_I8 &&
                symtab->symbols[i].type.kind <= HONEY_TYPE_U64) {
              make_int(symtab->symbols[i].comptime_value.int_value);
            } else if (symtab->symbols[i].type.kind >= HONEY_TYPE_F32 &&
                       symtab->symbols[i].type.kind <= HONEY_TYPE_F64) {
              make_float(symtab->symbols[i].comptime_value.float_value);
            }
          }
          // runtime dependent functions and variables can't be used in comptime
          return make_invalid();
        }
      }
      // unknown symbol
      return make_invalid();
    }

    // function calls, variables, etc. are not comptime-evaluable (YET!)
    case HONEY_AST_CALL_EXPR:
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
