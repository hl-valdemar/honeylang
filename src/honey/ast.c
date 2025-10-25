#include "honey/ast.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char*
honey_unary_op_kind_to_text(enum honey_unary_op_kind kind)
{
  switch (kind) {
#define X(name)                                                                \
  case name:                                                                   \
    return #name;
    HONEY_UNARY_OP_KINDS
#undef X
  }
}

const char*
honey_binary_op_kind_to_text(enum honey_binary_op_kind kind)
{
  switch (kind) {
#define X(name)                                                                \
  case name:                                                                   \
    return #name;
    HONEY_BINARY_OP_KINDS
#undef X
  }
}

struct honey_ast_node*
honey_ast_create(enum honey_ast_kind kind)
{
  struct honey_ast_node* node = calloc(1, sizeof(struct honey_ast_node));
  node->kind = kind;
  return node;
}

void
honey_ast_destroy(struct honey_ast_node* node)
{
  if (!node)
    return;

  switch (node->kind) {
    case HONEY_AST_COMPTIME_DECL:
      free(node->data.comptime_decl.name);
      free(node->data.comptime_decl.explicit_type);
      honey_ast_destroy(node->data.comptime_decl.value);
      break;

    case HONEY_AST_FUNC_DECL:
      free(node->data.func_decl.name);
      free(node->data.func_decl.return_type);
      for (int i = 0; i < node->data.func_decl.param_count; i += 1)
        honey_ast_destroy(node->data.func_decl.params[i]);

      free(node->data.func_decl.params);

      honey_ast_destroy(node->data.func_decl.body);
      break;

    case HONEY_AST_TEST_DECL:
      free(node->data.test_decl.name);
      honey_ast_destroy(node->data.test_decl.body);
      break;

    case HONEY_AST_CALL_EXPR:
      free(node->data.call_expr.function_name);
      for (int i = 0; i < node->data.call_expr.argument_count; i += 1)
        honey_ast_destroy(node->data.call_expr.arguments[i]);
      break;

    case HONEY_AST_IF_STMT:
      honey_ast_destroy(node->data.if_stmt.guard);
      honey_ast_destroy(node->data.if_stmt.if_body);
      honey_ast_destroy(node->data.if_stmt.else_body);
      for (int i = 0; i < node->data.if_stmt.else_if_count; i += 1) {
        honey_ast_destroy(node->data.if_stmt.else_ifs[i]->guard);
        honey_ast_destroy(node->data.if_stmt.else_ifs[i]->body);
        free(node->data.if_stmt.else_ifs[i]); // free struct itself
      }
      free(node->data.if_stmt.else_ifs); // free the array
      break;

    case HONEY_AST_BLOCK:
      // regular statements
      for (int i = 0; i < node->data.block.statement_count; i += 1)
        honey_ast_destroy(node->data.block.statements[i]);

      free(node->data.block.statements);

      // defer statements
      for (int i = 0; i < node->data.block.deferred_count; i += 1)
        honey_ast_destroy(node->data.block.deferred[i]);

      free(node->data.block.deferred);
      break;

    case HONEY_AST_UNARY_OP:
      honey_ast_destroy(node->data.unary_op.operand);
      break;

    case HONEY_AST_BINARY_OP:
      honey_ast_destroy(node->data.binary_op.left);
      honey_ast_destroy(node->data.binary_op.right);
      break;

    case HONEY_AST_RETURN_STMT:
      honey_ast_destroy(node->data.return_stmt.value);
      break;

    case HONEY_AST_DEFER_STMT:
      honey_ast_destroy(node->data.defer_stmt.statement);
      break;

    case HONEY_AST_NAME:
      free(node->data.name.identifier);
      free(node->data.name.type);
      break;

    case HONEY_AST_VAR_DECL:
      free(node->data.var_decl.name);
      free(node->data.var_decl.type);
      honey_ast_destroy(node->data.var_decl.value);
      break;

    case HONEY_AST_ASSIGNMENT:
      free(node->data.assignment.name);
      honey_ast_destroy(node->data.assignment.value);
      break;

    case HONEY_AST_LITERAL_INT:
    case HONEY_AST_LITERAL_FLOAT:
    case HONEY_AST_LITERAL_BOOL:
      // no heap data to free
      break;
  }

  free(node);
}

void
honey_ast_print(struct honey_ast_node* node, int indent)
{
  if (!node)
    return;

  for (int i = 0; i < indent; i += 1)
    printf("  ");

  switch (node->kind) {
    case HONEY_AST_COMPTIME_DECL:
      printf("comptime_decl: %s", node->data.comptime_decl.name);
      if (node->data.comptime_decl.explicit_type)
        printf(" : %s", node->data.comptime_decl.explicit_type);

      printf("\n");
      honey_ast_print(node->data.comptime_decl.value, indent + 1);
      break;

    case HONEY_AST_IF_STMT:
      printf("if_stmt:\n");

      // if guard
      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("guard:\n");
      honey_ast_print(node->data.if_stmt.guard, indent + 2);

      // if body
      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("if_body:\n");
      honey_ast_print(node->data.if_stmt.if_body, indent + 2);

      // else ifs
      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("else_ifs:\n");
      for (int i = 0; i < node->data.if_stmt.else_if_count; i += 1) {
        for (int i = 0; i < indent + 2; i += 1)
          printf("  ");
        printf("else_if:\n");
        for (int i = 0; i < indent + 3; i += 1)
          printf("  ");
        printf("guard:\n");
        honey_ast_print(node->data.if_stmt.else_ifs[i]->guard, indent + 4);
        for (int i = 0; i < indent + 3; i += 1)
          printf("  ");
        printf("else_if_body:\n");
        honey_ast_print(node->data.if_stmt.else_ifs[i]->body, indent + 4);
      }

      // else body
      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("else_body:\n");
      honey_ast_print(node->data.if_stmt.else_body, indent + 2);
      break;

    case HONEY_AST_FUNC_DECL:
      printf("func_decl: %s\n", node->data.func_decl.name);

      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("params:\n");
      for (int i = 0; i < node->data.func_decl.param_count; i += 1)
        honey_ast_print(node->data.func_decl.params[i], indent + 2);

      if (node->data.func_decl.return_type) {
        for (int i = 0; i < indent + 1; i += 1)
          printf("  ");
        printf("return_type: %s\n", node->data.func_decl.return_type);
      }

      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("body:\n");
      honey_ast_print(node->data.func_decl.body, indent + 2);
      break;

    case HONEY_AST_TEST_DECL:
      printf("test_decl: %s\n", node->data.test_decl.name);
      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("body:\n");
      honey_ast_print(node->data.test_decl.body, indent + 2);
      break;

    case HONEY_AST_BLOCK:
      printf("block:\n");
      // regular statements
      for (int i = 0; i < node->data.block.statement_count; i += 1)
        honey_ast_print(node->data.block.statements[i], indent + 1);

      // deferred statements
      if (node->data.block.deferred_count > 0) {
        for (int i = 0; i < indent + 1; i += 1)
          printf("  ");
        printf("deferred:\n");
        for (int i = 0; i < node->data.block.deferred_count; i += 1)
          honey_ast_print(node->data.block.deferred[i], indent + 2);
      }
      break;

    case HONEY_AST_RETURN_STMT:
      printf("return:\n");
      if (node->data.return_stmt.value)
        honey_ast_print(node->data.return_stmt.value, indent + 1);

      break;

    case HONEY_AST_DEFER_STMT:
      printf("defer:\n");
      if (node->data.defer_stmt.statement)
        honey_ast_print(node->data.defer_stmt.statement, indent + 1);
      break;

    case HONEY_AST_UNARY_OP:
      printf("unary op: %s\n",
             honey_unary_op_kind_to_text(node->data.unary_op.op));
      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("operand:\n");
      honey_ast_print(node->data.unary_op.operand, indent + 2);
      break;

    case HONEY_AST_BINARY_OP:
      printf("binary op: %s\n",
             honey_binary_op_kind_to_text(node->data.binary_op.op));

      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("left:\n");
      honey_ast_print(node->data.binary_op.left, indent + 2);

      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("right:\n");
      honey_ast_print(node->data.binary_op.right, indent + 2);
      break;

    case HONEY_AST_CALL_EXPR:
      printf("call expr: %s\n", node->data.call_expr.function_name);
      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("arguments:\n");
      for (int i = 0; i < node->data.call_expr.argument_count; i += 1)
        honey_ast_print(node->data.call_expr.arguments[i], indent + 2);
      break;

    case HONEY_AST_VAR_DECL:
      printf("var decl:\n");
      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("name: %s\n", node->data.var_decl.name);
      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("type: %s\n", node->data.var_decl.type);
      if (node->data.var_decl.value)
        honey_ast_print(node->data.var_decl.value, indent + 1);
      break;

    case HONEY_AST_ASSIGNMENT:
      printf("assignment:\n");
      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("name: %s\n", node->data.assignment.name);
      honey_ast_print(node->data.var_decl.value, indent + 1);
      break;

    case HONEY_AST_LITERAL_INT:
      printf("int_literal: %lld\n", node->data.int_literal);
      break;

    case HONEY_AST_LITERAL_FLOAT:
      printf("float_literal: %f\n", node->data.float_literal);
      break;

    case HONEY_AST_LITERAL_BOOL:
      printf("bool_literal: %s\n", node->data.bool_literal ? "true" : "false");
      break;

    case HONEY_AST_NAME:
      printf("name: %s", node->data.name.identifier);
      if (node->data.name.type) {
        printf(" : %s", node->data.name.type);
      }
      printf("\n");
      break;
  }
}
