#include "ast.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char*
binary_op_kind_to_text(enum honey_binary_op_kind kind)
{
  switch (kind) {
    case BINARY_OP_ADD:
      return "BINARY_OP_ADD";
      break;
    case BINARY_OP_SUB:
      return "BINARY_OP_SUB";
      break;
    case BINARY_OP_MUL:
      return "BINARY_OP_MUL";
      break;
    case BINARY_OP_DIV:
      return "BINARY_OP_DIV";
      break;
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
    case AST_COMPTIME_DECL:
      free(node->data.comptime_decl.name);
      free(node->data.comptime_decl.explicit_type);
      honey_ast_destroy(node->data.comptime_decl.value);
      break;

    case AST_FUNC_DECL:
      free(node->data.func_decl.name);
      free(node->data.func_decl.return_type);
      for (int i = 0; i < node->data.func_decl.param_count; i++) {
        honey_ast_destroy(node->data.func_decl.params[i]);
      }
      free(node->data.func_decl.params);
      honey_ast_destroy(node->data.func_decl.body);
      break;

    case AST_BLOCK:
      // regular statements
      for (int i = 0; i < node->data.block.statement_count; i++) {
        honey_ast_destroy(node->data.block.statements[i]);
      }
      free(node->data.block.statements);
      // defer statements
      for (int i = 0; i < node->data.block.deferred_count; i++) {
        honey_ast_destroy(node->data.block.deferred[i]);
      }
      free(node->data.block.deferred);
      break;

    case AST_BINARY_OP:
      honey_ast_destroy(node->data.binary_op.left);
      honey_ast_destroy(node->data.binary_op.right);
      break;

    case AST_RETURN_STMT:
      honey_ast_destroy(node->data.return_stmt.value);
      break;

    case AST_DEFER_STMT:
      honey_ast_destroy(node->data.defer_stmt.statement);
      break;

    case AST_NAME:
      free(node->data.name.identifier);
      free(node->data.name.type);
      break;

    case AST_LITERAL_INT:
    case AST_LITERAL_FLOAT:
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

  for (int i = 0; i < indent; i++)
    printf("  ");

  switch (node->kind) {
    case AST_COMPTIME_DECL:
      printf("comptime_decl: %s", node->data.comptime_decl.name);
      if (node->data.comptime_decl.explicit_type) {
        printf(" : %s", node->data.comptime_decl.explicit_type);
      }
      printf("\n");
      honey_ast_print(node->data.comptime_decl.value, indent + 1);
      break;

    case AST_FUNC_DECL:
      printf("func_decl: %s\n", node->data.func_decl.name);

      for (int i = 0; i < indent + 1; i++)
        printf("  ");
      printf("params:\n");
      for (int i = 0; i < node->data.func_decl.param_count; i++) {
        honey_ast_print(node->data.func_decl.params[i], indent + 2);
      }

      if (node->data.func_decl.return_type) {
        for (int i = 0; i < indent + 1; i++)
          printf("  ");
        printf("return_type: %s\n", node->data.func_decl.return_type);
      }

      for (int i = 0; i < indent + 1; i++)
        printf("  ");
      printf("body:\n");
      honey_ast_print(node->data.func_decl.body, indent + 2);
      break;

    case AST_BLOCK:
      printf("block:\n");
      // regular statements
      for (int i = 0; i < node->data.block.statement_count; i++) {
        honey_ast_print(node->data.block.statements[i], indent + 1);
      }
      // deferred statements
      if (node->data.block.deferred_count > 0) {
        for (int i = 0; i < indent + 1; i++)
          printf("  ");
        printf("deferred:\n");
        for (int i = 0; i < node->data.block.deferred_count; i++) {
          honey_ast_print(node->data.block.deferred[i], indent + 2);
        }
      }
      break;

    case AST_RETURN_STMT:
      printf("return:\n");
      if (node->data.return_stmt.value) {
        honey_ast_print(node->data.return_stmt.value, indent + 1);
      }
      break;

    case AST_DEFER_STMT:
      printf("defer:\n");
      if (node->data.defer_stmt.statement) {
        honey_ast_print(node->data.defer_stmt.statement, indent + 1);
      }
      break;

    case AST_BINARY_OP:
      printf("binary op: %s\n",
             binary_op_kind_to_text(node->data.binary_op.op));

      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("left:\n");
      honey_ast_print(node->data.binary_op.left, indent + 2);

      for (int i = 0; i < indent + 1; i += 1)
        printf("  ");
      printf("right:\n");
      honey_ast_print(node->data.binary_op.right, indent + 2);
      break;

    case AST_LITERAL_INT:
      printf("int_literal: %lld\n", node->data.int_literal);
      break;

    case AST_LITERAL_FLOAT:
      printf("float_literal: %f\n", node->data.float_literal);
      break;

    case AST_NAME:
      printf("name: %s", node->data.name.identifier);
      if (node->data.name.type) {
        printf(" : %s", node->data.name.type);
      }
      printf("\n");
      break;
  }
}
