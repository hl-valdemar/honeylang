#ifndef HONEY_AST_H
#define HONEY_AST_H

#include <stdbool.h>
#include <stdint.h>

enum honey_ast_kind
{
  AST_COMPTIME_DECL, // NAME :: value
  AST_FUNC_DECL,     // NAME :: func(...) : type { ... }
  AST_LITERAL_INT,   // 10
  AST_LITERAL_FLOAT, // 3.14
  AST_NAME,          // identifier reference
  AST_BLOCK,         // { statements }
  AST_RETURN_STMT,   // return expr
  AST_DEFER_STMT,    // defer statement
  AST_BINARY_OP,     // binary operation
};

enum honey_binary_op_kind
{
  BINARY_OP_ADD, // +
  BINARY_OP_SUB, // -
  BINARY_OP_MUL, // *
  BINARY_OP_DIV, // /
};

struct honey_ast_node
{
  enum honey_ast_kind kind;

  union
  {
    // comptime declaration: NAME :: value or NAME : type :: value
    struct
    {
      char* name;
      char* explicit_type; // null if inferred
      struct honey_ast_node* value;
    } comptime_decl;

    // function declaration
    struct
    {
      char* name;

      // parameters (array of ast_name nodes with type annotations)
      struct honey_ast_node** params;
      int param_count;

      // return type (null if none specified)
      char* return_type;

      // function body (ast_block)
      struct honey_ast_node* body;
    } func_decl;

    // block statement
    struct
    {
      struct honey_ast_node** statements;
      int statement_count;

      // deferred statements (executed at end of block in reverse order)
      struct honey_ast_node** deferred;
      int deferred_count;
    } block;

    // return statement
    struct
    {
      struct honey_ast_node* value; // null for bare return
    } return_stmt;

    // defer statement
    struct
    {
      struct honey_ast_node* statement; // statement to defer
    } defer_stmt;

    // binary operation
    struct
    {
      enum honey_binary_op_kind op;
      struct honey_ast_node* left;
      struct honey_ast_node* right;
    } binary_op;

    // integer literal
    int64_t int_literal;

    // float literal
    double float_literal;

    // name reference (also used for parameters with type)
    struct
    {
      char* identifier;
      char* type; // null if no type annotation
    } name;
  } data;
};

struct honey_ast_node*
honey_ast_create(enum honey_ast_kind kind);

void
honey_ast_destroy(struct honey_ast_node* node);

void
honey_ast_print(struct honey_ast_node* node, int indent);

#endif
