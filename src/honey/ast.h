#ifndef HONEY_AST_H
#define HONEY_AST_H

#include <stdbool.h>
#include <stdint.h>

enum honey_ast_kind
{
  // declarations and assignment
  HONEY_AST_COMPTIME_DECL, // NAME :: value
  HONEY_AST_FUNC_DECL,     // NAME :: func(...) : type { ... }
  HONEY_AST_TEST_DECL,     // NAME :: test { ... }
  HONEY_AST_VAR_DECL,      // local variable declaration
  HONEY_AST_ASSIGNMENT,    // local variable assignment

  // literals
  HONEY_AST_LITERAL_INT,   // 10
  HONEY_AST_LITERAL_FLOAT, // 3.14

  // statements
  HONEY_AST_RETURN_STMT, // return expr
  HONEY_AST_DEFER_STMT,  // defer statement

  // other
  HONEY_AST_NAME,      // identifier reference
  HONEY_AST_BLOCK,     // { statements }
  HONEY_AST_UNARY_OP,  // unary operation
  HONEY_AST_BINARY_OP, // binary operation
  HONEY_AST_CALL_EXPR, // function call
};

enum honey_unary_op_kind
{
  HONEY_UNARY_OP_NEG,    // -x (negation)
  HONEY_UNARY_OP_NOT,    // !x (logical not)
  HONEY_UNARY_OP_BITNOT, // ~x (bitwise not)
};

enum honey_binary_op_kind
{
  HONEY_BINARY_OP_ADD, // +
  HONEY_BINARY_OP_SUB, // -
  HONEY_BINARY_OP_MUL, // *
  HONEY_BINARY_OP_DIV, // /
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

    // test declaration
    struct
    {
      char* name;                  // test name
      struct honey_ast_node* body; // AST_BLOCK
    } test_decl;

    // variable declaration: name: type = value
    struct
    {
      char* name;
      char* type;                   // required for now
      struct honey_ast_node* value; // initial value (memzero if omitted)
      bool is_mutable;              // true if declared with 'mut'
    } var_decl;

    // assignment: name = value
    struct
    {
      char* name;
      struct honey_ast_node* value;
    } assignment;

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

    // unary operation
    struct
    {
      enum honey_unary_op_kind op;
      struct honey_ast_node* operand;
    } unary_op;

    // binary operation
    struct
    {
      enum honey_binary_op_kind op;
      struct honey_ast_node* left;
      struct honey_ast_node* right;
    } binary_op;

    struct
    {
      char* function_name;
      struct honey_ast_node** arguments;
      int argument_count;
    } call_expr;

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
