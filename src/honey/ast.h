#ifndef HONEY_AST_H
#define HONEY_AST_H

#include <stdbool.h>
#include <stdint.h>

// define all token kinds in one place
#define HONEY_AST_KINDS                                                        \
  X(HONEY_AST_COMPTIME_DECL)                                                   \
  X(HONEY_AST_FUNC_DECL)                                                       \
  X(HONEY_AST_TEST_DECL)                                                       \
  X(HONEY_AST_VAR_DECL)                                                        \
  X(HONEY_AST_ASSIGNMENT)                                                      \
  X(HONEY_AST_LITERAL_INT)                                                     \
  X(HONEY_AST_LITERAL_FLOAT)                                                   \
  X(HONEY_AST_LITERAL_BOOL)                                                    \
  X(HONEY_AST_RETURN_STMT)                                                     \
  X(HONEY_AST_DEFER_STMT)                                                      \
  X(HONEY_AST_IF_STMT)                                                         \
  X(HONEY_AST_NAME)                                                            \
  X(HONEY_AST_BLOCK)                                                           \
  X(HONEY_AST_UNARY_OP)                                                        \
  X(HONEY_AST_BINARY_OP)                                                       \
  X(HONEY_AST_CALL_EXPR)

enum honey_ast_kind
{
#define X(name) name,
  HONEY_AST_KINDS
#undef X
};

#define HONEY_UNARY_OP_KINDS                                                   \
  X(HONEY_UNARY_OP_NEG)                                                        \
  X(HONEY_UNARY_OP_NOT)                                                        \
  X(HONEY_UNARY_OP_BIT_NOT)

enum honey_unary_op_kind
{
#define X(name) name,
  HONEY_UNARY_OP_KINDS
#undef X
};

#define HONEY_BINARY_OP_KINDS                                                  \
  X(HONEY_BINARY_OP_ADD)                                                       \
  X(HONEY_BINARY_OP_SUB)                                                       \
  X(HONEY_BINARY_OP_MUL)                                                       \
  X(HONEY_BINARY_OP_DIV)                                                       \
  X(HONEY_BINARY_OP_OR)                                                        \
  X(HONEY_BINARY_OP_AND)                                                       \
  X(HONEY_BINARY_OP_LESS)                                                      \
  X(HONEY_BINARY_OP_GREATER)                                                   \
  X(HONEY_BINARY_OP_LESS_EQUAL)                                                \
  X(HONEY_BINARY_OP_GREATER_EQUAL)                                             \
  X(HONEY_BINARY_OP_EQUAL)                                                     \
  X(HONEY_BINARY_OP_BIT_OR)                                                    \
  X(HONEY_BINARY_OP_BIT_AND)                                                   \
  X(HONEY_BINARY_OP_BIT_XOR)                                                   \
  X(HONEY_BINARY_OP_BITSHIFT_LEFT)                                             \
  X(HONEY_BINARY_OP_BITSHIFT_RIGHT)

enum honey_binary_op_kind
{
#define X(name) name,
  HONEY_BINARY_OP_KINDS
#undef X
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
      char* return_type; // return type (null if none specified)
      bool is_comptime;  // whether function should be comptime-capable

      // function body (ast_block)
      struct honey_ast_node* body;
    } func_decl;

    // test declaration
    struct
    {
      char* name;                  // test name
      struct honey_ast_node* body; // AST_BLOCK
    } test_decl;

    // if statement
    struct
    {
      struct honey_ast_node* guard; // boolean expression
      struct honey_ast_node* if_body;
      struct honey_ast_node* else_body;

      struct
      {
        struct honey_ast_node* guard;
        struct honey_ast_node* body;
      }** else_ifs;

      int else_if_count;
    } if_stmt;

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

    // boolean literal
    bool bool_literal;

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

const char*
honey_unary_op_kind_to_text(enum honey_unary_op_kind kind);

const char*
honey_binary_op_kind_to_text(enum honey_binary_op_kind kind);

void
honey_ast_print(struct honey_ast_node* node, int indent);

#endif
