#include "../src/honey/ast.h"
#include "../src/honey/context.h"
#include "../src/honey/lexer.h"
#include "../src/honey/parser.h"
#include "../src/honey/test-framework.h"
#include <stdlib.h>

#define LOG_DISABLED 1

// ============================================================================
// helper functions
// ============================================================================

static struct honey_ast_node**
parse_source(const char* source, int* out_count)
{
  struct honey_context* ctx = honey_context_create();
  honey_scan(ctx, source);
  struct honey_ast_node** nodes = honey_parse(ctx, out_count);
  honey_context_destroy(ctx);
  return nodes;
}

static void
cleanup_ast(struct honey_ast_node** nodes, int count)
{
  for (int i = 0; i < count; i++) {
    honey_ast_destroy(nodes[i]);
  }
  free(nodes);
}

// ============================================================================
// literal tests
// ============================================================================

static bool
test_parse_integer_literal(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("answer :: 42", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  ASSERT_EQ(count, 1, "should have 1 declaration");
  ASSERT_EQ(nodes[0]->kind,
            HONEY_AST_COMPTIME_DECL,
            "should be comptime declaration");
  ASSERT_STR_EQ(nodes[0]->data.comptime_decl.name, "answer", "name should be answer");
  ASSERT_EQ(nodes[0]->data.comptime_decl.value->kind,
            HONEY_AST_LITERAL_INT,
            "value should be integer literal");
  ASSERT_EQ(nodes[0]->data.comptime_decl.value->data.int_literal,
            42,
            "integer value should be 42");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_float_literal(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("pi :: 3.14", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  ASSERT_EQ(count, 1, "should have 1 declaration");
  ASSERT_EQ(nodes[0]->data.comptime_decl.value->kind,
            HONEY_AST_LITERAL_FLOAT,
            "value should be float literal");

  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// binary operation tests
// ============================================================================

static bool
test_parse_addition(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("sum :: 2 + 3", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  ASSERT_EQ(count, 1, "should have 1 declaration");

  struct honey_ast_node* value = nodes[0]->data.comptime_decl.value;
  ASSERT_EQ(value->kind, HONEY_AST_BINARY_OP, "value should be binary operation");
  ASSERT_EQ(
    value->data.binary_op.op, HONEY_BINARY_OP_ADD, "operation should be ADD");
  ASSERT_EQ(value->data.binary_op.left->kind,
            HONEY_AST_LITERAL_INT,
            "left should be int literal");
  ASSERT_EQ(value->data.binary_op.left->data.int_literal, 2, "left value should be 2");
  ASSERT_EQ(value->data.binary_op.right->data.int_literal, 3, "right value should be 3");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_multiplication(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("product :: 4 * 5", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* value = nodes[0]->data.comptime_decl.value;
  ASSERT_EQ(
    value->data.binary_op.op, HONEY_BINARY_OP_MUL, "operation should be MUL");
  ASSERT_EQ(value->data.binary_op.left->data.int_literal, 4, "left should be 4");
  ASSERT_EQ(value->data.binary_op.right->data.int_literal, 5, "right should be 5");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_operator_precedence(void)
{
  // should parse as: 2 + (3 * 4)
  int count;
  struct honey_ast_node** nodes = parse_source("expr :: 2 + 3 * 4", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* value = nodes[0]->data.comptime_decl.value;

  // root should be addition
  ASSERT_EQ(
    value->data.binary_op.op, HONEY_BINARY_OP_ADD, "root should be addition");

  // left should be 2
  ASSERT_EQ(value->data.binary_op.left->kind,
            HONEY_AST_LITERAL_INT,
            "left should be int");
  ASSERT_EQ(value->data.binary_op.left->data.int_literal, 2, "left should be 2");

  // right should be multiplication (3 * 4)
  struct honey_ast_node* right = value->data.binary_op.right;
  ASSERT_EQ(
    right->kind, HONEY_AST_BINARY_OP, "right should be binary operation");
  ASSERT_EQ(right->data.binary_op.op,
            HONEY_BINARY_OP_MUL,
            "right should be multiplication");
  ASSERT_EQ(right->data.binary_op.left->data.int_literal, 3, "should be 3");
  ASSERT_EQ(right->data.binary_op.right->data.int_literal, 4, "should be 4");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_parenthesized_expression(void)
{
  // should parse as: (2 + 3) * 4
  int count;
  struct honey_ast_node** nodes = parse_source("expr :: (2 + 3) * 4", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* value = nodes[0]->data.comptime_decl.value;

  // root should be multiplication
  ASSERT_EQ(value->data.binary_op.op,
            HONEY_BINARY_OP_MUL,
            "root should be multiplication");

  // left should be addition (2 + 3)
  struct honey_ast_node* left = value->data.binary_op.left;
  ASSERT_EQ(
    left->kind, HONEY_AST_BINARY_OP, "left should be binary operation");
  ASSERT_EQ(left->data.binary_op.op,
            HONEY_BINARY_OP_ADD,
            "left should be addition");

  // right should be 4
  ASSERT_EQ(value->data.binary_op.right->data.int_literal, 4, "right should be 4");

  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// comptime declaration tests
// ============================================================================

static bool
test_parse_comptime_declaration(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("x :: 10", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  ASSERT_EQ(count, 1, "should have 1 declaration");
  ASSERT_EQ(nodes[0]->kind,
            HONEY_AST_COMPTIME_DECL,
            "should be comptime declaration");
  ASSERT_STR_EQ(nodes[0]->data.comptime_decl.name, "x", "name should be x");
  ASSERT_NULL(nodes[0]->data.comptime_decl.explicit_type,
              "should have no explicit type");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_comptime_declaration_with_type(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("x: i32 :: 10", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  ASSERT_EQ(count, 1, "should have 1 declaration");
  ASSERT_EQ(nodes[0]->kind,
            HONEY_AST_COMPTIME_DECL,
            "should be comptime declaration");
  ASSERT_STR_EQ(nodes[0]->data.comptime_decl.name, "x", "name should be x");
  ASSERT_NOT_NULL(nodes[0]->data.comptime_decl.explicit_type,
                  "should have explicit type");
  ASSERT_STR_EQ(nodes[0]->data.comptime_decl.explicit_type,
                "i32",
                "type should be i32");

  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// function declaration tests
// ============================================================================

static bool
test_parse_function_no_params(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("get_value :: func() i32 { return 42 }", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  ASSERT_EQ(count, 1, "should have 1 declaration");
  ASSERT_EQ(nodes[0]->kind, HONEY_AST_FUNC_DECL, "should be function declaration");
  ASSERT_STR_EQ(nodes[0]->data.func_decl.name, "get_value", "name should be get_value");
  ASSERT_EQ(nodes[0]->data.func_decl.param_count, 0, "should have 0 parameters");
  ASSERT_STR_EQ(nodes[0]->data.func_decl.return_type,
                "i32",
                "return type should be i32");
  ASSERT_NOT_NULL(nodes[0]->data.func_decl.body, "should have body");
  ASSERT_EQ(
    nodes[0]->data.func_decl.body->kind, HONEY_AST_BLOCK, "body should be block");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_function_with_params(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("add :: func(a: i32, b: i32) i32 { return a + b }", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  ASSERT_EQ(count, 1, "should have 1 declaration");
  ASSERT_EQ(nodes[0]->kind, HONEY_AST_FUNC_DECL, "should be function declaration");
  ASSERT_STR_EQ(nodes[0]->data.func_decl.name, "add", "name should be add");
  ASSERT_EQ(nodes[0]->data.func_decl.param_count, 2, "should have 2 parameters");

  // check first parameter
  struct honey_ast_node* param1 = nodes[0]->data.func_decl.params[0];
  ASSERT_EQ(param1->kind, HONEY_AST_NAME, "param should be NAME node");
  ASSERT_STR_EQ(param1->data.name.identifier, "a", "first param name should be a");
  ASSERT_STR_EQ(param1->data.name.type, "i32", "first param type should be i32");

  // check second parameter
  struct honey_ast_node* param2 = nodes[0]->data.func_decl.params[1];
  ASSERT_STR_EQ(param2->data.name.identifier, "b", "second param name should be b");
  ASSERT_STR_EQ(param2->data.name.type, "i32", "second param type should be i32");

  ASSERT_STR_EQ(nodes[0]->data.func_decl.return_type,
                "i32",
                "return type should be i32");

  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// test declaration tests
// ============================================================================

static bool
test_parse_test_declaration(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("simple_test :: test { x: i32 = 42 }", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  ASSERT_EQ(count, 1, "should have 1 declaration");
  ASSERT_EQ(nodes[0]->kind, HONEY_AST_TEST_DECL, "should be test declaration");
  ASSERT_STR_EQ(
    nodes[0]->data.test_decl.name, "simple_test", "name should be simple_test");
  ASSERT_NOT_NULL(nodes[0]->data.test_decl.body, "should have body");
  ASSERT_EQ(
    nodes[0]->data.test_decl.body->kind, HONEY_AST_BLOCK, "body should be block");

  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// statement tests (inside blocks)
// ============================================================================

static bool
test_parse_variable_declaration(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("test_var :: test { x: i32 = 5 }", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* body = nodes[0]->data.test_decl.body;
  ASSERT_EQ(body->data.block.statement_count, 1, "block should have 1 statement");

  struct honey_ast_node* stmt = body->data.block.statements[0];
  ASSERT_EQ(stmt->kind, HONEY_AST_VAR_DECL, "should be variable declaration");
  ASSERT_STR_EQ(stmt->data.var_decl.name, "x", "variable name should be x");
  ASSERT_STR_EQ(stmt->data.var_decl.type, "i32", "variable type should be i32");
  ASSERT(stmt->data.var_decl.is_mutable == false, "should not be mutable");
  ASSERT_NOT_NULL(stmt->data.var_decl.value, "should have initial value");
  ASSERT_EQ(stmt->data.var_decl.value->data.int_literal,
            5,
            "initial value should be 5");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_mutable_variable_declaration(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("test_mut :: test { mut x: i32 = 10 }", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* body = nodes[0]->data.test_decl.body;
  struct honey_ast_node* stmt = body->data.block.statements[0];

  ASSERT_EQ(stmt->kind, HONEY_AST_VAR_DECL, "should be variable declaration");
  ASSERT(stmt->data.var_decl.is_mutable == true, "should be mutable");
  ASSERT_STR_EQ(stmt->data.var_decl.name, "x", "variable name should be x");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_assignment(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("test_assign :: test { mut x: i32 = 5 x = 10 }", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* body = nodes[0]->data.test_decl.body;
  ASSERT_EQ(body->data.block.statement_count, 2, "block should have 2 statements");

  struct honey_ast_node* assign_stmt = body->data.block.statements[1];
  ASSERT_EQ(assign_stmt->kind, HONEY_AST_ASSIGNMENT, "should be assignment");
  ASSERT_STR_EQ(assign_stmt->data.assignment.name, "x", "assignment target should be x");
  ASSERT_EQ(assign_stmt->data.assignment.value->kind,
            HONEY_AST_LITERAL_INT,
            "value should be int literal");
  ASSERT_EQ(assign_stmt->data.assignment.value->data.int_literal,
            10,
            "value should be 10");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_return_statement(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("get_num :: func() i32 { return 99 }", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* body = nodes[0]->data.func_decl.body;
  ASSERT_EQ(body->data.block.statement_count, 1, "block should have 1 statement");

  struct honey_ast_node* ret_stmt = body->data.block.statements[0];
  ASSERT_EQ(ret_stmt->kind, HONEY_AST_RETURN_STMT, "should be return statement");
  ASSERT_NOT_NULL(ret_stmt->data.return_stmt.value, "should have return value");
  ASSERT_EQ(ret_stmt->data.return_stmt.value->data.int_literal,
            99,
            "return value should be 99");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_bare_return(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("do_nothing :: func() void { return }", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* body = nodes[0]->data.func_decl.body;
  ASSERT_EQ(body->data.block.statement_count, 1, "block should have 1 statement");

  struct honey_ast_node* ret_stmt = body->data.block.statements[0];
  ASSERT_EQ(ret_stmt->kind, HONEY_AST_RETURN_STMT, "should be return statement");
  ASSERT_NULL(ret_stmt->data.return_stmt.value, "should have no return value");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_defer_statement(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source(
    "test_defer :: test { x: i32 = 5 defer x = 10 }", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* body = nodes[0]->data.test_decl.body;

  // regular statement
  ASSERT_EQ(body->data.block.statement_count, 1, "block should have 1 statement");

  // deferred statement
  ASSERT_EQ(
    body->data.block.deferred_count, 1, "block should have 1 deferred statement");
  struct honey_ast_node* defer_stmt = body->data.block.deferred[0];
  ASSERT_EQ(defer_stmt->kind, HONEY_AST_DEFER_STMT, "should be defer statement");
  ASSERT_NOT_NULL(defer_stmt->data.defer_stmt.statement, "should have statement");
  ASSERT_EQ(defer_stmt->data.defer_stmt.statement->kind,
            HONEY_AST_ASSIGNMENT,
            "deferred statement should be assignment");

  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// function call tests
// ============================================================================

static bool
test_parse_function_call_no_args(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("result :: get_value()", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* value = nodes[0]->data.comptime_decl.value;

  ASSERT_EQ(value->kind, HONEY_AST_CALL_EXPR, "should be call expression");
  ASSERT_STR_EQ(value->data.call_expr.function_name,
                "get_value",
                "function name should be get_value");
  ASSERT_EQ(
    value->data.call_expr.argument_count, 0, "should have 0 arguments");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_function_call_with_args(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("result :: add(5, 10)", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* value = nodes[0]->data.comptime_decl.value;

  ASSERT_EQ(value->kind, HONEY_AST_CALL_EXPR, "should be call expression");
  ASSERT_STR_EQ(
    value->data.call_expr.function_name, "add", "function name should be add");
  ASSERT_EQ(
    value->data.call_expr.argument_count, 2, "should have 2 arguments");

  // check first argument
  struct honey_ast_node* arg1 = value->data.call_expr.arguments[0];
  ASSERT_EQ(arg1->kind, HONEY_AST_LITERAL_INT, "first arg should be int literal");
  ASSERT_EQ(arg1->data.int_literal, 5, "first arg should be 5");

  // check second argument
  struct honey_ast_node* arg2 = value->data.call_expr.arguments[1];
  ASSERT_EQ(arg2->kind, HONEY_AST_LITERAL_INT, "second arg should be int literal");
  ASSERT_EQ(arg2->data.int_literal, 10, "second arg should be 10");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_function_call_with_expression_args(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("result :: add(2 + 3, 4 * 5)", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* value = nodes[0]->data.comptime_decl.value;

  ASSERT_EQ(
    value->data.call_expr.argument_count, 2, "should have 2 arguments");

  // first argument should be 2 + 3
  struct honey_ast_node* arg1 = value->data.call_expr.arguments[0];
  ASSERT_EQ(arg1->kind, HONEY_AST_BINARY_OP, "first arg should be binary op");
  ASSERT_EQ(arg1->data.binary_op.op,
            HONEY_BINARY_OP_ADD,
            "first arg should be addition");

  // second argument should be 4 * 5
  struct honey_ast_node* arg2 = value->data.call_expr.arguments[1];
  ASSERT_EQ(arg2->kind, HONEY_AST_BINARY_OP, "second arg should be binary op");
  ASSERT_EQ(arg2->data.binary_op.op,
            HONEY_BINARY_OP_MUL,
            "second arg should be multiplication");

  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// block tests
// ============================================================================

static bool
test_parse_empty_block(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("empty :: func() void { }", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* body = nodes[0]->data.func_decl.body;

  ASSERT_EQ(body->kind, HONEY_AST_BLOCK, "should be block");
  ASSERT_EQ(body->data.block.statement_count, 0, "block should be empty");
  ASSERT_EQ(body->data.block.deferred_count, 0, "should have no deferred statements");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_parse_block_with_multiple_statements(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source(
    "multi :: test { x: i32 = 1 y: i32 = 2 x = 3 }", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* body = nodes[0]->data.test_decl.body;

  ASSERT_EQ(body->data.block.statement_count, 3, "block should have 3 statements");
  ASSERT_EQ(body->data.block.statements[0]->kind,
            HONEY_AST_VAR_DECL,
            "first should be var decl");
  ASSERT_EQ(body->data.block.statements[1]->kind,
            HONEY_AST_VAR_DECL,
            "second should be var decl");
  ASSERT_EQ(body->data.block.statements[2]->kind,
            HONEY_AST_ASSIGNMENT,
            "third should be assignment");

  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// name reference tests
// ============================================================================

static bool
test_parse_name_reference(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("value :: some_var", &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  struct honey_ast_node* value = nodes[0]->data.comptime_decl.value;

  ASSERT_EQ(value->kind, HONEY_AST_NAME, "should be name reference");
  ASSERT_STR_EQ(value->data.name.identifier, "some_var", "identifier should be some_var");
  ASSERT_NULL(value->data.name.type, "should have no type annotation");

  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// multiple declarations test
// ============================================================================

static bool
test_parse_multiple_declarations(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source(
    "x :: 10\n"
    "y :: 20\n"
    "add :: func(a: i32, b: i32) i32 { return a + b }\n"
    "my_test :: test { z: i32 = 30 }",
    &count);

  ASSERT_NOT_NULL(nodes, "parsing should succeed");
  ASSERT_EQ(count, 4, "should have 4 declarations");
  ASSERT_EQ(nodes[0]->kind,
            HONEY_AST_COMPTIME_DECL,
            "first should be comptime decl");
  ASSERT_EQ(nodes[1]->kind,
            HONEY_AST_COMPTIME_DECL,
            "second should be comptime decl");
  ASSERT_EQ(nodes[2]->kind, HONEY_AST_FUNC_DECL, "third should be func decl");
  ASSERT_EQ(nodes[3]->kind, HONEY_AST_TEST_DECL, "fourth should be test decl");

  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// error handling tests
// ============================================================================

static bool
test_parse_invalid_syntax_returns_null(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("invalid syntax here", &count);

  ASSERT_NULL(nodes, "parsing invalid syntax should return NULL");
  ASSERT_EQ(count, 0, "count should be 0");

  return true;
}

static bool
test_parse_incomplete_function_returns_null(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("func :: func(", &count);

  ASSERT_NULL(nodes, "parsing incomplete function should return NULL");
  ASSERT_EQ(count, 0, "count should be 0");

  return true;
}

// ============================================================================
// main test runner
// ============================================================================

int
main(void)
{
  printf("\n=== parser tests ===\n\n");

  // literal tests
  RUN_TEST(test_parse_integer_literal);
  RUN_TEST(test_parse_float_literal);

  // binary operation tests
  RUN_TEST(test_parse_addition);
  RUN_TEST(test_parse_multiplication);
  RUN_TEST(test_parse_operator_precedence);
  RUN_TEST(test_parse_parenthesized_expression);

  // comptime declaration tests
  RUN_TEST(test_parse_comptime_declaration);
  RUN_TEST(test_parse_comptime_declaration_with_type);

  // function declaration tests
  RUN_TEST(test_parse_function_no_params);
  RUN_TEST(test_parse_function_with_params);

  // test declaration tests
  RUN_TEST(test_parse_test_declaration);

  // statement tests
  RUN_TEST(test_parse_variable_declaration);
  RUN_TEST(test_parse_mutable_variable_declaration);
  RUN_TEST(test_parse_assignment);
  RUN_TEST(test_parse_return_statement);
  RUN_TEST(test_parse_bare_return);
  RUN_TEST(test_parse_defer_statement);

  // function call tests
  RUN_TEST(test_parse_function_call_no_args);
  RUN_TEST(test_parse_function_call_with_args);
  RUN_TEST(test_parse_function_call_with_expression_args);

  // block tests
  RUN_TEST(test_parse_empty_block);
  RUN_TEST(test_parse_block_with_multiple_statements);

  // name reference tests
  RUN_TEST(test_parse_name_reference);

  // multiple declarations test
  RUN_TEST(test_parse_multiple_declarations);

  // error handling tests
  RUN_TEST(test_parse_invalid_syntax_returns_null);
  RUN_TEST(test_parse_incomplete_function_returns_null);

  print_test_summary();

  return test_stats.failed == 0 ? 0 : 1;
}
