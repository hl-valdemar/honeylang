#include "honey/parser.h"
#include "honey/ast.h"
#include "honey/context.h"
#include "honey/log.h"
#include "honey/token.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// forward declarations
static struct honey_ast_node*
parse_declaration(struct honey_parser* p);
static struct honey_ast_node*
parse_expression(struct honey_parser* p);
static struct honey_ast_node*
parse_call_expr(struct honey_parser* p, char* function_name);
static struct honey_ast_node*
parse_statement(struct honey_parser* p);
static struct honey_ast_node*
parse_block(struct honey_parser* p);
static struct honey_ast_node*
parse_var_decl(struct honey_parser* p);
static struct honey_ast_node*
parse_assignment(struct honey_parser* p);

static struct honey_token*
current_token(struct honey_parser* p)
{
  if (p->current_token >= p->ctx->next_token_idx)
    return NULL;
  return &p->ctx->tokens[p->current_token];
}

static struct honey_token*
peek_token(struct honey_parser* p, int offset)
{
  int idx = p->current_token + offset;
  if (idx >= p->ctx->next_token_idx)
    return NULL;
  return &p->ctx->tokens[idx];
}

static void
advance_token(struct honey_parser* p)
{
  if (p->current_token < p->ctx->next_token_idx)
    p->current_token += 1;
}

static bool
check(struct honey_parser* p, enum honey_token_kind kind)
{
  struct honey_token* tok = current_token(p);
  return tok && tok->kind == kind;
}

static bool
match(struct honey_parser* p, enum honey_token_kind kind)
{
  if (check(p, kind)) {
    advance_token(p);
    return true;
  }
  return false;
}

static void
parser_error(struct honey_parser* p, [[maybe_unused]] const char* message)
{
  struct honey_token* tok = current_token(p);

  if (tok) {
    honey_error(
      "parse error at %sline %d%s, %scolumn %d%s: %s%s%s, %sgot %s \"%s\"%s",
      ANSI_COLOR_RED,
      tok->line,
      ANSI_COLOR_RESET,
      ANSI_COLOR_RED,
      tok->column,
      ANSI_COLOR_RESET,
      ANSI_COLOR_YELLOW,
      message,
      ANSI_COLOR_RESET,
      ANSI_COLOR_YELLOW,
      honey_token_kind_to_text(tok->kind),
      tok->value ? tok->value : "",
      ANSI_COLOR_RESET);
  } else {
    honey_error("parse error at EOF: %s", message);
  }

  p->had_error = true;
}

static bool
expect(struct honey_parser* p, enum honey_token_kind kind, const char* message)
{
  if (check(p, kind)) {
    advance_token(p);
    return true;
  }

  parser_error(p, message);
  return false;
}

static struct honey_ast_node*
parse_primary(struct honey_parser* p)
{
  struct honey_token* tok = current_token(p);

  if (!tok) {
    parser_error(p, "unexpected end of input");
    return NULL;
  }

  struct honey_ast_node* node = NULL;

  switch (tok->kind) {
    case HONEY_TOKEN_INT: {
      node = honey_ast_create(HONEY_AST_LITERAL_INT);
      node->data.int_literal = atoll(tok->value);
      advance_token(p);
      break;
    }

    case HONEY_TOKEN_FLOAT: {
      node = honey_ast_create(HONEY_AST_LITERAL_FLOAT);
      node->data.float_literal = atof(tok->value);
      advance_token(p);
      break;
    }

    case HONEY_TOKEN_BOOL: {
      node = honey_ast_create(HONEY_AST_LITERAL_BOOL);
      if (strcmp(tok->value, "true") == 0)
        node->data.bool_literal = true;
      else if (strcmp(tok->value, "false") == 0)
        node->data.bool_literal = false;
      else
        honey_error(
          "invalid boolean value \"%s\", must be either \"true\" or \"false\"",
          tok->value);
      break;
    }

    case HONEY_TOKEN_NAME: {
      char* name = strdup(tok->value);
      advance_token(p);

      // check if function call
      if (check(p, HONEY_TOKEN_LPAREN)) {
        advance_token(p); // consume "("
        node = parse_call_expr(p, name);
        if (!node) {
          free(name);
          return NULL;
        }
      }
      // just a name reference
      else {
        node = honey_ast_create(HONEY_AST_NAME);
        node->data.name.identifier = name;
        node->data.name.type = NULL;
      }

      break;
    }

    // parenthesized expression
    case HONEY_TOKEN_LPAREN: {
      advance_token(p);
      node = parse_expression(p);
      if (!node) {
        return NULL;
      }
      if (!expect(p, HONEY_TOKEN_RPAREN, "expected \")\"")) {
        honey_ast_destroy(node);
        return NULL;
      }
      break;
    }

    default:
      parser_error(p, "expected expression");
      return NULL;
  }

  return node;
}

static struct honey_ast_node*
parse_unary(struct honey_parser* p)
{
  if (match(p, HONEY_TOKEN_MINUS)) {
    // note: disallow chaining, e.g. '--x', as this has ambiguous semantics
    // across different languages

    struct honey_ast_node* unary = honey_ast_create(HONEY_AST_UNARY_OP);
    unary->data.unary_op.op = HONEY_UNARY_OP_NEG;
    unary->data.unary_op.operand = parse_primary(p);
    return unary;
  }

  return parse_primary(p);
}

// parse multiplication and division (higher precedence)
static struct honey_ast_node*
parse_term(struct honey_parser* p)
{
  struct honey_ast_node* left = parse_unary(p);
  if (!left)
    return NULL;

  while (check(p, HONEY_TOKEN_STAR)) {
    advance_token(p);

    struct honey_ast_node* right = parse_unary(p);
    if (!right) {
      honey_ast_destroy(left);
      return NULL;
    }

    struct honey_ast_node* binary = honey_ast_create(HONEY_AST_BINARY_OP);
    binary->data.binary_op.op = HONEY_BINARY_OP_MUL;
    binary->data.binary_op.left = left;
    binary->data.binary_op.right = right;

    left = binary;
  }

  return left;
}

// parse addition and subtraction (lower precedence)
static struct honey_ast_node*
parse_expression(struct honey_parser* p)
{
  struct honey_ast_node* left = parse_term(p);
  if (!left)
    return NULL;

  struct honey_token* next_tok = peek_token(p, 1);

  while (next_tok && (next_tok->kind == HONEY_TOKEN_PLUS ||
                      next_tok->kind == HONEY_TOKEN_GREATER ||
                      next_tok->kind == HONEY_TOKEN_LESS ||
                      next_tok->kind == HONEY_TOKEN_GREATER_EQUAL ||
                      next_tok->kind == HONEY_TOKEN_LESS_EQUAL ||
                      next_tok->kind == HONEY_TOKEN_DOUBLE_EQUAL)) {

    advance_token(p);

    struct honey_ast_node* right = parse_term(p);
    if (!right) {
      honey_ast_destroy(left);
      return NULL;
    }

    struct honey_ast_node* binary = honey_ast_create(HONEY_AST_BINARY_OP);
    binary->data.binary_op.op = HONEY_BINARY_OP_ADD;
    binary->data.binary_op.left = left;
    binary->data.binary_op.right = right;

    left = binary;
  }

  return left;
}

// parse a block: { statement* }
static struct honey_ast_node*
parse_block(struct honey_parser* p)
{
  if (!expect(p, HONEY_TOKEN_LBRACE, "expected \"{\"")) {
    return NULL;
  }

  struct honey_ast_node* block = honey_ast_create(HONEY_AST_BLOCK);

  // dynamically grow statement list
  int capacity_statements = 8;
  block->data.block.statements =
    malloc(sizeof(struct honey_ast_node*) * capacity_statements);
  block->data.block.statement_count = 0;

  // initialize deferred list
  int capacity_deferred = 4;
  block->data.block.deferred =
    malloc(sizeof(struct honey_ast_node*) * capacity_deferred);
  block->data.block.deferred_count = 0;

  while (!check(p, HONEY_TOKEN_RBRACE) && !check(p, HONEY_TOKEN_EOF)) {
    struct honey_ast_node* stmt = parse_statement(p);
    if (!stmt) {
      honey_ast_destroy(block);
      return NULL;
    }

    // if defer statement, add to deferred list
    if (stmt->kind == HONEY_AST_DEFER_STMT) {
      // grow if needed
      if (block->data.block.deferred_count >= capacity_deferred) {
        capacity_deferred *= 2;
        block->data.block.deferred =
          realloc(block->data.block.deferred,
                  sizeof(struct honey_ast_node*) * capacity_deferred);
      }

      // set data
      block->data.block.deferred[block->data.block.deferred_count] = stmt;
      block->data.block.deferred_count += 1;
    } else { // regular statement
      // grow if needed
      if (block->data.block.statement_count >= capacity_statements) {
        capacity_statements *= 2;
        block->data.block.statements =
          realloc(block->data.block.statements,
                  sizeof(struct honey_ast_node*) * capacity_statements);
      }

      // set data
      block->data.block.statements[block->data.block.statement_count] = stmt;
      block->data.block.statement_count += 1;
    }
  }

  if (!expect(p, HONEY_TOKEN_RBRACE, "expected \"}\"")) {
    honey_ast_destroy(block);
    return NULL;
  }

  return block;
}

// parse a return statement: return [expression]
static struct honey_ast_node*
parse_return_stmt(struct honey_parser* p)
{
  if (!expect(p, HONEY_TOKEN_RETURN, "expected \"return\"")) {
    return NULL;
  }

  struct honey_ast_node* ret = honey_ast_create(HONEY_AST_RETURN_STMT);

  // check for expression to return
  if (!check(p, HONEY_TOKEN_RBRACE) && !check(p, HONEY_TOKEN_EOF)) {
    ret->data.return_stmt.value = parse_expression(p);
    if (!ret->data.return_stmt.value) {
      honey_ast_destroy(ret);
      return NULL;
    }
  } else {
    ret->data.return_stmt.value = NULL;
  }

  return ret;
}

static struct honey_ast_node*
parse_defer_stmt(struct honey_parser* p)
{
  if (!expect(p, HONEY_TOKEN_DEFER, "expected \"defer\"")) {
    return NULL;
  }

  struct honey_ast_node* defer_node = honey_ast_create(HONEY_AST_DEFER_STMT);

  // parse the statement to defer
  defer_node->data.defer_stmt.statement = parse_statement(p);
  if (!defer_node->data.defer_stmt.statement) {
    honey_ast_destroy(defer_node);
    return NULL;
  }

  return defer_node;
}

// parse: if expr { body } else { body }
static struct honey_ast_node*
parse_if_stmt(struct honey_parser* p)
{
  // parse "if"
  if (!expect(p, HONEY_TOKEN_IF, "expected \"if\"")) {
    return NULL;
  }

  struct honey_ast_node* node = honey_ast_create(HONEY_AST_IF_STMT);

  // parse boolean expression (the gard)
  node->data.if_stmt.gard = parse_expression(p);
  if (!node->data.if_stmt.gard) {
    honey_ast_destroy(node);
    return NULL;
  }

  // parse the "if" body
  node->data.if_stmt.if_body = parse_block(p);
  if (!node->data.if_stmt.if_body) {
    honey_ast_destroy(node);
    return NULL;
  }

  struct honey_token* tok = current_token(p);
  struct honey_token* next_tok = peek_token(p, 1);

  // parse potential "else if" blocks
  while (tok && tok->kind == HONEY_TOKEN_ELSE && next_tok &&
         next_tok->kind == HONEY_TOKEN_IF) {
    // consume "else if" tokens
    advance_token(p);
    advance_token(p);

    int else_if_idx = node->data.if_stmt.else_if_count;

    // parse "else if" gard
    node->data.if_stmt.else_ifs[else_if_idx]->gard = parse_expression(p);
    if (!node->data.if_stmt.else_ifs[else_if_idx]->gard) {
      honey_ast_destroy(node);
      return NULL;
    }

    // parse "else if" body
    node->data.if_stmt.else_ifs[else_if_idx]->body = parse_block(p);
    if (!node->data.if_stmt.else_ifs[else_if_idx]->body) {
      honey_ast_destroy(node);
      return NULL;
    }

    node->data.if_stmt.else_if_count += 1;
  }

  // parse "else"
  if (match(p, HONEY_TOKEN_ELSE)) {
    node->data.if_stmt.else_body = parse_block(p);
    if (!node->data.if_stmt.else_body) {
      honey_ast_destroy(node);
      return NULL;
    }
  }

  return node;
}

static struct honey_ast_node*
parse_statement(struct honey_parser* p)
{
  if (check(p, HONEY_TOKEN_RETURN))
    return parse_return_stmt(p);
  if (check(p, HONEY_TOKEN_DEFER))
    return parse_defer_stmt(p);
  if (check(p, HONEY_TOKEN_IF))
    return parse_if_stmt(p);

  struct honey_token* next_tok = peek_token(p, 1);

  if (check(p, HONEY_TOKEN_MUT) || (check(p, HONEY_TOKEN_NAME) && next_tok &&
                                    next_tok->kind == HONEY_TOKEN_COLON)) {
    return parse_var_decl(p);
  }

  if (check(p, HONEY_TOKEN_NAME) && next_tok &&
      next_tok->kind == HONEY_TOKEN_EQUAL) {
    return parse_assignment(p);
  }

  parser_error(p, "expected statement");
  return NULL;
}

static struct honey_ast_node*
parse_var_decl(struct honey_parser* p)
{
  bool is_mutable = match(p, HONEY_TOKEN_MUT);

  struct honey_token* name_tok = current_token(p);
  if (!expect(p, HONEY_TOKEN_NAME, "expected variable name")) {
    return NULL;
  }

  if (!expect(p, HONEY_TOKEN_COLON, "expected \":\" after variable name")) {
    return NULL;
  }

  struct honey_token* type_tok = current_token(p);
  if (!expect(p, HONEY_TOKEN_NAME, "expected type name")) {
    return NULL;
  }

  struct honey_ast_node* node = honey_ast_create(HONEY_AST_VAR_DECL);
  node->data.var_decl.name = strdup(name_tok->value);
  node->data.var_decl.type = strdup(type_tok->value);
  node->data.var_decl.is_mutable = is_mutable;
  node->data.var_decl.value = NULL;

  // check for initial value
  if (match(p, HONEY_TOKEN_EQUAL)) {
    node->data.var_decl.value = parse_expression(p);
    if (!node->data.var_decl.value) {
      honey_ast_destroy(node);
      return NULL;
    }
  }

  return node;
}

static struct honey_ast_node*
parse_assignment(struct honey_parser* p)
{
  struct honey_token* name_tok = current_token(p);
  if (!expect(p, HONEY_TOKEN_NAME, "expected variable name")) {
    return NULL;
  }

  if (!expect(p, HONEY_TOKEN_EQUAL, "expected \"=\"")) {
    return NULL;
  }

  struct honey_ast_node* value = parse_expression(p);
  if (!value) {
    return NULL;
  }

  struct honey_ast_node* node = honey_ast_create(HONEY_AST_ASSIGNMENT);
  node->data.assignment.name = strdup(name_tok->value);
  node->data.assignment.value = value;

  return node;
}

// parse function declaration
// name :: func(params) type { body }
// name :: comptime func(params) type { body }
static struct honey_ast_node*
parse_func_decl(struct honey_parser* p)
{
  struct honey_ast_node* node = honey_ast_create(HONEY_AST_FUNC_DECL);

  // expect name
  struct honey_token* name_tok = current_token(p);
  if (!expect(p, HONEY_TOKEN_NAME, "expected function name")) {
    honey_ast_destroy(node);
    return NULL;
  }
  node->data.func_decl.name = strdup(name_tok->value);

  // expect :: (assignment)
  if (!expect(p, HONEY_TOKEN_DOUBLE_COLON, "expected \"::\"")) {
    honey_ast_destroy(node);
    return NULL;
  }

  // check for comptime keyword
  if (match(p, HONEY_TOKEN_COMPTIME)) {
    node->data.func_decl.is_comptime = true;
  }

  // expect "func" keyword
  if (!expect(p, HONEY_TOKEN_FUNC, "expected \"func\"")) {
    honey_ast_destroy(node);
    return NULL;
  }

  // parse parameters if present: (param: type, ...)
  node->data.func_decl.params = NULL;
  node->data.func_decl.param_count = 0;

  if (!expect(p, HONEY_TOKEN_LPAREN, "expected \"(\"")) {
    honey_ast_destroy(node);
    return NULL;
  }

  // parse parameter list
  int capacity = 4;
  node->data.func_decl.params =
    malloc(sizeof(struct honey_ast_node*) * capacity);

  if (!check(p, HONEY_TOKEN_RPAREN)) {
    do {
      // expect param_name : type
      struct honey_token* param_name = current_token(p);
      if (!expect(p, HONEY_TOKEN_NAME, "expected parameter name")) {
        honey_ast_destroy(node);
        return NULL;
      }

      if (!expect(
            p, HONEY_TOKEN_COLON, "expected \":\" after parameter name")) {
        honey_ast_destroy(node);
        return NULL;
      }

      struct honey_token* param_type = current_token(p);
      if (!expect(p, HONEY_TOKEN_NAME, "expected parameter type")) {
        honey_ast_destroy(node);
        return NULL;
      }

      // create parameter node
      struct honey_ast_node* param = honey_ast_create(HONEY_AST_NAME);
      param->data.name.identifier = strdup(param_name->value);
      param->data.name.type = strdup(param_type->value);

      // grow array if needed
      if (node->data.func_decl.param_count >= capacity) {
        capacity *= 2;
        node->data.func_decl.params =
          realloc(node->data.func_decl.params,
                  sizeof(struct honey_ast_node*) * capacity);
      }

      node->data.func_decl.params[node->data.func_decl.param_count] = param;
      node->data.func_decl.param_count += 1;

    } while (match(p, HONEY_TOKEN_COMMA));
  }

  if (!expect(p, HONEY_TOKEN_RPAREN, "expected \")\"")) {
    honey_ast_destroy(node);
    return NULL;
  }

  // parse return type (must be present!)
  node->data.func_decl.return_type = NULL;
  struct honey_token* ret_type = current_token(p);
  if (!expect(p, HONEY_TOKEN_NAME, "expected return type")) {
    honey_ast_destroy(node);
    return NULL;
  }
  node->data.func_decl.return_type = strdup(ret_type->value);

  // parse function body
  node->data.func_decl.body = parse_block(p);
  if (!node->data.func_decl.body) {
    honey_ast_destroy(node);
    return NULL;
  }

  return node;
}

// parse: NAME :: test { ... }
static struct honey_ast_node*
parse_test_decl(struct honey_parser* p)
{
  struct honey_ast_node* node = honey_ast_create(HONEY_AST_TEST_DECL);

  // expect name
  struct honey_token* name_tok = current_token(p);
  if (!expect(p, HONEY_TOKEN_NAME, "expected test name")) {
    honey_ast_destroy(node);
    return NULL;
  }
  node->data.test_decl.name = strdup(name_tok->value);

  // expect :: (assignment)
  if (!expect(p, HONEY_TOKEN_DOUBLE_COLON, "expected \"::\"")) {
    honey_ast_destroy(node);
    return NULL;
  }

  // expect test keyword
  if (!expect(p, HONEY_TOKEN_TEST, "expected \"test\"")) {
    honey_ast_destroy(node);
    return NULL;
  }

  // parse test body
  node->data.test_decl.body = parse_block(p);
  if (!node->data.test_decl.body) {
    honey_ast_destroy(node);
    return NULL;
  }

  return node;
}

static struct honey_ast_node*
parse_call_expr(struct honey_parser* p, char* function_name)
{
  struct honey_ast_node* node = honey_ast_create(HONEY_AST_CALL_EXPR);

  node->data.call_expr.function_name = function_name;
  node->data.call_expr.arguments = NULL;
  node->data.call_expr.argument_count = 0;

  // parse arguments if present: (argument, ...)
  int capacity = 4;
  node->data.call_expr.arguments =
    malloc(sizeof(struct honey_ast_node*) * capacity);

  if (!check(p, HONEY_TOKEN_RPAREN)) {
    do {
      // parse argument as full expression
      struct honey_ast_node* arg = parse_expression(p);
      if (!arg) {
        honey_ast_destroy(arg);
        return NULL;
      }

      // grow array if needed
      if (node->data.call_expr.argument_count >= capacity) {
        capacity *= 2;
        node->data.call_expr.arguments =
          realloc(node->data.call_expr.arguments,
                  sizeof(struct honey_ast_node*) * capacity);
      }

      // set data
      node->data.call_expr.arguments[node->data.call_expr.argument_count] = arg;
      node->data.call_expr.argument_count += 1;

    } while (match(p, HONEY_TOKEN_COMMA));
  }

  if (!expect(p, HONEY_TOKEN_RPAREN, "expected \")\"")) {
    honey_ast_destroy(node);
    return NULL;
  }

  return node;
}

// parse: NAME :: expression
// or:    NAME: type :: expression
static struct honey_ast_node*
parse_comptime_decl(struct honey_parser* p)
{
  struct honey_ast_node* node = honey_ast_create(HONEY_AST_COMPTIME_DECL);

  // expect NAME
  struct honey_token* name_tok = current_token(p);
  if (!expect(
        p, HONEY_TOKEN_NAME, "expected identifier in comptime declaration")) {
    honey_ast_destroy(node);
    return NULL;
  }
  node->data.comptime_decl.name = strdup(name_tok->value);

  // check for explicit type: (NAME: type :: value)
  if (match(p, HONEY_TOKEN_COLON)) {
    struct honey_token* type_tok = current_token(p);
    if (!expect(p, HONEY_TOKEN_NAME, "expected type name after \":\"")) {
      honey_ast_destroy(node);
      return NULL;
    }
    node->data.comptime_decl.explicit_type = strdup(type_tok->value);
  } else {
    node->data.comptime_decl.explicit_type = NULL;
  }

  // expect ::
  if (!expect(p,
              HONEY_TOKEN_DOUBLE_COLON,
              "expected \"::\" in comptime declaration")) {
    honey_ast_destroy(node);
    return NULL;
  }

  // parse value expression
  node->data.comptime_decl.value = parse_expression(p);
  if (!node->data.comptime_decl.value) {
    honey_ast_destroy(node);
    return NULL;
  }

  return node;
}

static struct honey_ast_node*
parse_declaration(struct honey_parser* p)
{
  struct honey_token* tok = current_token(p);
  struct honey_token* next = peek_token(p, 1);

  // handle (name :: func(params...) type {...}) and (name :: test {...})
  if (tok && tok->kind == HONEY_TOKEN_NAME && next &&
      next->kind == HONEY_TOKEN_DOUBLE_COLON) {

    // peek for func keyword
    struct honey_token* after_colon = peek_token(p, 2);
    if (after_colon && (after_colon->kind == HONEY_TOKEN_FUNC ||
                        after_colon->kind == HONEY_TOKEN_COMPTIME)) {
      return parse_func_decl(p);
    }
    // peek for test keyword
    else if (after_colon && after_colon->kind == HONEY_TOKEN_TEST) {
      return parse_test_decl(p);
    }
  }

  // handle (NAME: type :: value) (comptime with explicit type)
  if (tok && tok->kind == HONEY_TOKEN_NAME) {
    if ((next && next->kind == HONEY_TOKEN_DOUBLE_COLON) ||
        (next && next->kind == HONEY_TOKEN_COLON)) {
      return parse_comptime_decl(p);
    }
  }

  parser_error(p, "expected declaration");
  return NULL;
}

struct honey_ast_node**
honey_parse(struct honey_context* ctx, int* out_count)
{
  struct honey_parser parser = {
    .ctx = ctx,
    .current_token = 0,
    .had_error = false,
  };

  // parse all top-level declarations
  int capacity = 8;
  struct honey_ast_node** declarations =
    malloc(sizeof(struct honey_ast_node*) * capacity);
  int count = 0;

  while (!check(&parser, HONEY_TOKEN_EOF)) {
    struct honey_ast_node* decl = parse_declaration(&parser);
    if (!decl) {
      // clean up on error
      for (int i = 0; i < count; i += 1) {
        honey_ast_destroy(declarations[i]);
      }
      free(declarations);
      *out_count = 0;
      return NULL;
    }

    // grow array if needed
    if (count >= capacity) {
      capacity *= 2;
      declarations =
        realloc(declarations, sizeof(struct honey_ast_node*) * capacity);
    }

    declarations[count] = decl;
    count += 1;
  }

  *out_count = count;
  return declarations;
}
