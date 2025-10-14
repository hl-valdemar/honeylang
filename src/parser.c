#include "parser.h"
#include "log.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct honey_ast_node*
parse_declaration(struct honey_parser* p);

static struct honey_ast_node*
parse_expression(struct honey_parser* p);

static struct honey_ast_node*
parse_statement(struct honey_parser* p);

static struct honey_ast_node*
parse_block(struct honey_parser* p);

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
    p->current_token++;
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
parser_error(struct honey_parser* p, const char* message)
{
  struct honey_token* tok = current_token(p);

  if (tok) {
    honey_error(
      "parse error at line %d, column %d: %s", tok->line, tok->column, message);
    honey_error("  got: %s \"%s\"",
                honey_token_kind_to_text(tok->kind),
                tok->data.value ? tok->data.value : "");
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
      node = honey_ast_create(AST_LITERAL_INT);
      node->data.int_literal = atoll(tok->data.value);
      advance_token(p);
      break;
    }

    case HONEY_TOKEN_FLOAT: {
      node = honey_ast_create(AST_LITERAL_FLOAT);
      node->data.float_literal = atof(tok->data.value);
      advance_token(p);
      break;
    }

    case HONEY_TOKEN_NAME: {
      node = honey_ast_create(AST_NAME);
      node->data.name.identifier = strdup(tok->data.value);
      node->data.name.type = NULL;
      advance_token(p);
      break;
    }

    default:
      parser_error(p, "expected expression");
      return NULL;
  }

  return node;
}

static struct honey_ast_node*
parse_expression(struct honey_parser* p)
{
  // for now, just parse primary expressions
  // later: handle binary operators, function calls, etc.
  return parse_primary(p);
}

// parse a block: { statement* }
static struct honey_ast_node*
parse_block(struct honey_parser* p)
{
  if (!expect(p, HONEY_TOKEN_LBRACE, "expected \"{\"")) {
    return NULL;
  }

  struct honey_ast_node* block = honey_ast_create(AST_BLOCK);

  // dynamically grow statement list
  int capacity = 8;
  block->data.block.statements =
    malloc(sizeof(struct honey_ast_node*) * capacity);
  block->data.block.statement_count = 0;

  while (!check(p, HONEY_TOKEN_RBRACE) && !check(p, HONEY_TOKEN_EOF)) {
    struct honey_ast_node* stmt = parse_statement(p);
    if (!stmt) {
      honey_ast_destroy(block);
      return NULL;
    }

    // grow array if needed
    if (block->data.block.statement_count >= capacity) {
      capacity *= 2;
      block->data.block.statements =
        realloc(block->data.block.statements,
                sizeof(struct honey_ast_node*) * capacity);
    }

    block->data.block.statements[block->data.block.statement_count] = stmt;
    block->data.block.statement_count += 1;
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

  struct honey_ast_node* ret = honey_ast_create(AST_RETURN_STMT);

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
parse_statement(struct honey_parser* p)
{
  if (check(p, HONEY_TOKEN_RETURN)) {
    return parse_return_stmt(p);
  }

  // only return statements for now
  // later: variable declarations, if statements, etc...
  parser_error(p, "expected statement");
  return NULL;
}

// parse function declaration
// name :: func(params) : type { body }
// name :: func : type { body }
// name :: func { body }
static struct honey_ast_node*
parse_func_decl(struct honey_parser* p)
{
  struct honey_ast_node* node = honey_ast_create(AST_FUNC_DECL);

  // expect name
  struct honey_token* name_tok = current_token(p);
  if (!expect(p, HONEY_TOKEN_NAME, "expected function name")) {
    honey_ast_destroy(node);
    return NULL;
  }
  node->data.func_decl.name = strdup(name_tok->data.value);

  // expect : (type annotation)
  if (!expect(p, HONEY_TOKEN_COLON, "expected \":\"")) {
    honey_ast_destroy(node);
    return NULL;
  }

  // expect "func" keyword
  if (!expect(p, HONEY_TOKEN_FUNC, "expected \"func\"")) {
    honey_ast_destroy(node);
    return NULL;
  }

  // parse parameters if present: (param : type, ...)
  node->data.func_decl.params = NULL;
  node->data.func_decl.param_count = 0;

  if (match(p, HONEY_TOKEN_LPAREN)) {
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
        struct honey_ast_node* param = honey_ast_create(AST_NAME);
        param->data.name.identifier = strdup(param_name->data.value);
        param->data.name.type = strdup(param_type->data.value);

        // grow array if needed
        if (node->data.func_decl.param_count >= capacity) {
          capacity *= 2;
          node->data.func_decl.params =
            realloc(node->data.func_decl.params,
                    sizeof(struct honey_ast_node*) * capacity);
        }

        node->data.func_decl.params[node->data.func_decl.param_count++] = param;

      } while (match(p, HONEY_TOKEN_COMMA));
    }

    if (!expect(p, HONEY_TOKEN_RPAREN, "expected \")\"")) {
      honey_ast_destroy(node);
      return NULL;
    }
  }

  // parse return type if present: : return_type
  node->data.func_decl.return_type = NULL;
  if (match(p, HONEY_TOKEN_COLON)) {
    struct honey_token* ret_type = current_token(p);
    if (!expect(p, HONEY_TOKEN_NAME, "expected return type")) {
      honey_ast_destroy(node);
      return NULL;
    }
    node->data.func_decl.return_type = strdup(ret_type->data.value);
  }

  // expect :: (assignment)
  if (!expect(p, HONEY_TOKEN_DOUBLE_COLON, "expected \"::\"")) {
    honey_ast_destroy(node);
    return NULL;
  }

  // parse function body
  node->data.func_decl.body = parse_block(p);
  if (!node->data.func_decl.body) {
    honey_ast_destroy(node);
    return NULL;
  }

  return node;
}

// parse: NAME :: expression
// or:    NAME : type :: expression
static struct honey_ast_node*
parse_comptime_decl(struct honey_parser* p)
{
  struct honey_ast_node* node = honey_ast_create(AST_COMPTIME_DECL);

  // expect NAME
  struct honey_token* name_tok = current_token(p);
  if (!expect(
        p, HONEY_TOKEN_NAME, "expected identifier in comptime declaration")) {
    honey_ast_destroy(node);
    return NULL;
  }
  node->data.comptime_decl.name = strdup(name_tok->data.value);

  // check for explicit type: NAME : type :: value
  if (match(p, HONEY_TOKEN_COLON)) {
    struct honey_token* type_tok = current_token(p);
    if (!expect(p, HONEY_TOKEN_NAME, "expected type name after \":\"")) {
      honey_ast_destroy(node);
      return NULL;
    }
    node->data.comptime_decl.explicit_type = strdup(type_tok->data.value);
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
  struct honey_token* next2 = peek_token(p, 2);

  // check for function: name : func
  if (tok && tok->kind == HONEY_TOKEN_NAME && next &&
      next->kind == HONEY_TOKEN_COLON && next2 &&
      next2->kind == HONEY_TOKEN_FUNC) {
    return parse_func_decl(p);
  }

  // check for comptime declaration: NAME :: or NAME :
  if (tok && tok->kind == HONEY_TOKEN_NAME) {
    if ((next && next->kind == HONEY_TOKEN_DOUBLE_COLON) ||
        (next && next->kind == HONEY_TOKEN_COLON)) {
      return parse_comptime_decl(p);
    }
  }

  parser_error(p, "expected declaration");
  return NULL;
}

struct honey_ast_node*
honey_parse(struct honey_context* ctx)
{
  struct honey_parser parser = {
    .ctx = ctx,
    .current_token = 0,
    .had_error = false,
  };

  struct honey_ast_node* ast = parse_declaration(&parser);

  if (parser.had_error) {
    honey_ast_destroy(ast);
    return NULL;
  }

  return ast;
}
