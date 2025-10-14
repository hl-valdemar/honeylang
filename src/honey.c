#include "honey.h"
#include "log.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct honey_context*
honey_create_context()
{
  struct honey_context* ctx = calloc(1, sizeof(struct honey_context));
  ctx->current_line = 1;
  ctx->current_column = 1;
  return ctx;
}

void
honey_destroy_context(struct honey_context* ctx)
{
  if (!ctx)
    return;

  for (int i = 0; i < ctx->next_token_idx; i += 1) {
    free(ctx->tokens[i].data.value);
  }
  free(ctx);
}

static inline char
peek_char(struct honey_context* ctx)
{
  return ctx->src[ctx->next_src_idx];
}

static inline char
peek_char_offset(struct honey_context* ctx, int offset)
{
  return ctx->src[ctx->next_src_idx + offset];
}

static void
advance_char(struct honey_context* ctx)
{
  if (peek_char(ctx) == '\n') {
    ctx->current_line += 1;
    ctx->current_column = 1;
  } else {
    ctx->current_column += 1;
  }
  ctx->next_src_idx += 1;
}

static void
skip_whitespace_and_comments(struct honey_context* ctx)
{
  while (true) {
    char c = peek_char(ctx);

    // skip whitespace
    if (isspace(c)) {
      advance_char(ctx);
      continue;
    }

    // skip comments (# to end of line)
    if (c == '#') {
      while (peek_char(ctx) != '\0' && peek_char(ctx) != '\n') {
        advance_char(ctx);
      }
      continue;
    }

    break;
  }
}

static void
add_token(struct honey_context* ctx, struct honey_token token)
{
  if (ctx->next_token_idx >= HONEY_MAX_TOKEN_COUNT) {
    honey_error("too many tokens");
    return;
  }

  token.line = ctx->current_line;
  token.column = ctx->current_column;
  ctx->tokens[ctx->next_token_idx] = token;
  ctx->next_token_idx += 1;
}

static struct honey_token
make_simple_token(enum honey_token_kind kind)
{
  return (struct honey_token){
    .kind = kind,
    .data.value = NULL,
  };
}

static struct honey_token
make_value_token(enum honey_token_kind kind, const char* start, int len)
{
  struct honey_token token = {
    .kind = kind,
    .data.value = malloc(len + 1),
  };

  if (token.data.value) {
    strncpy(token.data.value, start, len);
    token.data.value[len] = '\0';
  }

  return token;
}

static void
scan_name(struct honey_context* ctx)
{
  const char* start = &ctx->src[ctx->next_src_idx];
  int len = 0;

  // first char must be alpha or underscore
  // then can be alphanumeric or underscore
  while (true) {
    char c = peek_char(ctx);
    if (c == '\0')
      break;

    if (len == 0) {
      if (!isalpha(c) && c != '_')
        break;
    } else {
      if (!isalnum(c) && c != '_')
        break;
    }

    advance_char(ctx);
    len += 1;
  }

  struct honey_token token = make_value_token(HONEY_TOKEN_NAME, start, len);
  add_token(ctx, token);
}

static void
scan_number(struct honey_context* ctx)
{
  const char* start = &ctx->src[ctx->next_src_idx];
  int len = 0;
  bool has_decimal = false;
  enum honey_token_kind kind = HONEY_TOKEN_INT;

  while (true) {
    char c = peek_char(ctx);

    if (isdigit(c)) {
      advance_char(ctx);
      len += 1;
    } else if (c == '.' && !has_decimal) {
      // check that next char is a digit (avoid "10." being treated as float)
      char next = peek_char_offset(ctx, 1);
      if (isdigit(next)) {
        has_decimal = true;
        kind = HONEY_TOKEN_FLOAT;
        advance_char(ctx);
        len += 1;
      } else {
        break;
      }
    } else {
      break;
    }
  }

  struct honey_token token = make_value_token(kind, start, len);
  add_token(ctx, token);
}

void
honey_scan(struct honey_context* ctx, const char* src)
{
  ctx->src = src;
  ctx->next_src_idx = 0;
  ctx->next_token_idx = 0;
  ctx->current_line = 1;
  ctx->current_column = 1;

  while (peek_char(ctx) != '\0') {
    skip_whitespace_and_comments(ctx);

    char c = peek_char(ctx);

    if (c == '\0') {
      break;
    }

    // identifiers and keywords
    if (isalpha(c) || c == '_') {
      scan_name(ctx);
    }
    // numbers
    else if (isdigit(c)) {
      scan_number(ctx);
    }
    // single colon
    else if (c == ':' && peek_char_offset(ctx, 1) != ':') {
      add_token(ctx, make_simple_token(HONEY_TOKEN_COLON));
      advance_char(ctx);
    }
    // double colon
    else if (c == ':' && peek_char_offset(ctx, 1) == ':') {
      add_token(ctx, make_simple_token(HONEY_TOKEN_DOUBLE_COLON));
      advance_char(ctx);
      advance_char(ctx);
    }
    // unknown character (warn and skip)
    else {
      honey_warn("unknown character '%s%c%s' at %sline %d%s, %scolumn %d%s",
                 ANSI_COLOR_RED,
                 c,
                 ANSI_COLOR_RESET,
                 ANSI_COLOR_RED,
                 ctx->current_line,
                 ANSI_COLOR_RESET,
                 ANSI_COLOR_RED,
                 ctx->current_column,
                 ANSI_COLOR_RESET);
      advance_char(ctx);
    }
  }

  // at last, add eof token
  add_token(ctx, make_simple_token(HONEY_TOKEN_EOF));
}
