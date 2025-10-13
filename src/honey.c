#include "honey.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

struct honey_context*
honey_create_context()
{
  static struct honey_context honey_ctx = { 0 };
  return &honey_ctx;
}

void
honey_destroy_context(struct honey_context* ctx)
{
  for (int i = 0; i < HONEY_MAX_TOKEN_COUNT; i += 1) {
    free(ctx->tokens[i].data.value);
  }
}

void
honey_scan(struct honey_context* ctx, const char* src)
{
  for (ctx->next_src_idx = 0; src[ctx->next_src_idx] != '\0';
       ctx->next_src_idx += 1) {
    if (isalpha(src[ctx->next_src_idx]) != 0) {
      struct honey_token name_token = { 0 };
      honey_parse_name(ctx, src, &name_token);
      ctx->tokens[ctx->next_token_idx] = name_token;
      ctx->next_token_idx += 1;
    } else if (src[ctx->next_src_idx] == ':') {
      if (src[ctx->next_src_idx + 1] == ':') {
        ctx->next_src_idx += 1; // step
        ctx->tokens[ctx->next_token_idx] = (struct honey_token){
          .kind = HONEY_TOKEN_DOUBLE_COLON,
        };
        ctx->next_token_idx += 1;
      }
    } else if (isdigit(src[ctx->next_src_idx]) != 0) {
      struct honey_token number_token = { 0 };
      honey_parse_number(ctx, src, &number_token);
      ctx->tokens[ctx->next_token_idx] = number_token;
      ctx->next_token_idx += 1;
    }
  }
}

// caller owns allocated token.data.name
void
honey_parse_name(struct honey_context* ctx,
                 const char* src,
                 struct honey_token* out)
{
  // find the bounds of the name
  int start = ctx->next_src_idx;
  int next = ctx->next_src_idx;
  while (src[next] != ' ') {
    next += 1;
  }

  // copy over the name
  out->kind = HONEY_TOKEN_NAME;
  int len = next - start;
  out->data.value = malloc(len + 1); // +1 for null terminator
  strncpy(out->data.value, src + start, len);
  out->data.value[len] = '\0';

  // update the current position for the lexer
  ctx->next_src_idx = next;
  ctx->next_src_idx =
    next - 1; // point to the last char (scan function increments)
}

// caller owns allocated token.data.name
void
honey_parse_number(struct honey_context* ctx,
                   const char* src,
                   struct honey_token* out)
{
  bool has_decimal = false;
  int start = ctx->next_src_idx;
  int next = ctx->next_src_idx;
  while (isdigit(src[next]) != 0 || src[next] == '.') {
    if (src[next] == '.')
      has_decimal = true;
    next += 1;
  }

  if (has_decimal)
    out->kind = HONEY_TOKEN_FLOAT;
  else
    out->kind = HONEY_TOKEN_INT;

  // copy over the number
  int len = next - start;
  out->data.value = malloc(len + 1); // +1 for null terminator
  strncpy(out->data.value, src + start, len);
  out->data.value[len] = '\0';

  // update the current position
  ctx->next_src_idx =
    next - 1; // point to the last char (scan function increments)
}
