#include "../src/honey/context.h"
#include "../src/honey/lexer.h"
#include "../src/honey/test-framework.h"
#include "../src/honey/token.h"

static bool
test_scan_integer(void)
{
  struct honey_context* ctx = honey_context_create();
  honey_scan(ctx, "42");

  assert_eq(ctx->next_token_idx, 2, "should have 2 tokens (int + EOF)");
  assert_eq(ctx->tokens[0].kind, HONEY_TOKEN_INT, "first token should be INT");
  assert_str_eq(ctx->tokens[0].value, "42", "value should be 42");
  assert_eq(ctx->tokens[1].kind, HONEY_TOKEN_EOF, "second token should be EOF");

  honey_context_destroy(ctx);
  return true;
}

static bool
test_scan_float(void)
{
  struct honey_context* ctx = honey_context_create();
  honey_scan(ctx, "3.14");

  assert_eq(ctx->next_token_idx, 2, "should have 2 tokens");
  assert_eq(
    ctx->tokens[0].kind, HONEY_TOKEN_FLOAT, "first token should be FLOAT");
  assert_str_eq(ctx->tokens[0].value, "3.14", "value should be 3.14");

  honey_context_destroy(ctx);
  return true;
}

static bool
test_scan_keywords(void)
{
  struct honey_context* ctx = honey_context_create();
  honey_scan(ctx, "func return defer");

  assert_eq(ctx->next_token_idx, 4, "should have 4 tokens");
  assert_eq(ctx->tokens[0].kind, HONEY_TOKEN_FUNC, "should recognize func");
  assert_eq(ctx->tokens[1].kind, HONEY_TOKEN_RETURN, "should recognize return");
  assert_eq(ctx->tokens[2].kind, HONEY_TOKEN_DEFER, "should recognize defer");

  honey_context_destroy(ctx);
  return true;
}

static bool
test_scan_operators(void)
{
  struct honey_context* ctx = honey_context_create();
  honey_scan(ctx, "+ - * / = ==");

  assert_eq(ctx->next_token_idx, 7, "should have 7 tokens");
  assert_eq(ctx->tokens[0].kind, HONEY_TOKEN_PLUS, "should be PLUS");
  assert_eq(ctx->tokens[1].kind, HONEY_TOKEN_MINUS, "should be MINUS");
  assert_eq(ctx->tokens[2].kind, HONEY_TOKEN_STAR, "should be STAR");
  assert_eq(ctx->tokens[3].kind, HONEY_TOKEN_SLASH, "should be SLASH");
  assert_eq(ctx->tokens[4].kind, HONEY_TOKEN_EQUAL, "should be EQUAL");
  assert_eq(
    ctx->tokens[5].kind, HONEY_TOKEN_DOUBLE_EQUAL, "should be DOUBLE_EQUAL");

  honey_context_destroy(ctx);
  return true;
}

int
main(void)
{
  printf("\n=== lexer tests ===\n\n");

  run_test(test_scan_integer);
  run_test(test_scan_float);
  run_test(test_scan_keywords);
  run_test(test_scan_operators);

  print_test_summary();

  return test_stats.failed == 0 ? 0 : 1;
}
