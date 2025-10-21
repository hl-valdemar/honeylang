#include "../src/honey/context.h"
#include "../src/honey/lexer.h"
#include "../src/honey/test-framework.h"
#include "../src/honey/token.h"

#define LOG_DISABLED 1

static bool
test_scan_integer(void)
{
  struct honey_context* ctx = honey_context_create();
  honey_scan(ctx, "42");

  ASSERT_EQ(ctx->next_token_idx, 2, "should have 2 tokens (int + EOF)");
  ASSERT_EQ(ctx->tokens[0].kind, HONEY_TOKEN_INT, "first token should be INT");
  ASSERT_STR_EQ(ctx->tokens[0].value, "42", "value should be 42");
  ASSERT_EQ(ctx->tokens[1].kind, HONEY_TOKEN_EOF, "second token should be EOF");

  honey_context_destroy(ctx);
  return true;
}

static bool
test_scan_float(void)
{
  struct honey_context* ctx = honey_context_create();
  honey_scan(ctx, "3.14");

  ASSERT_EQ(ctx->next_token_idx, 2, "should have 2 tokens");
  ASSERT_EQ(
    ctx->tokens[0].kind, HONEY_TOKEN_FLOAT, "first token should be FLOAT");
  ASSERT_STR_EQ(ctx->tokens[0].value, "3.14", "value should be 3.14");

  honey_context_destroy(ctx);
  return true;
}

static bool
test_scan_keywords(void)
{
  struct honey_context* ctx = honey_context_create();
  honey_scan(ctx, "func return defer");

  ASSERT_EQ(ctx->next_token_idx, 4, "should have 4 tokens");
  ASSERT_EQ(ctx->tokens[0].kind, HONEY_TOKEN_FUNC, "should recognize func");
  ASSERT_EQ(ctx->tokens[1].kind, HONEY_TOKEN_RETURN, "should recognize return");
  ASSERT_EQ(ctx->tokens[2].kind, HONEY_TOKEN_DEFER, "should recognize defer");

  honey_context_destroy(ctx);
  return true;
}

static bool
test_scan_operators(void)
{
  struct honey_context* ctx = honey_context_create();
  honey_scan(ctx, "+ - * / = ==");

  ASSERT_EQ(ctx->next_token_idx, 7, "should have 7 tokens");
  ASSERT_EQ(ctx->tokens[0].kind, HONEY_TOKEN_PLUS, "should be PLUS");
  ASSERT_EQ(ctx->tokens[1].kind, HONEY_TOKEN_MINUS, "should be MINUS");
  ASSERT_EQ(ctx->tokens[2].kind, HONEY_TOKEN_STAR, "should be STAR");
  ASSERT_EQ(ctx->tokens[3].kind, HONEY_TOKEN_SLASH, "should be SLASH");
  ASSERT_EQ(ctx->tokens[4].kind, HONEY_TOKEN_EQUAL, "should be EQUAL");
  ASSERT_EQ(
    ctx->tokens[5].kind, HONEY_TOKEN_DOUBLE_EQUAL, "should be DOUBLE_EQUAL");

  honey_context_destroy(ctx);
  return true;
}

int
main(void)
{
  printf("\n=== lexer tests ===\n\n");

  RUN_TEST(test_scan_integer);
  RUN_TEST(test_scan_float);
  RUN_TEST(test_scan_keywords);
  RUN_TEST(test_scan_operators);

  print_test_summary();

  return test_stats.failed == 0 ? 0 : 1;
}
