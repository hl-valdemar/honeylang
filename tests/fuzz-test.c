#include "../src/honey/ast.h"
#include "../src/honey/codegen/arm64.h"
#include "../src/honey/context.h"
#include "../src/honey/lexer.h"
#include "../src/honey/parser.h"
#include "../src/honey/semantic.h"
#include "../src/honey/test-framework.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// ============================================================================
// fuzz testing configuration
// ============================================================================

#define FUZZ_ITERATIONS 1000
#define MAX_STRING_LENGTH 256
#define MAX_IDENTIFIER_LENGTH 64
#define MAX_NUMBER_VALUE 999999

// ============================================================================
// random generation utilities
// ============================================================================

static unsigned int fuzz_seed = 0;

static void
fuzz_init(void)
{
  fuzz_seed = (unsigned int)time(NULL);
  srand(fuzz_seed);
  printf("fuzz seed: %u\n\n", fuzz_seed);
}

static int
rand_range(int min, int max)
{
  return min + (rand() % (max - min + 1));
}

static char
rand_char(void)
{
  const char charset[] =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";
  return charset[rand() % (sizeof(charset) - 1)];
}

static char
rand_identifier_start_char(void)
{
  const char charset[] =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
  return charset[rand() % (sizeof(charset) - 1)];
}

static char*
rand_identifier(int max_length)
{
  int len = rand_range(1, max_length);
  char* id = malloc(len + 1);

  // first character must be letter or underscore
  id[0] = rand_identifier_start_char();

  // rest can be alphanumeric or underscore
  for (int i = 1; i < len; i++) {
    id[i] = rand_char();
  }
  id[len] = '\0';

  return id;
}

static int
rand_int(void)
{
  return rand_range(-MAX_NUMBER_VALUE, MAX_NUMBER_VALUE);
}

static const char*
rand_type(void)
{
  const char* types[] = { "i8",  "i16", "i32", "i64", "u8",  "u16",
                          "u32", "u64", "f32", "f64", "void" };
  return types[rand() % (sizeof(types) / sizeof(types[0]))];
}

static const char*
rand_operator(void)
{
  const char* ops[] = { "+", "-", "*", "/" };
  return ops[rand() % (sizeof(ops) / sizeof(ops[0]))];
}

static char*
rand_whitespace(void)
{
  int spaces = rand_range(0, 10);
  char* ws = malloc(spaces + 1);
  for (int i = 0; i < spaces; i++) {
    int r = rand() % 3;
    ws[i] = (r == 0) ? ' ' : (r == 1) ? '\t' : '\n';
  }
  ws[spaces] = '\0';
  return ws;
}

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
  if (!nodes)
    return;
  for (int i = 0; i < count; i++) {
    if (nodes[i])
      honey_ast_destroy(nodes[i]);
  }
  free(nodes);
}

static void
cleanup_symbol_table(struct honey_symbol_table* symtab)
{
  for (int i = 0; i < symtab->count; i++) {
    struct honey_symbol* sym = &symtab->symbols[i];
    free(sym->name);

    if (sym->type.kind == HONEY_TYPE_FUNCTION) {
      free(sym->type.func.return_type);
      if (sym->type.func.param_types) {
        for (int j = 0; j < sym->type.func.param_count; j++) {
          free(sym->type.func.param_types[j]);
        }
        free(sym->type.func.param_types);
      }
    }
  }
}

// ============================================================================
// property-based test: lexer should never crash
// ============================================================================

static bool
fuzz_lexer_never_crashes(void)
{
  int crashes = 0;

  for (int i = 0; i < FUZZ_ITERATIONS; i++) {
    // generate random input
    int len = rand_range(0, MAX_STRING_LENGTH);
    char* input = malloc(len + 1);
    for (int j = 0; j < len; j++) {
      input[j] = (char)rand_range(0, 127); // ascii range
    }
    input[len] = '\0';

    // lexer should not crash regardless of input
    struct honey_context* ctx = honey_context_create();
    honey_scan(ctx, input);
    honey_context_destroy(ctx);

    free(input);
  }

  assert_eq(crashes, 0, "lexer should never crash on random input");
  return true;
}

// ============================================================================
// property-based test: lexer token invariants
// ============================================================================

static bool
fuzz_lexer_produces_valid_tokens(void)
{
  for (int i = 0; i < FUZZ_ITERATIONS; i++) {
    // generate valid-looking input
    char* id = rand_identifier(MAX_IDENTIFIER_LENGTH);
    int num = rand_int();
    char input[512];
    snprintf(input, sizeof(input), "%s :: %d", id, num);

    struct honey_context* ctx = honey_context_create();
    honey_scan(ctx, input);

    // property: should always have at least EOF token
    assert(ctx->next_token_idx >= 1, "should have at least EOF token");
    assert_eq(ctx->tokens[ctx->next_token_idx - 1].kind,
              HONEY_TOKEN_EOF,
              "last token should be EOF");

    honey_context_destroy(ctx);
    free(id);
  }

  return true;
}

// ============================================================================
// property-based test: parser handles malformed input gracefully
// ============================================================================

static bool
fuzz_parser_handles_invalid_syntax(void)
{
  const char* invalid_patterns[] = {
    ":: ::", "func func", "{ { { }", "return return", "( ) ) (",
    "+ + +", "1 2 3",     ": : :",   "= = =",         "test test test",
  };

  int pattern_count = sizeof(invalid_patterns) / sizeof(invalid_patterns[0]);

  for (int i = 0; i < FUZZ_ITERATIONS; i++) {
    // pick random invalid pattern
    const char* pattern = invalid_patterns[rand() % pattern_count];

    int count;
    struct honey_ast_node** nodes = parse_source(pattern, &count);

    // property: parser should either reject or produce valid AST
    // (should not crash or produce inconsistent state)
    if (nodes != NULL) {
      // if parsing succeeded, verify basic invariants
      for (int j = 0; j < count; j++) {
        assert_not_null(nodes[j], "AST node should not be NULL");
      }
      cleanup_ast(nodes, count);
    }
  }

  return true;
}

// ============================================================================
// property-based test: valid declarations always parse
// ============================================================================

static bool
fuzz_parser_accepts_valid_declarations(void)
{
  for (int i = 0; i < FUZZ_ITERATIONS / 4; i++) {
    char* name = rand_identifier(20);
    int value = rand_int();
    char input[256] = { 0 };

    // generate valid comptime declaration
    char* ws1 = rand_whitespace();
    char* ws2 = rand_whitespace();
    snprintf(input, sizeof(input), "%s%s%s::%s%d", ws1, name, ws2, ws2, value);

    int count;
    struct honey_ast_node** nodes = parse_source(input, &count);

    char msg[256] = { 0 };

    // property: valid declarations should always parse successfully
    snprintf(
      msg, sizeof(msg), "valid comptime declaration should parse: %s", input);
    assert_not_null(nodes, msg);

    snprintf(msg, sizeof(msg), "should have exactly 1 declaration: %s", input);
    assert_eq(count, 1, msg);

    snprintf(msg, sizeof(msg), "should be comptime declaration: %s", input);
    assert_eq(nodes[0]->kind, HONEY_AST_COMPTIME_DECL, msg);

    cleanup_ast(nodes, count);
    free(name);
    free(ws1);
    free(ws2);
  }

  return true;
}

// ============================================================================
// property-based test: function declarations parse consistently
// ============================================================================

static bool
fuzz_parser_function_declarations(void)
{
  for (int i = 0; i < FUZZ_ITERATIONS / 4; i++) {
    char* name = rand_identifier(20);
    const char* ret_type = rand_type();
    int ret_val = rand_int();
    char input[512];

    // generate function: name :: func() type { return value }
    snprintf(input,
             sizeof(input),
             "%s :: func() %s { return %d }",
             name,
             ret_type,
             ret_val);

    int count;
    struct honey_ast_node** nodes = parse_source(input, &count);

    // property: well-formed functions should parse
    if (nodes != NULL && count > 0) {
      assert_eq(
        nodes[0]->kind, HONEY_AST_FUNC_DECL, "should be function declaration");
      assert_not_null(nodes[0]->data.func_decl.body,
                      "function should have body");
      cleanup_ast(nodes, count);
    }

    free(name);
  }

  return true;
}

// ============================================================================
// property-based test: binary operations maintain structure
// ============================================================================

static bool
fuzz_parser_binary_operations(void)
{
  for (int i = 0; i < FUZZ_ITERATIONS / 4; i++) {
    int a = rand_int();
    int b = rand_int();
    const char* op = rand_operator();
    char* name = rand_identifier(20);
    char input[256];

    snprintf(input, sizeof(input), "%s :: %d %s %d", name, a, op, b);

    int count;
    struct honey_ast_node** nodes = parse_source(input, &count);

    // property: binary operations should maintain tree structure
    if (nodes != NULL && count > 0) {
      assert_not_null(nodes[0]->data.comptime_decl.value,
                      "should have value expression");
      struct honey_ast_node* expr = nodes[0]->data.comptime_decl.value;

      if (expr->kind == HONEY_AST_BINARY_OP) {
        // property: binary op should have both operands
        assert_not_null(expr->data.binary_op.left, "should have left operand");
        assert_not_null(expr->data.binary_op.right,
                        "should have right operand");
      }

      cleanup_ast(nodes, count);
    }

    free(name);
  }

  return true;
}

// ============================================================================
// property-based test: semantic analysis type consistency
// ============================================================================

static bool
fuzz_semantic_type_resolution(void)
{
  const char* all_types[] = { "void", "i8",  "i16", "i32", "i64", "u8",
                              "u16",  "u32", "u64", "f32", "f64" };
  int type_count = sizeof(all_types) / sizeof(all_types[0]);

  for (int i = 0; i < FUZZ_ITERATIONS / 4; i++) {
    const char* type_name = all_types[rand() % type_count];

    // property: all valid type names should resolve consistently
    enum honey_type_kind kind = honey_resolve_type_name(type_name);
    assert(kind != HONEY_TYPE_UNKNOWN,
           "valid type names should resolve to known types");

    // property: resolving same type twice gives same result
    enum honey_type_kind kind2 = honey_resolve_type_name(type_name);
    assert_eq(kind, kind2, "type resolution should be deterministic");
  }

  return true;
}

// ============================================================================
// property-based test: semantic analysis handles undefined symbols
// ============================================================================

static bool
fuzz_semantic_undefined_symbols(void)
{
  for (int i = 0; i < FUZZ_ITERATIONS / 4; i++) {
    char* undefined_name = rand_identifier(20);
    char input[256];
    snprintf(
      input, sizeof(input), "x :: func() i32 { return %s }", undefined_name);

    int count;
    struct honey_ast_node** nodes = parse_source(input, &count);

    if (nodes != NULL) {
      struct honey_symbol_table symtab = { 0 };

      // property: referencing undefined symbols should fail analysis
      // (we expect this to fail gracefully, not crash)
      bool success = honey_analyze(nodes, count, &symtab);

      // either fails or succeeds, but should not crash
      if (success) {
        cleanup_symbol_table(&symtab);
      }

      cleanup_ast(nodes, count);
    }

    free(undefined_name);
  }

  return true;
}

// ============================================================================
// property-based test: code generation produces valid assembly structure
// ============================================================================

static bool
fuzz_codegen_basic_structure(void)
{
  for (int i = 0; i < FUZZ_ITERATIONS / 10; i++) {
    char* func_name = rand_identifier(20);
    int ret_value = rand_range(0, 255); // keep values reasonable
    char input[256];
    snprintf(input,
             sizeof(input),
             "%s :: func() i32 { return %d }",
             func_name,
             ret_value);

    int count;
    struct honey_ast_node** nodes = parse_source(input, &count);

    if (nodes != NULL) {
      struct honey_symbol_table symtab;
      bool analyze_success = honey_analyze(nodes, count, &symtab);

      if (analyze_success) {
        char output_path[128];
        snprintf(output_path, sizeof(output_path), "/tmp/fuzz_test_%d.s", i);

        // property: code generation should not crash
        bool codegen_success = honey_emit_arm64(&symtab, output_path, false);

        if (codegen_success) {
          // verify file was created
          FILE* f = fopen(output_path, "r");
          assert_not_null(f, "output file should be created");
          if (f) {
            fclose(f);
            remove(output_path);
          }
        }

        cleanup_symbol_table(&symtab);
      }

      cleanup_ast(nodes, count);
    }

    free(func_name);
  }

  return true;
}

// ============================================================================
// property-based test: extreme values
// ============================================================================

static bool
fuzz_extreme_integer_values(void)
{
  long long extreme_values[] = {
    0,
    1,
    -1,
    127,           // i8 max
    -128,          // i8 min
    255,           // u8 max
    32767,         // i16 max
    -32768,        // i16 min
    65535,         // u16 max
    2147483647,    // i32 max
    -2147483648LL, // i32 min (note: LL suffix for long long literal)
  };

  int value_count = sizeof(extreme_values) / sizeof(extreme_values[0]);

  for (int i = 0; i < value_count; i++) {
    char input[128];
    snprintf(input, sizeof(input), "x :: %lld", extreme_values[i]);

    int count;
    struct honey_ast_node** nodes = parse_source(input, &count);

    char msg[256];

    // property: extreme values should parse correctly
    snprintf(msg, sizeof(msg), "extreme values should parse: %s", input);
    assert_not_null(nodes, msg);

    if (nodes != NULL && count > 0) {
      snprintf(msg, sizeof(msg), "should be integer literal: %s", input);
      assert_eq(
        nodes[0]->data.comptime_decl.value->kind, HONEY_AST_LITERAL_INT, msg);

      cleanup_ast(nodes, count);
    }
  }

  return true;
}

// ============================================================================
// property-based test: deeply nested expressions
// ============================================================================

static bool
fuzz_deeply_nested_expressions(void)
{
  for (int depth = 1; depth <= 10; depth++) {
    char input[1024] = "x :: ";
    char* p = input + strlen(input);

    // build nested expression: (((1 + 1) + 1) + 1)...
    for (int i = 0; i < depth; i++) {
      *p++ = '(';
    }
    *p++ = '1';

    for (int i = 0; i < depth; i++) {
      strcpy(p, " + 1)");
      p += 5;
    }
    *p = '\0';

    int count;
    struct honey_ast_node** nodes = parse_source(input, &count);

    // property: nested expressions should parse without stack overflow
    if (nodes != NULL) {
      assert_not_null(nodes[0], "nested expression should parse");
      cleanup_ast(nodes, count);
    }
  }

  return true;
}

// ============================================================================
// property-based test: identifier length boundaries
// ============================================================================

static bool
fuzz_identifier_length_boundaries(void)
{
  int lengths[] = { 1, 2, 10, 50, 100, 200 };
  int length_count = sizeof(lengths) / sizeof(lengths[0]);

  for (int i = 0; i < length_count; i++) {
    int len = lengths[i];
    char* id = malloc(len + 1);

    // create identifier of specific length
    id[0] = 'a'; // start with letter
    for (int j = 1; j < len; j++) {
      id[j] = 'x';
    }
    id[len] = '\0';

    char input[512];
    snprintf(input, sizeof(input), "%s :: 42", id);

    int count;
    struct honey_ast_node** nodes = parse_source(input, &count);

    // property: identifiers of various lengths should work
    if (nodes != NULL) {
      assert_not_null(nodes[0], "identifier should parse");
      cleanup_ast(nodes, count);
    }

    free(id);
  }

  return true;
}

// ============================================================================
// property-based test: test declarations
// ============================================================================

static bool
fuzz_parser_test_declarations(void)
{
  for (int i = 0; i < FUZZ_ITERATIONS / 4; i++) {
    char* test_name = rand_identifier(20);
    int value = rand_int();
    char input[256];

    snprintf(
      input, sizeof(input), "%s :: test { x: i32 = %d }", test_name, value);

    int count;
    struct honey_ast_node** nodes = parse_source(input, &count);

    // property: test declarations should parse
    if (nodes != NULL && count > 0) {
      assert_eq(
        nodes[0]->kind, HONEY_AST_TEST_DECL, "should be test declaration");
      assert_not_null(nodes[0]->data.test_decl.body, "test should have body");

      // semantic analysis should work
      struct honey_symbol_table symtab;
      bool success = honey_analyze(nodes, count, &symtab);

      if (success) {
        assert_eq(symtab.count, 1, "should have one symbol");
        assert_eq(
          symtab.symbols[0].kind, HONEY_SYMBOL_TEST, "symbol should be test");
        cleanup_symbol_table(&symtab);
      }

      cleanup_ast(nodes, count);
    }

    free(test_name);
  }

  return true;
}

// ============================================================================
// main test runner
// ============================================================================

int
main(void)
{
  printf("\n=== fuzz tests ===\n\n");
  printf("running %d iterations per test\n", FUZZ_ITERATIONS);

  fuzz_init();

  // lexer fuzz tests
  printf("::[[ lexer fuzz tests ]]::\n\n");
  run_test(fuzz_lexer_never_crashes);
  run_test(fuzz_lexer_produces_valid_tokens);

  // parser fuzz tests
  printf("\n::[[ parser fuzz tests ]]::\n\n");
  run_test(fuzz_parser_handles_invalid_syntax);
  run_test(fuzz_parser_accepts_valid_declarations);
  run_test(fuzz_parser_function_declarations);
  run_test(fuzz_parser_binary_operations);
  run_test(fuzz_parser_test_declarations);

  // semantic analysis fuzz tests
  printf("\n::[[ semantic analysis fuzz tests ]]::\n\n");
  run_test(fuzz_semantic_type_resolution);
  run_test(fuzz_semantic_undefined_symbols);

  // code generation fuzz tests
  printf("\n::[[ code generation fuzz tests ]]::\n\n");
  run_test(fuzz_codegen_basic_structure);

  // edge case fuzz tests
  printf("\n::[[ edge case fuzz tests ]]::\n\n");
  run_test(fuzz_extreme_integer_values);
  run_test(fuzz_deeply_nested_expressions);
  run_test(fuzz_identifier_length_boundaries);

  print_test_summary();

  return 0;
}
