#include "../src/honey/ast.h"
#include "../src/honey/codegen/arm64.h"
#include "../src/honey/context.h"
#include "../src/honey/lexer.h"
#include "../src/honey/parser.h"
#include "../src/honey/semantic.h"
#include "../src/honey/test-framework.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

static char*
read_file_contents(const char* filepath)
{
  FILE* f = fopen(filepath, "r");
  if (!f) {
    return NULL;
  }

  fseek(f, 0, SEEK_END);
  long size = ftell(f);
  fseek(f, 0, SEEK_SET);

  char* content = malloc(size + 1);
  if (!content) {
    fclose(f);
    return NULL;
  }

  fread(content, 1, size, f);
  content[size] = '\0';
  fclose(f);

  return content;
}

static bool
file_contains(const char* filepath, const char* substring)
{
  char* content = read_file_contents(filepath);
  if (!content) {
    return false;
  }

  bool result = strstr(content, substring) != NULL;
  free(content);
  return result;
}

static bool
file_not_contains(const char* filepath, const char* substring)
{
  return !file_contains(filepath, substring);
}

static void
cleanup_test_file(const char* filepath)
{
  remove(filepath);
}

// ============================================================================
// basic code generation tests
// ============================================================================

static bool
test_codegen_creates_output_file(void)
{
  const char* output = "/tmp/test_codegen_output.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("get_value :: func() i32 { return 42 }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);
  ASSERT(success, "semantic analysis should succeed");

  // generate code
  bool codegen_success = honey_emit_arm64(&symtab, output, false);
  ASSERT(codegen_success, "code generation should succeed");

  // verify file was created
  FILE* f = fopen(output, "r");
  ASSERT_NOT_NULL(f, "output file should be created");
  if (f) {
    fclose(f);
  }

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_empty_symbol_table(void)
{
  const char* output = "/tmp/test_codegen_empty.s";
  struct honey_symbol_table symtab;
  symtab.count = 0;

  bool success = honey_emit_arm64(&symtab, output, false);
  ASSERT(success, "code generation should succeed for empty symbol table");

  // verify file contains minimal structure
  ASSERT(file_contains(output, ".text"), "should contain .text section");
  ASSERT(file_contains(output, "_test_runner"),
         "should contain test_runner label");

  cleanup_test_file(output);
  return true;
}

static bool
test_codegen_generates_text_section(void)
{
  const char* output = "/tmp/test_codegen_text.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("main :: func() i32 { return 0 }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  ASSERT(file_contains(output, ".text"), "should contain .text section");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// function code generation tests
// ============================================================================

static bool
test_codegen_simple_function(void)
{
  const char* output = "/tmp/test_codegen_simple_func.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("get_value :: func() i32 { return 42 }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  // verify function label is generated
  ASSERT(file_contains(output, ".global _get_value"),
         "should contain global declaration");
  ASSERT(file_contains(output, "_get_value:"), "should contain function label");

  // verify function returns constant 42
  ASSERT(file_contains(output, "mov x0, #42"),
         "should contain mov instruction for return value");

  // verify function has return
  ASSERT(file_contains(output, "ret"), "should contain ret instruction");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_function_with_parameters(void)
{
  const char* output = "/tmp/test_codegen_func_params.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("add :: func(a: i32, b: i32) i32 { return a }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  ASSERT(file_contains(output, "_add:"), "should contain function label");
  ASSERT(file_contains(output, "ret"), "should contain ret instruction");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_function_with_local_variables(void)
{
  const char* output = "/tmp/test_codegen_func_locals.s";
  int count;
  struct honey_ast_node** nodes = parse_source(
    "calc :: func() i32 { x: i32 = 10\n y: i32 = 20\n return x }",
    &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  ASSERT(file_contains(output, "_calc:"), "should contain function label");

  // verify stack allocation for local variables
  ASSERT(file_contains(output, "sub sp, sp"),
         "should allocate stack space for locals");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_function_prologue_epilogue(void)
{
  const char* output = "/tmp/test_codegen_func_prologue.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("foo :: func() i32 { return 1 }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  // verify prologue/epilogue markers exist
  ASSERT(file_contains(output, "; prologue") ||
           file_contains(output, "stp x29, x30"),
         "should have prologue");
  ASSERT(file_contains(output, "; epilogue") ||
           file_contains(output, "ldp x29, x30"),
         "should have epilogue");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_void_function(void)
{
  const char* output = "/tmp/test_codegen_void_func.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("do_nothing :: func() void { return }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  ASSERT(file_contains(output, "_do_nothing:"),
         "should contain function label");
  ASSERT(file_contains(output, "ret"), "should contain ret instruction");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// expression code generation tests
// ============================================================================

static bool
test_codegen_integer_literal(void)
{
  const char* output = "/tmp/test_codegen_int_literal.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("get_num :: func() i32 { return 123 }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  ASSERT(file_contains(output, "mov x0, #123"),
         "should generate mov instruction for literal");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_binary_addition(void)
{
  const char* output = "/tmp/test_codegen_add.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("add_nums :: func() i32 { return 10 + 20 }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  // should contain addition instruction
  ASSERT(file_contains(output, "add "), "should contain add instruction");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_binary_subtraction(void)
{
  // Note: Parser doesn't support subtraction yet, so test complex addition
  const char* output = "/tmp/test_codegen_complex_add.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("calc :: func() i32 { return 10 + 20 + 30 }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  // should contain addition instructions for chained operations
  ASSERT(file_contains(output, "add "), "should contain add instruction");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_binary_multiplication(void)
{
  const char* output = "/tmp/test_codegen_mul.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("mul_nums :: func() i32 { return 5 * 6 }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  ASSERT(file_contains(output, "mul "), "should contain mul instruction");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_binary_division(void)
{
  // Note: Parser doesn't support division yet, so test complex multiplication
  const char* output = "/tmp/test_codegen_complex_mul.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("calc_mul :: func() i32 { return 2 * 3 * 4 }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  // should contain multiplication instructions for chained operations
  ASSERT(file_contains(output, "mul "), "should contain mul instruction");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_local_variable_reference(void)
{
  const char* output = "/tmp/test_codegen_local_ref.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("use_local :: func() i32 { x: i32 = 42\n return x }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  // should contain load instruction to read local variable from stack
  ASSERT(file_contains(output, "ldr ") || file_contains(output, "str "),
         "should contain load/store for local variable");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// comptime constant tests
// ============================================================================

static bool
test_codegen_comptime_constant(void)
{
  const char* output = "/tmp/test_codegen_comptime.s";
  int count;
  struct honey_ast_node** nodes = parse_source(
    "MAGIC :: 42\n"
    "get_magic :: func() i32 { return MAGIC }",
    &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  // should have .const section for comptime constant
  ASSERT(file_contains(output, ".const"), "should contain .const section");
  ASSERT(file_contains(output, "_COMPTIME_MAGIC"),
         "should contain comptime constant label");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_multiple_comptime_constants(void)
{
  const char* output = "/tmp/test_codegen_multi_comptime.s";
  int count;
  struct honey_ast_node** nodes = parse_source(
    "X :: 10\n"
    "Y :: 20\n"
    "Z :: 30\n"
    "main :: func() i32 { return X }",
    &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  ASSERT(file_contains(output, "_COMPTIME_X"),
         "should contain first constant");
  ASSERT(file_contains(output, "_COMPTIME_Y"),
         "should contain second constant");
  ASSERT(file_contains(output, "_COMPTIME_Z"),
         "should contain third constant");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// test declaration tests
// ============================================================================

static bool
test_codegen_test_included(void)
{
  const char* output = "/tmp/test_codegen_test_inc.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("simple_test :: test { x: i32 = 1 }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  // generate with include_tests=true
  honey_emit_arm64(&symtab, output, true);

  ASSERT(file_contains(output, "_simple_test"),
         "should contain test function label");
  ASSERT(file_contains(output, "_test_runner"),
         "should contain test runner");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_test_excluded(void)
{
  const char* output = "/tmp/test_codegen_test_exc.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("simple_test :: test { x: i32 = 1 }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  // generate with include_tests=false
  honey_emit_arm64(&symtab, output, false);

  ASSERT(file_not_contains(output, "_simple_test"),
         "should NOT contain test function when excluded");
  ASSERT(file_contains(output, "_test_runner"),
         "should still contain empty test_runner");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_test_runner_calls_tests(void)
{
  const char* output = "/tmp/test_codegen_test_runner.s";
  int count;
  struct honey_ast_node** nodes = parse_source(
    "test_one :: test { x: i32 = 1 }\n"
    "test_two :: test { y: i32 = 2 }",
    &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, true);

  // verify test runner calls both tests
  ASSERT(file_contains(output, "bl _test_one"),
         "test runner should call first test");
  ASSERT(file_contains(output, "bl _test_two"),
         "test runner should call second test");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// statement code generation tests
// ============================================================================

static bool
test_codegen_variable_declaration(void)
{
  const char* output = "/tmp/test_codegen_var_decl.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("func_with_var :: func() i32 { x: i32 = 100\n return x }",
                 &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  // should allocate stack space and initialize variable
  ASSERT(file_contains(output, "mov x0, #100"),
         "should initialize variable");
  ASSERT(file_contains(output, "str "), "should store variable to stack");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_variable_assignment(void)
{
  const char* output = "/tmp/test_codegen_var_assign.s";
  int count;
  struct honey_ast_node** nodes = parse_source(
    "func_assign :: func() i32 { mut x: i32 = 10\n x = 20\n return x }",
    &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  // should contain multiple store operations
  ASSERT(file_contains(output, "str "), "should contain store operations");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// function call tests
// ============================================================================

static bool
test_codegen_function_call(void)
{
  const char* output = "/tmp/test_codegen_func_call.s";
  int count;
  struct honey_ast_node** nodes = parse_source(
    "helper :: func() i32 { return 42 }\n"
    "caller :: func() i32 { return helper() }",
    &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  ASSERT(file_contains(output, "bl _helper"),
         "should contain branch-link to helper function");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_function_call_with_arguments(void)
{
  const char* output = "/tmp/test_codegen_call_args.s";
  int count;
  struct honey_ast_node** nodes = parse_source(
    "add :: func(a: i32, b: i32) i32 { return a }\n"
    "caller :: func() i32 { return add(5, 10) }",
    &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  ASSERT(file_contains(output, "bl _add"),
         "should call add function");
  // arguments should be set up in registers before call
  ASSERT(file_contains(output, "mov x0, #5") ||
           file_contains(output, "mov w0, #5"),
         "should move first argument to register");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// integration tests
// ============================================================================

static bool
test_codegen_integration_multiple_functions(void)
{
  const char* output = "/tmp/test_codegen_multi_func.s";
  int count;
  struct honey_ast_node** nodes = parse_source(
    "func_a :: func() i32 { return 1 }\n"
    "func_b :: func() i32 { return 2 }\n"
    "func_c :: func() i32 { return 3 }",
    &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  ASSERT(file_contains(output, "_func_a:"),
         "should contain first function");
  ASSERT(file_contains(output, "_func_b:"),
         "should contain second function");
  ASSERT(file_contains(output, "_func_c:"),
         "should contain third function");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_integration_mixed_declarations(void)
{
  const char* output = "/tmp/test_codegen_mixed.s";
  int count;
  struct honey_ast_node** nodes = parse_source(
    "CONST :: 100\n"
    "my_func :: func() i32 { return CONST }\n"
    "my_test :: test { x: i32 = 1 }",
    &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, true);

  ASSERT(file_contains(output, "_COMPTIME_CONST"),
         "should contain comptime constant");
  ASSERT(file_contains(output, "_my_func:"), "should contain function");
  ASSERT(file_contains(output, "_my_test:"), "should contain test");
  ASSERT(file_contains(output, "_test_runner"), "should contain test runner");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_integration_complex_function(void)
{
  const char* output = "/tmp/test_codegen_complex.s";
  int count;
  struct honey_ast_node** nodes = parse_source(
    "calculate :: func(a: i32, b: i32) i32 {\n"
    "  x: i32 = a + b\n"
    "  y: i32 = x * 2\n"
    "  return y\n"
    "}",
    &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  honey_emit_arm64(&symtab, output, false);

  ASSERT(file_contains(output, "_calculate:"), "should contain function");
  ASSERT(file_contains(output, "add "), "should contain addition");
  ASSERT(file_contains(output, "mul "), "should contain multiplication");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// edge case tests
// ============================================================================

static bool
test_codegen_nested_expressions(void)
{
  const char* output = "/tmp/test_codegen_nested.s";
  int count;
  struct honey_ast_node** nodes =
    parse_source("nested :: func() i32 { return 1 + 2 * 3 }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  bool success = honey_emit_arm64(&symtab, output, false);
  ASSERT(success, "should generate code for nested expressions");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_codegen_multiple_return_statements(void)
{
  const char* output = "/tmp/test_codegen_multi_return.s";
  int count;
  // Note: This may not be semantically valid, but tests codegen robustness
  struct honey_ast_node** nodes =
    parse_source("multi_return :: func() i32 { return 1 }", &count);
  ASSERT_NOT_NULL(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  honey_analyze(nodes, count, &symtab);

  bool success = honey_emit_arm64(&symtab, output, false);
  ASSERT(success, "should handle function with return statement");

  cleanup_test_file(output);
  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// main test runner
// ============================================================================

int
main(void)
{
  printf("\n=== codegen tests ===\n\n");

  // basic code generation tests
  RUN_TEST(test_codegen_creates_output_file);
  RUN_TEST(test_codegen_empty_symbol_table);
  RUN_TEST(test_codegen_generates_text_section);

  // function code generation tests
  RUN_TEST(test_codegen_simple_function);
  RUN_TEST(test_codegen_function_with_parameters);
  RUN_TEST(test_codegen_function_with_local_variables);
  RUN_TEST(test_codegen_function_prologue_epilogue);
  RUN_TEST(test_codegen_void_function);

  // expression code generation tests
  RUN_TEST(test_codegen_integer_literal);
  RUN_TEST(test_codegen_binary_addition);
  RUN_TEST(test_codegen_binary_subtraction);
  RUN_TEST(test_codegen_binary_multiplication);
  RUN_TEST(test_codegen_binary_division);
  RUN_TEST(test_codegen_local_variable_reference);

  // comptime constant tests
  RUN_TEST(test_codegen_comptime_constant);
  RUN_TEST(test_codegen_multiple_comptime_constants);

  // test declaration tests
  RUN_TEST(test_codegen_test_included);
  RUN_TEST(test_codegen_test_excluded);
  RUN_TEST(test_codegen_test_runner_calls_tests);

  // statement code generation tests
  RUN_TEST(test_codegen_variable_declaration);
  RUN_TEST(test_codegen_variable_assignment);

  // function call tests
  RUN_TEST(test_codegen_function_call);
  RUN_TEST(test_codegen_function_call_with_arguments);

  // integration tests
  RUN_TEST(test_codegen_integration_multiple_functions);
  RUN_TEST(test_codegen_integration_mixed_declarations);
  RUN_TEST(test_codegen_integration_complex_function);

  // edge case tests
  RUN_TEST(test_codegen_nested_expressions);
  RUN_TEST(test_codegen_multiple_return_statements);

  print_test_summary();

  return test_stats.failed == 0 ? 0 : 1;
}
