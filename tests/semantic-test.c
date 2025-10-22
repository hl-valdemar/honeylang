#include "../src/honey/ast.h"
#include "../src/honey/context.h"
#include "../src/honey/lexer.h"
#include "../src/honey/parser.h"
#include "../src/honey/semantic.h"
#include "../src/honey/test-framework.h"
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

// ============================================================================
// type resolution tests
// ============================================================================

static bool
test_resolve_void_type(void)
{
  enum honey_type_kind kind = honey_resolve_type_name("void");
  assert_eq(kind, HONEY_TYPE_VOID, "void type should resolve correctly");
  return true;
}

static bool
test_resolve_signed_int_types(void)
{
  assert_eq(honey_resolve_type_name("i8"),
            HONEY_TYPE_I8,
            "i8 type should resolve");
  assert_eq(honey_resolve_type_name("i16"),
            HONEY_TYPE_I16,
            "i16 type should resolve");
  assert_eq(honey_resolve_type_name("i32"),
            HONEY_TYPE_I32,
            "i32 type should resolve");
  assert_eq(honey_resolve_type_name("i64"),
            HONEY_TYPE_I64,
            "i64 type should resolve");
  return true;
}

static bool
test_resolve_unsigned_int_types(void)
{
  assert_eq(honey_resolve_type_name("u8"),
            HONEY_TYPE_U8,
            "u8 type should resolve");
  assert_eq(honey_resolve_type_name("u16"),
            HONEY_TYPE_U16,
            "u16 type should resolve");
  assert_eq(honey_resolve_type_name("u32"),
            HONEY_TYPE_U32,
            "u32 type should resolve");
  assert_eq(honey_resolve_type_name("u64"),
            HONEY_TYPE_U64,
            "u64 type should resolve");
  return true;
}

static bool
test_resolve_float_types(void)
{
  assert_eq(honey_resolve_type_name("f32"),
            HONEY_TYPE_F32,
            "f32 type should resolve");
  assert_eq(honey_resolve_type_name("f64"),
            HONEY_TYPE_F64,
            "f64 type should resolve");
  return true;
}

static bool
test_resolve_unknown_type(void)
{
  enum honey_type_kind kind = honey_resolve_type_name("invalid_type");
  assert_eq(kind, HONEY_TYPE_UNKNOWN, "invalid type should return UNKNOWN");
  return true;
}

// ============================================================================
// type to text conversion tests
// ============================================================================

static bool
test_type_kind_to_text(void)
{
  assert_str_eq(honey_type_kind_to_text(HONEY_TYPE_VOID),
                "void",
                "void type text");
  assert_str_eq(honey_type_kind_to_text(HONEY_TYPE_I8), "i8", "i8 type text");
  assert_str_eq(honey_type_kind_to_text(HONEY_TYPE_I32),
                "i32",
                "i32 type text");
  assert_str_eq(honey_type_kind_to_text(HONEY_TYPE_I64),
                "i64",
                "i64 type text");
  assert_str_eq(honey_type_kind_to_text(HONEY_TYPE_U8), "u8", "u8 type text");
  assert_str_eq(honey_type_kind_to_text(HONEY_TYPE_U32),
                "u32",
                "u32 type text");
  assert_str_eq(honey_type_kind_to_text(HONEY_TYPE_F32),
                "f32",
                "f32 type text");
  assert_str_eq(honey_type_kind_to_text(HONEY_TYPE_F64),
                "f64",
                "f64 type text");
  assert_str_eq(honey_type_kind_to_text(HONEY_TYPE_FUNCTION),
                "func",
                "function type text");
  assert_str_eq(honey_type_kind_to_text(HONEY_TYPE_UNKNOWN),
                "unknown",
                "unknown type text");
  return true;
}

// ============================================================================
// type size tests
// ============================================================================

static bool
test_type_size(void)
{
  assert_eq(honey_type_size(HONEY_TYPE_VOID), 0, "void size should be 0");
  assert_eq(honey_type_size(HONEY_TYPE_I8), 1, "i8 size should be 1");
  assert_eq(honey_type_size(HONEY_TYPE_U8), 1, "u8 size should be 1");
  assert_eq(honey_type_size(HONEY_TYPE_I16), 2, "i16 size should be 2");
  assert_eq(honey_type_size(HONEY_TYPE_U16), 2, "u16 size should be 2");
  assert_eq(honey_type_size(HONEY_TYPE_I32), 4, "i32 size should be 4");
  assert_eq(honey_type_size(HONEY_TYPE_U32), 4, "u32 size should be 4");
  assert_eq(honey_type_size(HONEY_TYPE_F32), 4, "f32 size should be 4");
  assert_eq(honey_type_size(HONEY_TYPE_I64), 8, "i64 size should be 8");
  assert_eq(honey_type_size(HONEY_TYPE_U64), 8, "u64 size should be 8");
  assert_eq(honey_type_size(HONEY_TYPE_F64), 8, "f64 size should be 8");
  assert_eq(honey_type_size(HONEY_TYPE_FUNCTION),
            8,
            "function size should be 8 (pointer)");
  return true;
}

// ============================================================================
// comptime declaration analysis tests
// ============================================================================

static bool
test_analyze_comptime_int_with_inferred_type(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("answer :: 42", &count);
  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(success, "semantic analysis should succeed");
  assert_eq(symtab.count, 1, "should have 1 symbol");

  struct honey_symbol* sym = &symtab.symbols[0];
  assert_str_eq(sym->name, "answer", "symbol name should be 'answer'");
  assert_eq(sym->kind, HONEY_SYMBOL_COMPTIME, "should be comptime symbol");
  assert_eq(sym->type.kind, HONEY_TYPE_I64, "inferred type should be i64");
  assert_eq(sym->comptime_value.int_value, 42, "value should be 42");

  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_analyze_comptime_int_with_explicit_type(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("x: i32 :: 100", &count);
  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(success, "semantic analysis should succeed");
  assert_eq(symtab.count, 1, "should have 1 symbol");

  struct honey_symbol* sym = &symtab.symbols[0];
  assert_str_eq(sym->name, "x", "symbol name should be 'x'");
  assert_eq(sym->type.kind, HONEY_TYPE_I32, "explicit type should be i32");
  assert_eq(sym->comptime_value.int_value, 100, "value should be 100");

  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_analyze_comptime_float_with_inferred_type(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("pi :: 3.14", &count);
  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(success, "semantic analysis should succeed");
  assert_eq(symtab.count, 1, "should have 1 symbol");

  struct honey_symbol* sym = &symtab.symbols[0];
  assert_str_eq(sym->name, "pi", "symbol name should be 'pi'");
  assert_eq(sym->kind, HONEY_SYMBOL_COMPTIME, "should be comptime symbol");
  assert_eq(sym->type.kind, HONEY_TYPE_F64, "inferred type should be f64");

  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_analyze_comptime_float_with_explicit_type(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("epsilon: f32 :: 0.001", &count);
  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(success, "semantic analysis should succeed");
  assert_eq(symtab.count, 1, "should have 1 symbol");

  struct honey_symbol* sym = &symtab.symbols[0];
  assert_str_eq(sym->name, "epsilon", "symbol name should be 'epsilon'");
  assert_eq(sym->type.kind, HONEY_TYPE_F32, "explicit type should be f32");

  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_analyze_comptime_with_invalid_type(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("bad: invalid_type :: 42", &count);
  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(!success, "semantic analysis should fail with invalid type");

  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// function declaration analysis tests
// ============================================================================

static bool
test_analyze_function_no_params(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("get_value :: func() i32 { return 42 }", &count);
  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(success, "semantic analysis should succeed");
  assert_eq(symtab.count, 1, "should have 1 symbol");

  struct honey_symbol* sym = &symtab.symbols[0];
  assert_str_eq(sym->name, "get_value", "symbol name should be 'get_value'");
  assert_eq(sym->kind, HONEY_SYMBOL_FUNCTION, "should be function symbol");
  assert_eq(sym->type.kind, HONEY_TYPE_FUNCTION, "type should be function");

  // check return type
  assert_not_null(sym->type.func.return_type, "should have return type");
  assert_eq(sym->type.func.return_type->kind,
            HONEY_TYPE_I32,
            "return type should be i32");

  // check parameters
  assert_eq(sym->type.func.param_count, 0, "should have 0 parameters");
  assert_null(sym->type.func.param_types, "param_types should be NULL");

  // check function node reference
  assert_not_null(sym->func_node, "should have reference to AST node");
  assert_eq(sym->func_node->kind,
            HONEY_AST_FUNC_DECL,
            "referenced node should be function");

  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_analyze_function_with_params(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("add :: func(a: i32, b: i32) i32 { return a + b }", &count);
  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(success, "semantic analysis should succeed");
  assert_eq(symtab.count, 1, "should have 1 symbol");

  struct honey_symbol* sym = &symtab.symbols[0];
  assert_str_eq(sym->name, "add", "symbol name should be 'add'");
  assert_eq(sym->kind, HONEY_SYMBOL_FUNCTION, "should be function symbol");

  // check return type
  assert_eq(sym->type.func.return_type->kind,
            HONEY_TYPE_I32,
            "return type should be i32");

  // check parameters
  assert_eq(sym->type.func.param_count, 2, "should have 2 parameters");
  assert_not_null(sym->type.func.param_types, "param_types should not be NULL");

  assert_eq(sym->type.func.param_types[0]->kind,
            HONEY_TYPE_I32,
            "first param should be i32");
  assert_eq(sym->type.func.param_types[1]->kind,
            HONEY_TYPE_I32,
            "second param should be i32");

  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_analyze_function_with_void_return(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("do_nothing :: func() void { return }", &count);
  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(success, "semantic analysis should succeed");

  struct honey_symbol* sym = &symtab.symbols[0];
  assert_eq(sym->type.func.return_type->kind,
            HONEY_TYPE_VOID,
            "return type should be void");

  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_analyze_function_with_mixed_param_types(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("mixed :: func(a: i32, b: f64, c: u8) void { }", &count);
  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(success, "semantic analysis should succeed");

  struct honey_symbol* sym = &symtab.symbols[0];
  assert_eq(sym->type.func.param_count, 3, "should have 3 parameters");

  assert_eq(sym->type.func.param_types[0]->kind,
            HONEY_TYPE_I32,
            "first param should be i32");
  assert_eq(sym->type.func.param_types[1]->kind,
            HONEY_TYPE_F64,
            "second param should be f64");
  assert_eq(sym->type.func.param_types[2]->kind,
            HONEY_TYPE_U8,
            "third param should be u8");

  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_analyze_function_with_invalid_return_type(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("bad :: func() invalid_type { }", &count);
  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(!success, "semantic analysis should fail with invalid return type");

  cleanup_ast(nodes, count);
  return true;
}

static bool
test_analyze_function_with_invalid_param_type(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("bad :: func(x: invalid_type) i32 { return 0 }", &count);
  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(!success, "semantic analysis should fail with invalid param type");

  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// test declaration analysis tests
// ============================================================================

static bool
test_analyze_test_declaration(void)
{
  int count;
  struct honey_ast_node** nodes =
    parse_source("simple_test :: test { x: i32 = 42 }", &count);
  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(success, "semantic analysis should succeed");
  assert_eq(symtab.count, 1, "should have 1 symbol");

  struct honey_symbol* sym = &symtab.symbols[0];
  assert_str_eq(sym->name,
                "simple_test",
                "symbol name should be 'simple_test'");
  assert_eq(sym->kind, HONEY_SYMBOL_TEST, "should be test symbol");
  assert_eq(sym->type.kind, HONEY_TYPE_FUNCTION, "test type should be function");

  // tests are void -> void functions
  assert_not_null(sym->type.func.return_type, "should have return type");
  assert_eq(sym->type.func.return_type->kind,
            HONEY_TYPE_VOID,
            "return type should be void");
  assert_eq(sym->type.func.param_count, 0, "should have 0 parameters");
  assert_null(sym->type.func.param_types, "param_types should be NULL");

  // check test node reference
  assert_not_null(sym->test_node, "should have reference to AST node");
  assert_eq(sym->test_node->kind,
            HONEY_AST_TEST_DECL,
            "referenced node should be test");

  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// symbol table management tests
// ============================================================================

static bool
test_symbol_table_multiple_declarations(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source(
    "x :: 10\n"
    "y :: 20\n"
    "add :: func(a: i32, b: i32) i32 { return a + b }\n"
    "my_test :: test { z: i32 = 30 }",
    &count);

  assert_not_null(nodes, "parsing should succeed");
  assert_eq(count, 4, "should have 4 declarations");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(success, "semantic analysis should succeed");
  assert_eq(symtab.count, 4, "should have 4 symbols");

  // check first symbol (comptime)
  assert_str_eq(symtab.symbols[0].name, "x", "first symbol should be 'x'");
  assert_eq(symtab.symbols[0].kind,
            HONEY_SYMBOL_COMPTIME,
            "first should be comptime");

  // check second symbol (comptime)
  assert_str_eq(symtab.symbols[1].name, "y", "second symbol should be 'y'");
  assert_eq(symtab.symbols[1].kind,
            HONEY_SYMBOL_COMPTIME,
            "second should be comptime");

  // check third symbol (function)
  assert_str_eq(symtab.symbols[2].name, "add", "third symbol should be 'add'");
  assert_eq(symtab.symbols[2].kind,
            HONEY_SYMBOL_FUNCTION,
            "third should be function");

  // check fourth symbol (test)
  assert_str_eq(symtab.symbols[3].name,
                "my_test",
                "fourth symbol should be 'my_test'");
  assert_eq(symtab.symbols[3].kind, HONEY_SYMBOL_TEST, "fourth should be test");

  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_symbol_table_empty_input(void)
{
  struct honey_symbol_table symtab;
  bool success = honey_analyze(NULL, 0, &symtab);

  assert(!success, "analyzing NULL AST should fail");
  return true;
}

static bool
test_symbol_table_zero_count(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source("x :: 42", &count);
  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, 0, &symtab);

  assert(success, "analyzing 0 declarations should succeed");
  assert_eq(symtab.count, 0, "symbol table should be empty");

  cleanup_ast(nodes, count);
  return true;
}

// ============================================================================
// integration tests
// ============================================================================

static bool
test_integration_all_integer_types(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source(
    "a: i8 :: 1\n"
    "b: i16 :: 2\n"
    "c: i32 :: 3\n"
    "d: i64 :: 4\n"
    "e: u8 :: 5\n"
    "f: u16 :: 6\n"
    "g: u32 :: 7\n"
    "h: u64 :: 8",
    &count);

  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(success, "semantic analysis should succeed");
  assert_eq(symtab.count, 8, "should have 8 symbols");

  // verify types
  assert_eq(symtab.symbols[0].type.kind, HONEY_TYPE_I8, "should be i8");
  assert_eq(symtab.symbols[1].type.kind, HONEY_TYPE_I16, "should be i16");
  assert_eq(symtab.symbols[2].type.kind, HONEY_TYPE_I32, "should be i32");
  assert_eq(symtab.symbols[3].type.kind, HONEY_TYPE_I64, "should be i64");
  assert_eq(symtab.symbols[4].type.kind, HONEY_TYPE_U8, "should be u8");
  assert_eq(symtab.symbols[5].type.kind, HONEY_TYPE_U16, "should be u16");
  assert_eq(symtab.symbols[6].type.kind, HONEY_TYPE_U32, "should be u32");
  assert_eq(symtab.symbols[7].type.kind, HONEY_TYPE_U64, "should be u64");

  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_integration_all_float_types(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source(
    "pi32: f32 :: 3.14\n"
    "pi64: f64 :: 3.14159265",
    &count);

  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(success, "semantic analysis should succeed");
  assert_eq(symtab.count, 2, "should have 2 symbols");

  assert_eq(symtab.symbols[0].type.kind, HONEY_TYPE_F32, "should be f32");
  assert_eq(symtab.symbols[1].type.kind, HONEY_TYPE_F64, "should be f64");

  cleanup_symbol_table(&symtab);
  cleanup_ast(nodes, count);
  return true;
}

static bool
test_integration_complex_function_signature(void)
{
  int count;
  struct honey_ast_node** nodes = parse_source(
    "complex :: func(a: i8, b: i16, c: i32, d: i64, e: u8, f: u16, g: u32, h: "
    "u64, x: f32, y: f64) void { }",
    &count);

  assert_not_null(nodes, "parsing should succeed");

  struct honey_symbol_table symtab;
  bool success = honey_analyze(nodes, count, &symtab);

  assert(success, "semantic analysis should succeed");
  assert_eq(symtab.count, 1, "should have 1 symbol");

  struct honey_symbol* sym = &symtab.symbols[0];
  assert_eq(sym->type.func.param_count, 10, "should have 10 parameters");

  // verify all parameter types
  assert_eq(sym->type.func.param_types[0]->kind, HONEY_TYPE_I8, "param 0: i8");
  assert_eq(sym->type.func.param_types[1]->kind,
            HONEY_TYPE_I16,
            "param 1: i16");
  assert_eq(sym->type.func.param_types[2]->kind,
            HONEY_TYPE_I32,
            "param 2: i32");
  assert_eq(sym->type.func.param_types[3]->kind,
            HONEY_TYPE_I64,
            "param 3: i64");
  assert_eq(sym->type.func.param_types[4]->kind, HONEY_TYPE_U8, "param 4: u8");
  assert_eq(sym->type.func.param_types[5]->kind,
            HONEY_TYPE_U16,
            "param 5: u16");
  assert_eq(sym->type.func.param_types[6]->kind,
            HONEY_TYPE_U32,
            "param 6: u32");
  assert_eq(sym->type.func.param_types[7]->kind,
            HONEY_TYPE_U64,
            "param 7: u64");
  assert_eq(sym->type.func.param_types[8]->kind,
            HONEY_TYPE_F32,
            "param 8: f32");
  assert_eq(sym->type.func.param_types[9]->kind,
            HONEY_TYPE_F64,
            "param 9: f64");

  assert_eq(sym->type.func.return_type->kind,
            HONEY_TYPE_VOID,
            "return type: void");

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
  printf("\n=== semantic analysis tests ===\n\n");

  // type resolution tests
  run_test(test_resolve_void_type);
  run_test(test_resolve_signed_int_types);
  run_test(test_resolve_unsigned_int_types);
  run_test(test_resolve_float_types);
  run_test(test_resolve_unknown_type);

  // type to text conversion tests
  run_test(test_type_kind_to_text);

  // type size tests
  run_test(test_type_size);

  // comptime declaration tests
  run_test(test_analyze_comptime_int_with_inferred_type);
  run_test(test_analyze_comptime_int_with_explicit_type);
  run_test(test_analyze_comptime_float_with_inferred_type);
  run_test(test_analyze_comptime_float_with_explicit_type);
  run_test(test_analyze_comptime_with_invalid_type);

  // function declaration tests
  run_test(test_analyze_function_no_params);
  run_test(test_analyze_function_with_params);
  run_test(test_analyze_function_with_void_return);
  run_test(test_analyze_function_with_mixed_param_types);
  run_test(test_analyze_function_with_invalid_return_type);
  run_test(test_analyze_function_with_invalid_param_type);

  // test declaration tests
  run_test(test_analyze_test_declaration);

  // symbol table management tests
  run_test(test_symbol_table_multiple_declarations);
  run_test(test_symbol_table_empty_input);
  run_test(test_symbol_table_zero_count);

  // integration tests
  run_test(test_integration_all_integer_types);
  run_test(test_integration_all_float_types);
  run_test(test_integration_complex_function_signature);

  print_test_summary();

  return test_stats.failed == 0 ? 0 : 1;
}
