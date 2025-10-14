#include "ast.h"
#include "codegen.h"
#include "honey.h"
#include "log.h"
#include "parser.h"
#include "semantic.h"
#include <stdio.h>
#include <stdlib.h>

const char* test_program = "SOME_VALUE :: 42\n"
                           "\n"
                           "main : func : i32 :: {\n"
                           "  return SOME_VALUE\n"
                           "}\n";

int
main(void)
{
  printf("=== Source Code ===\n");
  printf("%s\n", test_program);

  // lexing
  printf("=== Lexing ===\n");
  struct honey_context* honey_ctx = honey_create_context();
  honey_scan(honey_ctx, test_program);
  printf("generated %d tokens\n\n", honey_ctx->next_token_idx);

  // parsing
  printf("=== Parsing ===\n");
  int ast_count = 0;
  struct honey_ast_node** declarations = honey_parse(honey_ctx, &ast_count);
  if (!declarations) {
    honey_error("parsing failed");
    honey_destroy_context(honey_ctx);
    return 1;
  }

  printf("parsed %d declarations:\n", ast_count);
  for (int i = 0; i < ast_count; i++) {
    honey_ast_print(declarations[i], 0);
  }
  printf("\n");

  // semantic analysis
  printf("=== Semantic Analysis ===\n");
  struct honey_symbol_table symtab = { 0 };
  if (!honey_analyze(declarations, ast_count, &symtab)) {
    honey_error("semantic analysis failed");
    for (int i = 0; i < ast_count; i++) {
      honey_ast_destroy(declarations[i]);
    }
    free(declarations);
    honey_destroy_context(honey_ctx);
    return 1;
  }
  honey_symbol_table_print(&symtab);
  printf("\n");

  // code generation
  printf("=== Code Generation ===\n");
  const char* asm_path = "output.s";
  if (!honey_codegen_arm64(&symtab, asm_path)) {
    honey_error("code generation failed");
    for (int i = 0; i < ast_count; i++) {
      honey_ast_destroy(declarations[i]);
    }
    free(declarations);
    honey_destroy_context(honey_ctx);
    return 1;
  }
  printf("generated arm64 assembly: %s\n\n", asm_path);

  // assemble and link
  printf("=== Assembling and Linking ===\n");
  system("as output.s -o output.o");
  system("ld output.o -o honey_prog -lSystem -syslibroot `xcrun -sdk macosx "
         "--show-sdk-path` -e _main -arch arm64");
  printf("created executable: honey_prog\n\n");

  // run and check result
  printf("=== Running Program ===\n");
  system("./honey_prog; echo \"exit code: $?\"");

  // cleanup
  for (int i = 0; i < ast_count; i++) {
    honey_ast_destroy(declarations[i]);
  }
  free(declarations);
  honey_destroy_context(honey_ctx);

  return 0;
}
