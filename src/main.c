#include "ast.h"
#include "codegen.h"
#include "honey.h"
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

  // Step 1: Lexing
  printf("=== Lexing ===\n");
  struct honey_context* honey_ctx = honey_create_context();
  honey_scan(honey_ctx, test_program);
  printf("Generated %d tokens\n\n", honey_ctx->next_token_idx);

  // Step 2: Parsing
  printf("=== Parsing ===\n");
  struct honey_ast_node* ast = honey_parse(honey_ctx);
  if (!ast) {
    fprintf(stderr, "Parsing failed\n");
    honey_destroy_context(honey_ctx);
    return 1;
  }
  honey_ast_print(ast, 0);
  printf("\n");

  // Step 3: Semantic Analysis
  printf("=== Semantic Analysis ===\n");
  struct honey_symbol_table symtab = { 0 };
  if (!honey_analyze(ast, &symtab)) {
    fprintf(stderr, "Semantic analysis failed\n");
    honey_ast_destroy(ast);
    honey_destroy_context(honey_ctx);
    return 1;
  }
  honey_symbol_table_print(&symtab);
  printf("\n");

  // Step 4: Code Generation
  printf("=== Code Generation ===\n");
  const char* asm_path = "output.s";
  if (!honey_codegen_arm64(&symtab, asm_path)) {
    fprintf(stderr, "Code generation failed\n");
    honey_ast_destroy(ast);
    honey_destroy_context(honey_ctx);
    return 1;
  }
  printf("Generated ARM64 assembly: %s\n\n", asm_path);

  // Step 5: Assemble and Link
  printf("=== Assembling and Linking ===\n");
  system("as output.s -o output.o");
  system("ld output.o -o honey_prog -lSystem -syslibroot `xcrun -sdk macosx "
         "--show-sdk-path` -e _main -arch arm64");
  printf("Created executable: honey_prog\n\n");

  // Step 6: Run and check result
  printf("=== Running Program ===\n");
  int result = system("./honey_prog; echo \"Exit code: $?\"");

  // Cleanup
  honey_ast_destroy(ast);
  honey_destroy_context(honey_ctx);

  return 0;
}
