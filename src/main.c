#include "honey/ast.h"
#include "honey/codegen/arm64.h"
#include "honey/context.h"
#include "honey/lexer.h"
#include "honey/log.h"
#include "honey/parser.h"
#include "honey/semantic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char*
read_file(const char* path)
{
  FILE* f = fopen(path, "rb");
  if (!f) {
    honey_error("could not open file: %s", path);
    return NULL;
  }

  // get file size
  fseek(f, 0, SEEK_END);
  long size = ftell(f);
  fseek(f, 0, SEEK_SET);

  // allocate buffer and read
  char* buffer = malloc(size + 1);
  if (!buffer) {
    honey_error("could not allocate memory for file");
    fclose(f);
    return NULL;
  }

  size_t bytes_read = fread(buffer, 1, size, f);
  buffer[bytes_read] = '\0';

  fclose(f);
  return buffer;
}

int
main(int argc, char** argv)
{
  bool test_mode = false;
  const char* source_path = NULL;

  // check for args
  if (argc < 2) {
    honey_error("usage: %s <sauce.hon>", argv[0]);
    return 1;
  }

  // parse args
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--test") == 0) {
      test_mode = true;
    } else {
      source_path = argv[i];
    }
  }

  char* source_code = read_file(source_path);
  if (!source_code) {
    return 1;
  }

  // lexing
  struct honey_context* honey_ctx = honey_context_create();
  honey_scan(honey_ctx, source_code);

  // parsing
  int ast_count = 0;
  struct honey_ast_node** declarations = honey_parse(honey_ctx, &ast_count);
  if (!declarations) {
    honey_error("parsing failed");
    free(source_code);
    honey_context_destroy(honey_ctx);
    return 1;
  }

  // semantic analysis
  struct honey_symbol_table symtab = { 0 };
  if (!honey_analyze(declarations, ast_count, &symtab)) {
    honey_error("semantic analysis failed");
    for (int i = 0; i < ast_count; i += 1) {
      honey_ast_destroy(declarations[i]);
    }
    free(declarations);
    free(source_code);
    honey_context_destroy(honey_ctx);
    return 1;
  }

  // code generation
  const char* asm_path = "output.s";
  if (!honey_codegen_arm64(&symtab, asm_path, test_mode)) {
    honey_error("code generation failed");
    for (int i = 0; i < ast_count; i += 1) {
      honey_ast_destroy(declarations[i]);
    }
    free(declarations);
    free(source_code);
    honey_context_destroy(honey_ctx);
    return 1;
  }

  // assemble and link
  printf("=== Assembling and Linking ===\n");
  if (test_mode) {
    system("as output.s -o output.o");
    system("ld output.o -o honey_test -lSystem -syslibroot `xcrun -sdk macosx "
           "--show-sdk-path` -e _test_runner -arch arm64");
    printf("created executable: honey_test\n\n");
  } else {
    system("as output.s -o output.o");
    system("ld output.o -o honey_prog -lSystem -syslibroot `xcrun -sdk macosx "
           "--show-sdk-path` -e _start -arch arm64");
    printf("created executable: honey_prog\n\n");
  }

  // cleanup
  for (int i = 0; i < ast_count; i += 1) {
    honey_ast_destroy(declarations[i]);
  }
  free(declarations);
  free(source_code);
  honey_context_destroy(honey_ctx);

  return 0;
}
