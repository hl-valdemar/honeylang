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
  bool dump_info = false;
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
    } else if (strcmp(argv[i], "--dump-info") == 0) {
      dump_info = true;
    } else {
      source_path = argv[i];
    }
  }

  char* source_code = read_file(source_path);
  if (!source_code) {
    return 1;
  }

  if (dump_info) {
    printf("=== Source Code ===\n");
    printf("%s\n", source_code);
  }

  // lexing
  struct honey_context* honey_ctx = honey_context_create();
  honey_scan(honey_ctx, source_code);

  if (dump_info) {
    printf("=== Lexing ===\n");
    printf("generated %d tokens:\n\n", honey_ctx->next_token_idx);
    for (int j = 0; j < honey_ctx->next_token_idx; j += 1) {
      struct honey_token* tok = &honey_ctx->tokens[j];
      printf("[%d:%d] %s",
             tok->line,
             tok->column,
             honey_token_kind_to_text(tok->kind));

      if (tok->value) {
        printf(" = \"%s\"", tok->value);
      }
      printf("\n");
    }
    printf("\n");
  }

  // parsing
  int ast_count = 0;
  struct honey_ast_node** declarations = honey_parse(honey_ctx, &ast_count);
  if (!declarations) {
    honey_error("parsing failed");
    free(source_code);
    honey_context_destroy(honey_ctx);
    return 1;
  }

  if (dump_info) {
    printf("=== Parsing ===\n");
    printf("parsed %d declarations:\n\n", ast_count);
    for (int i = 0; i < ast_count; i += 1) {
      honey_ast_print(declarations[i], 0);
      printf("\n");
    }
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

  if (dump_info) {
    printf("=== Semantic Analysis ===\n");
    honey_symbol_table_print(&symtab);
    printf("\n");
  }

  // code generation
  const char* asm_path = "output.s";
  if (!honey_emit_arm64(&symtab, asm_path, test_mode)) {
    honey_error("code generation failed");
    for (int i = 0; i < ast_count; i += 1) {
      honey_ast_destroy(declarations[i]);
    }
    free(declarations);
    free(source_code);
    honey_context_destroy(honey_ctx);
    return 1;
  }

  if (dump_info) {
    printf("=== Code Emission ===\n");
    system("cat output.s");
  }

  // assemble generated code
  const char* assemble_cmd = "as output.s -o output.o";
  if (system(assemble_cmd) != 0) {
    honey_error("assembly failed");
    for (int i = 0; i < ast_count; i += 1) {
      honey_ast_destroy(declarations[i]);
    }
    free(declarations);
    free(source_code);
    honey_context_destroy(honey_ctx);
    return 1;
  }

  const char* link_cmd_normal =
    "ld build/runtime/start-darwin-arm64.o output.o -o program "
    "-lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` "
    "-e _start -arch arm64";
  const char* link_cmd_test =
    "ld build/runtime/start-darwin-arm64.o output.o -o test_program "
    "-lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` "
    "-e _test_start -arch arm64";

  // link with runtime for test mode
  if (test_mode) {
    if (system(link_cmd_test) != 0) {
      honey_error("linking failed");
      for (int i = 0; i < ast_count; i += 1) {
        honey_ast_destroy(declarations[i]);
      }
      free(declarations);
      free(source_code);
      honey_context_destroy(honey_ctx);
      return 1;
    }
  }
  // link with runtime for normal mode
  else {
    if (system(link_cmd_normal) != 0) {
      honey_error("linking failed");
      for (int i = 0; i < ast_count; i += 1) {
        honey_ast_destroy(declarations[i]);
      }
      free(declarations);
      free(source_code);
      honey_context_destroy(honey_ctx);
      return 1;
    }
  }

  if (dump_info) {
    printf("=== Assembly and Linking ===\n");

    if (test_mode) {
      printf("created executable: test_program\n\n");

      printf("=== Running Program ===\n");
      system("./test_program; echo \"exit code: $?\"");
    } else {
      printf("created executable: program\n\n");

      printf("=== Running Program ===\n");
      system("./program; echo \"exit code: $?\"");
    }
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
