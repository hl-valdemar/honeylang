#ifndef HONEY_CODEGEN_H
#define HONEY_CODEGEN_H

#include "../semantic.h"
#include <stdbool.h>

// generate arm64 assembly code
bool
honey_emit_arm64(struct honey_symbol_table* symtab, const char* output_path, bool include_tests);

#endif
