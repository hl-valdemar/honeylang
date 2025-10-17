#include "arm64.h"
#include "../log.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct honey_symbol*
find_symbol(struct honey_symbol_table* symtab, const char* name)
{
  for (int i = 0; i < symtab->count; i += 1) {
    if (strcmp(symtab->symbols[i].name, name) == 0) {
      return &symtab->symbols[i];
    }
  }
  return NULL;
}

// simple stack-based approach for expression evaluation
// result always ends up in x0
static void
push_register(FILE* f)
{
  fprintf(f, "    str x0, [sp, #-16]!\n"); // push x0 to stack
}

static void
pop_register(FILE* f, const char* reg)
{
  fprintf(f, "    ldr %s, [sp], #16\n", reg); // pop from stack to reg
}

static bool
codegen_expression(FILE* f,
                   struct honey_ast_node* expr,
                   struct honey_symbol_table* symtab)
{
  switch (expr->kind) {
    case AST_LITERAL_INT:
      // load immediate value into x0 (return register)
      fprintf(f, "    mov x0, #%lld\n", expr->data.int_literal);
      return true;

    case AST_NAME: {
      // look up the constant in the symbol table
      struct honey_symbol* sym =
        find_symbol(symtab, expr->data.name.identifier);
      if (!sym) {
        honey_error("undefined symbol \"%s\"", expr->data.name.identifier);
        return false;
      }

      if (sym->kind != SYMBOL_COMPTIME) {
        honey_error("\"%s\" is not comptime", expr->data.name.identifier);
        return false;
      }

      // load comptime value
      fprintf(f, "    mov x0, #%lld\n", sym->comptime_value.int_value);
      return true;
    }

    case AST_BINARY_OP: {
      // evaluate left operand (result in x0)
      if (!codegen_expression(f, expr->data.binary_op.left, symtab)) {
        return false;
      }

      // save left result on stack
      push_register(f);

      // evaluate right operand (result in x0)
      if (!codegen_expression(f, expr->data.binary_op.right, symtab)) {
        return false;
      }

      // pop left operand into x1
      pop_register(f, "x1");

      // now: x1 = left, x0 = right
      // perform operation and store result in x0
      switch (expr->data.binary_op.op) {
        case BINARY_OP_ADD:
          fprintf(f, "    add x0, x1, x0\n");
          break;
        case BINARY_OP_SUB:
          fprintf(f, "    sub x0, x1, x0\n");
          break;
        case BINARY_OP_MUL:
          fprintf(f, "    mul x0, x1, x0\n");
          break;
        case BINARY_OP_DIV:
          fprintf(f, "    sdiv x0, x1, x0\n"); // signed division
          break;
        default:
          honey_error("unsupported binary operation");
          return false;
      }

      return true;
    }

    default:
      honey_error("unsupported expression type in codegen");
      return false;
  }
}

static bool
codegen_statement(FILE* f,
                  struct honey_ast_node* stmt,
                  struct honey_symbol_table* symtab)
{
  switch (stmt->kind) {
    case AST_RETURN_STMT:
      if (stmt->data.return_stmt.value) {
        // evaluate return statement
        if (!codegen_expression(f, stmt->data.return_stmt.value, symtab)) {
          return false;
        }
      } else {
        // return void (x0 = 0)
        fprintf(f, "    mov x0, #0\n");
      }
      return true;

    case AST_DEFER_STMT:
      // defer statements are handled at block level, not individually
      // this should not be reached during normal code generation
      honey_error("defer statement should be handled at block level");
      return false;

    default:
      honey_error("unsupported statement type in codegen");
      return false;
  }
}

static bool
codegen_block(FILE* f,
              struct honey_ast_node* block,
              struct honey_symbol_table* symtab)
{
  if (block->kind != AST_BLOCK) {
    honey_error("expected block node\n");
    return false;
  }

  // generate code for regular statements
  for (int i = 0; i < block->data.block.statement_count; i += 1) {
    struct honey_ast_node* stmt = block->data.block.statements[i];

    // if this is a return statement, execute deferred statements first
    if (stmt->kind == AST_RETURN_STMT) {
      // execute deferred statements in reverse order (lifo)
      for (int j = block->data.block.deferred_count - 1; j >= 0; j -= 1) {
        struct honey_ast_node* deferred = block->data.block.deferred[j];
        // execute the deferred statement
        if (!codegen_statement(
              f, deferred->data.defer_stmt.statement, symtab)) {
          return false;
        }
      }

      // now execute the return
      if (!codegen_statement(f, stmt, symtab)) {
        return false;
      }
      return true;
    }
  }

  // reached end of block, execute deferred statements
  for (int i = block->data.block.deferred_count - 1; i >= 0; i -= 1) {
    struct honey_ast_node* deferred = block->data.block.deferred[i];
    if (!codegen_statement(f, deferred->data.defer_stmt.statement, symtab)) {
      return false;
    }
  }

  return true;
}

static bool
codegen_function(FILE* f,
                 struct honey_symbol* sym,
                 struct honey_symbol_table* symtab)
{
  struct honey_ast_node* func = sym->func_node;

  // function label (macOS requires _ prefix)
  fprintf(f, ".global _%s\n", sym->name);
  fprintf(f, ".align 2\n");
  fprintf(f, "_%s:\n", sym->name);

  // function prologue - allocate space for stack operations
  // we'll use a simple fixed frame for now
  fprintf(f, "    sub sp, sp, #64\n"); // allocate 64 bytes on stack

  // generate function body
  if (!codegen_block(f, func->data.func_decl.body, symtab)) {
    return false;
  }

  // function epilogue
  fprintf(f, "    add sp, sp, #64\n"); // deallocate stack space
  fprintf(f, "    ret\n");
  fprintf(f, "\n");

  return true;
}

bool
honey_codegen_arm64(struct honey_symbol_table* symtab, const char* output_path)
{
  FILE* f = fopen(output_path, "w");
  if (!f) {
    honey_error("could not open output file \"%s\"", output_path);
    return false;
  }

  // write assembly header
  fprintf(f, "# Generated by Honey compiler\n");
  fprintf(f, ".text\n\n");

  // generate code for all functions
  for (int i = 0; i < symtab->count; i += 1) {
    struct honey_symbol* sym = &symtab->symbols[i];

    if (sym->kind == SYMBOL_FUNCTION) {
      if (!codegen_function(f, sym, symtab)) {
        fclose(f);
        return false;
      }
    }
  }

  fclose(f);
  return true;
}
