#include "arm64.h"
#include "../log.h"
#include "honey/ast.h"
#include "honey/semantic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static bool
emit_test_runner(FILE* f, struct honey_symbol** tests, int count);

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
static void
push_register(FILE* f, const char* reg)
{
  // push reg to stack
  fprintf(f, "    str %s, [sp, #-16]!\n", reg);
}

static void
pop_register(FILE* f, const char* reg)
{
  // pop from stack to reg
  fprintf(f, "    ldr %s, [sp], #16\n", reg);
}

static bool
emit_expression(FILE* f,
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

      // load comptime value from .const section
      fprintf(f, "    adrp x1, _COMPTIME_%s@PAGE\n", sym->name);

      if (sym->type.kind >= TYPE_I8 && sym->type.kind <= TYPE_U64) {
        fprintf(f, "    ldr x0, [x1, _COMPTIME_%s@PAGEOFF]\n", sym->name);
      } else if (sym->type.kind == TYPE_F32) {
        fprintf(f, "    ldr s0, [x1, _COMPTIME_%s@PAGEOFF]\n", sym->name);
      } else if (sym->type.kind == TYPE_F64) {
        fprintf(f, "    ldr d0, [x1, _COMPTIME_%s@PAGEOFF]\n", sym->name);
      }

      return true;
    }

    case AST_BINARY_OP: {
      // evaluate left operand (result in x0)
      if (!emit_expression(f, expr->data.binary_op.left, symtab)) {
        return false;
      }

      // save left result on stack
      push_register(f, "x0");

      // evaluate right operand (result in x0)
      if (!emit_expression(f, expr->data.binary_op.right, symtab)) {
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

    case AST_CALL_EXPR: {
      // simple case: 0-7 arguments - all fit in registers

      // evaluate arguments right-to-left, pushing results to stack
      for (int i = expr->data.call_expr.argument_count - 1; i >= 0; i -= 1) {
        if (!emit_expression(f, expr->data.call_expr.arguments[i], symtab))
          return false;

        // push result
        if (i > 0)
          push_register(f, "x0"); // save result for later args
      }

      // pop arguments into register 0x-7x (in reverse order)
      for (int i = 1; i < expr->data.call_expr.argument_count; i += 1) {
        char reg[8];
        snprintf(reg, sizeof(reg), "x%d", i);
        pop_register(f, reg);
      }
      // x0 already has last evaluated argument

      // call the function
      fprintf(f, "    bl _%s\n", expr->data.call_expr.function_name);

      // result in x0
      return true;
    }

    default:
      honey_error("unsupported expression type in codegen");
      return false;
  }
}

static bool
emit_statement(FILE* f,
               struct honey_ast_node* stmt,
               struct honey_symbol_table* symtab)
{
  switch (stmt->kind) {
    case AST_RETURN_STMT:
      if (stmt->data.return_stmt.value) {
        // evaluate return statement
        if (!emit_expression(f, stmt->data.return_stmt.value, symtab)) {
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
emit_block(FILE* f,
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
        if (!emit_statement(f, deferred->data.defer_stmt.statement, symtab)) {
          return false;
        }
      }

      // now execute the return
      if (!emit_statement(f, stmt, symtab)) {
        return false;
      }
      return true;
    }
  }

  // reached end of block, execute deferred statements
  for (int i = block->data.block.deferred_count - 1; i >= 0; i -= 1) {
    struct honey_ast_node* deferred = block->data.block.deferred[i];
    if (!emit_statement(f, deferred->data.defer_stmt.statement, symtab)) {
      return false;
    }
  }

  return true;
}

static bool
emit_function(FILE* f,
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
  fprintf(f, "    sub sp, sp, #64\n");

  // generate function body
  if (!emit_block(f, func->data.func_decl.body, symtab)) {
    return false;
  }

  // function epilogue (different for entry point "main")
  if (strcmp(sym->name, "main") == 0) {
    fprintf(f, "    add sp, sp, #64\n");
    fprintf(f, "    mov x16, #1\n");
    fprintf(f, "    svc #0\n");
    fprintf(f, "\n");
  } else { // regular function
    fprintf(f, "    add sp, sp, #64\n");
    fprintf(f, "    ret\n");
    fprintf(f, "\n");
  }

  return true;
}

static bool
emit_test(FILE* f,
             struct honey_symbol* sym,
             struct honey_symbol_table* symtab)
{
  struct honey_ast_node* test = sym->test_node;

  // generate as normal function
  fprintf(f, ".global _%s\n", sym->name);
  fprintf(f, ".align 2\n");
  fprintf(f, "_%s:\n", sym->name);
  fprintf(f, "    sub sp, sp, #64\n");

  if (!emit_block(f, test->data.test_decl.body, symtab)) {
    return false;
  }

  fprintf(f, "    mov x0, #0\n");
  fprintf(f, "    add sp, sp, #64\n");
  fprintf(f, "    ret\n\n");

  return true;
}

bool
honey_emit_arm64(struct honey_symbol_table* symtab,
                    const char* output_path,
                    bool include_tests)
{
  FILE* f = fopen(output_path, "w");
  if (!f) {
    honey_error("could not open output file \"%s\"", output_path);
    return false;
  }

  fprintf(f, "# Generated by Honey compiler\n\n");

  // emit comptime constants in .const section
  bool has_comptime_data = false;
  for (int i = 0; i < symtab->count; i += 1) {
    struct honey_symbol* sym = &symtab->symbols[i];
    if (sym->kind == SYMBOL_COMPTIME) {
      if (!has_comptime_data) {
        fprintf(f, ".const\n");
        fprintf(f, ".align 3\n\n");
        has_comptime_data = true;
      }

      fprintf(f, "_COMPTIME_%s:\n", sym->name);

      if (sym->type.kind >= TYPE_I8 && sym->type.kind <= TYPE_U64) {
        fprintf(f, "    .quad %lld\n\n", sym->comptime_value.int_value);
      } else if (sym->type.kind == TYPE_F32 || sym->type.kind == TYPE_F64) {
        // convert f64 to hex for exact encoding
        union
        {
          double d;
          uint64_t u;
        } conv;

        conv.d = sym->comptime_value.float_value;
        fprintf(f, "    .quad 0x%llx\n\n", conv.u);
      }
    }
  }

  fprintf(f, ".text\n\n");

  // collect test symbols
  int test_count = 0;
  struct honey_symbol* tests[HONEY_MAX_SYMBOLS];

  // generate code for all functions and tests
  for (int i = 0; i < symtab->count; i += 1) {
    struct honey_symbol* sym = &symtab->symbols[i];

    if (sym->kind == SYMBOL_FUNCTION) {
      if (!emit_function(f, sym, symtab)) {
        fclose(f);
        return false;
      }
    } else if (sym->kind == SYMBOL_TEST) {
      if (include_tests) {
        tests[test_count] = sym;
        test_count += 1;

        if (!emit_test(f, sym, symtab)) {
          fclose(f);
          return false;
        }
      }
    }
  }

  // generate test runner if in test mode
  if (include_tests) {
    if (!emit_test_runner(f, tests, test_count)) {
      fclose(f);
      return false;
    }
  }

  fclose(f);
  return true;
}

static bool
emit_test_runner(FILE* f, struct honey_symbol** tests, int count)
{
  fprintf(f, ".global _test_runner\n");
  fprintf(f, ".align 2\n");
  fprintf(f, "_test_runner:\n");
  fprintf(f, "    sub sp, sp, #64\n");
  fprintf(f, "    str x30, [sp, #56]\n");

  // call each test
  for (int i = 0; i < count; i += 1) {
    fprintf(f, "\n    # Running test: %s\n", tests[i]->name);
    fprintf(f, "    bl _%s\n", tests[i]->name);

    // TODO: check return value, accumulate failures
  }

  fprintf(f, "\n    mov x0, #0\n");
  fprintf(f, "    ldr x30, [sp, #56]\n");
  fprintf(f, "    add sp, sp, #64\n");
  fprintf(f, "    ret\n\n");

  return true;
}
