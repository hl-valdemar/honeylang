#include "arm64.h"
#include "honey/ast.h"
#include "honey/log.h"
#include "honey/semantic.h"
#include "honey/stackframe.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// code generation context
struct codegen_context
{
  FILE* output;
  struct honey_symbol_table* symtab;
  struct honey_stackframe* frame; // current function's stack frame
};

// forward declarations
static bool
emit_test_runner(FILE* f, struct honey_symbol** tests, int count);
static bool
contains_call_expr(struct honey_ast_node* expr);
static bool
emit_expression(struct codegen_context* ctx, struct honey_ast_node* expr);
static bool
emit_statement(struct codegen_context* ctx, struct honey_ast_node* stmt);
static bool
emit_block(struct codegen_context* ctx, struct honey_ast_node* block);

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
  fprintf(f, "    str %s, [sp, #-16]!\n", reg);
}

static void
pop_register(FILE* f, const char* reg)
{
  fprintf(f, "    ldr %s, [sp], #16\n", reg);
}

static bool
emit_expression(struct codegen_context* ctx, struct honey_ast_node* expr)
{
  FILE* f = ctx->output;

  switch (expr->kind) {
    case HONEY_AST_LITERAL_INT:
      fprintf(f, "    mov x0, #%lld\n", expr->data.int_literal);
      return true;

    case HONEY_AST_NAME: {
      // first check if it's a local variable
      if (ctx->frame) {
        struct honey_local_var* local =
          honey_stackframe_find_local(ctx->frame, expr->data.name.identifier);

        if (local) {
          // load from stack frame (relative to frame pointer)
          int size = honey_type_size(local->type);

          if (size == 8) {
            fprintf(f, "    ldr x0, [x29, #%d]\n", local->stack_offset);
          } else if (size == 4) {
            fprintf(f, "    ldr w0, [x29, #%d]\n", local->stack_offset);
          } else if (size == 2) {
            fprintf(f, "    ldrh w0, [x29, #%d]\n", local->stack_offset);
          } else if (size == 1) {
            fprintf(f, "    ldrb w0, [x29, #%d]\n", local->stack_offset);
          }

          return true;
        }
      }

      // not a local, check if it's a comptime constant
      struct honey_symbol* sym =
        find_symbol(ctx->symtab, expr->data.name.identifier);

      if (!sym) {
        honey_error("undefined symbol \"%s\"", expr->data.name.identifier);
        return false;
      }

      if (sym->kind != HONEY_SYMBOL_COMPTIME) {
        honey_error("\"%s\" is not a comptime constant or local variable",
                    expr->data.name.identifier);
        return false;
      }

      fprintf(f, "    adrp x1, _COMPTIME_%s@PAGE\n", sym->name);

      if (sym->type.kind >= HONEY_TYPE_I8 && sym->type.kind <= HONEY_TYPE_U64) {
        fprintf(f, "    ldr x0, [x1, _COMPTIME_%s@PAGEOFF]\n", sym->name);
      } else if (sym->type.kind == HONEY_TYPE_F32) {
        fprintf(f, "    ldr s0, [x1, _COMPTIME_%s@PAGEOFF]\n", sym->name);
      } else if (sym->type.kind == HONEY_TYPE_F64) {
        fprintf(f, "    ldr d0, [x1, _COMPTIME_%s@PAGEOFF]\n", sym->name);
      }

      return true;
    }

    case HONEY_AST_UNARY_OP: {
      if (!emit_expression(ctx, expr->data.unary_op.operand)) {
        return false;
      }
      push_register(f, "x0");

      switch (expr->data.unary_op.op) {
        case HONEY_UNARY_OP_NEG:
          fprintf(f, "    neg x0, x0\n");
          break;
        default:
          honey_error("unsupported unary operation");
          return false;
      }

      return true;
    }

    case HONEY_AST_BINARY_OP: {
      if (!emit_expression(ctx, expr->data.binary_op.left)) {
        return false;
      }
      push_register(f, "x0");

      if (!emit_expression(ctx, expr->data.binary_op.right)) {
        return false;
      }
      pop_register(f, "x1");

      switch (expr->data.binary_op.op) {
        case HONEY_BINARY_OP_ADD:
          fprintf(f, "    add x0, x1, x0\n");
          break;
        case HONEY_BINARY_OP_SUB:
          fprintf(f, "    sub x0, x1, x0\n");
          break;
        case HONEY_BINARY_OP_MUL:
          fprintf(f, "    mul x0, x1, x0\n");
          break;
        case HONEY_BINARY_OP_DIV:
          fprintf(f, "    sdiv x0, x1, x0\n");
          break;
        default:
          honey_error("unsupported binary operation");
          return false;
      }

      return true;
    }

    case HONEY_AST_CALL_EXPR: {
      int arg_count = expr->data.call_expr.argument_count;

      if (arg_count > 8) {
        honey_error("functions with >8 arguments not yet supported");
        return false;
      }

      if (arg_count == 0) {
        fprintf(f, "    bl _%s\n", expr->data.call_expr.function_name);
        return true;
      }

      // evaluate args right-to-left and push to stack
      for (int i = arg_count - 1; i >= 0; i -= 1) {
        if (!emit_expression(ctx, expr->data.call_expr.arguments[i]))
          return false;
        push_register(f, "x0");
      }

      // pop arguments into registers x0-x7
      for (int i = 0; i < arg_count; i += 1) {
        char reg[8];
        snprintf(reg, sizeof(reg), "x%d", i);
        pop_register(f, reg);
      }

      fprintf(f, "    bl _%s\n", expr->data.call_expr.function_name);

      return true;
    }

    default:
      honey_error("unsupported expression type in codegen");
      return false;
  }
}

static bool
emit_statement(struct codegen_context* ctx, struct honey_ast_node* stmt)
{
  FILE* f = ctx->output;

  switch (stmt->kind) {
    case HONEY_AST_VAR_DECL: {
      // add variable to stack frame
      enum honey_type_kind type =
        honey_resolve_type_name(stmt->data.var_decl.type);

      if (type == HONEY_TYPE_UNKNOWN) {
        honey_error("unknown type \"%s\"", stmt->data.var_decl.type);
        return false;
      }

      int offset = honey_stackframe_add_local(ctx->frame,
                                              stmt->data.var_decl.name,
                                              type,
                                              false, // not a parameter
                                              stmt->data.var_decl.is_mutable);

      if (offset == -1) {
        honey_error("too many local variables");
        return false;
      }

      // if there's an initializer, evaluate it and store
      if (stmt->data.var_decl.value) {
        if (!emit_expression(ctx, stmt->data.var_decl.value)) {
          return false;
        }

        // store result to stack
        int size = honey_type_size(type);

        if (size == 8) {
          fprintf(f, "    str x0, [x29, #%d]\n", offset);
        } else if (size == 4) {
          fprintf(f, "    str w0, [x29, #%d]\n", offset);
        } else if (size == 2) {
          fprintf(f, "    strh w0, [x29, #%d]\n", offset);
        } else if (size == 1) {
          fprintf(f, "    strb w0, [x29, #%d]\n", offset);
        }
      }

      return true;
    }

    case HONEY_AST_ASSIGNMENT: {
      // look up the variable
      struct honey_local_var* local =
        honey_stackframe_find_local(ctx->frame, stmt->data.assignment.name);

      if (!local) {
        honey_error("undefined variable \"%s\"", stmt->data.assignment.name);
        return false;
      }

      if (!local->is_mutable) {
        honey_error("cannot assign to immutable variable \"%s\"",
                    stmt->data.assignment.name);
        return false;
      }

      // evaluate new value
      if (!emit_expression(ctx, stmt->data.assignment.value)) {
        return false;
      }

      // store to stack
      int size = honey_type_size(local->type);

      if (size == 8) {
        fprintf(f, "    str x0, [x29, #%d]\n", local->stack_offset);
      } else if (size == 4) {
        fprintf(f, "    str w0, [x29, #%d]\n", local->stack_offset);
      } else if (size == 2) {
        fprintf(f, "    strh w0, [x29, #%d]\n", local->stack_offset);
      } else if (size == 1) {
        fprintf(f, "    strb w0, [x29, #%d]\n", local->stack_offset);
      }

      return true;
    }

    case HONEY_AST_RETURN_STMT:
      if (stmt->data.return_stmt.value) {
        if (!emit_expression(ctx, stmt->data.return_stmt.value)) {
          return false;
        }
      } else {
        fprintf(f, "    mov x0, #0\n");
      }
      return true;

    case HONEY_AST_DEFER_STMT:
      honey_error("defer statement should be handled at block level");
      return false;

    default:
      honey_error("unsupported statement type in codegen");
      return false;
  }
}

static bool
emit_block(struct codegen_context* ctx, struct honey_ast_node* block)
{
  if (block->kind != HONEY_AST_BLOCK) {
    honey_error("expected block node\n");
    return false;
  }

  for (int i = 0; i < block->data.block.statement_count; i += 1) {
    struct honey_ast_node* stmt = block->data.block.statements[i];

    if (stmt->kind == HONEY_AST_RETURN_STMT) {
      // execute deferred statements in reverse order (LIFO)
      for (int j = block->data.block.deferred_count - 1; j >= 0; j -= 1) {
        struct honey_ast_node* deferred = block->data.block.deferred[j];
        if (!emit_statement(ctx, deferred->data.defer_stmt.statement)) {
          return false;
        }
      }

      if (!emit_statement(ctx, stmt)) {
        return false;
      }
      return true;
    }

    if (!emit_statement(ctx, stmt)) {
      return false;
    }
  }

  // reached end of block, execute deferred statements
  for (int i = block->data.block.deferred_count - 1; i >= 0; i -= 1) {
    struct honey_ast_node* deferred = block->data.block.deferred[i];
    if (!emit_statement(ctx, deferred->data.defer_stmt.statement)) {
      return false;
    }
  }

  return true;
}

static bool
contains_call_expr(struct honey_ast_node* expr)
{
  if (!expr)
    return false;

  if (expr->kind == HONEY_AST_CALL_EXPR)
    return true;

  if (expr->kind == HONEY_AST_BINARY_OP) {
    return contains_call_expr(expr->data.binary_op.left) ||
           contains_call_expr(expr->data.binary_op.right);
  }

  return false;
}

// check if function body contains any local variables
static bool
has_local_variables(struct honey_ast_node* body)
{
  if (!body || body->kind != HONEY_AST_BLOCK)
    return false;

  for (int i = 0; i < body->data.block.statement_count; i++) {
    if (body->data.block.statements[i]->kind == HONEY_AST_VAR_DECL) {
      return true;
    }
  }

  return false;
}

static void
get_function_needs(struct honey_ast_node* func,
                   bool* is_leaf,
                   bool* uses_callee_saved,
                   bool* needs_frame)
{
  *is_leaf = true;
  *uses_callee_saved = false;
  *needs_frame = false;

  if (!func->data.func_decl.body)
    return;

  // check for local variables
  if (has_local_variables(func->data.func_decl.body)) {
    *needs_frame = true;
  }

  // check for function calls
  for (int i = 0; i < func->data.func_decl.body->data.block.statement_count;
       i++) {
    struct honey_ast_node* stmt =
      func->data.func_decl.body->data.block.statements[i];

    if (stmt->kind == HONEY_AST_RETURN_STMT && stmt->data.return_stmt.value) {
      if (contains_call_expr(stmt->data.return_stmt.value)) {
        *is_leaf = false;
        *needs_frame = true; // non-leaf always needs frame
        break;
      }
    }
  }
}

static bool
emit_function(FILE* f,
              struct honey_symbol* sym,
              struct honey_symbol_table* symtab)
{
  struct honey_ast_node* func = sym->func.func_node;

  // get function requirements
  bool is_leaf = false;
  bool uses_callee_saved = false;
  bool needs_frame = false;
  get_function_needs(func, &is_leaf, &uses_callee_saved, &needs_frame);

  // create stack frame for this function
  struct honey_stackframe* frame = honey_stackframe_create();

  // add parameters to stack frame
  for (int i = 0; i < func->data.func_decl.param_count; i++) {
    struct honey_ast_node* param = func->data.func_decl.params[i];
    enum honey_type_kind param_type =
      honey_resolve_type_name(param->data.name.type);

    honey_stackframe_add_local(frame,
                               param->data.name.identifier,
                               param_type,
                               true,   // is parameter
                               false); // parameters are immutable by default
  }

  // create codegen context
  struct codegen_context ctx = {
    .output = f,
    .symtab = symtab,
    .frame = frame,
  };

  // first pass: analyze body to build complete frame
  // (traverse and find all var_decl nodes)
  // for now, build the frame as we go during emission

  // calculate stack sizes
  int stack_size = 0;

  if (needs_frame) {
    stack_size += 16; // fp + lr
  }

  // space for expression evaluation
  stack_size += 64;

  // add local variable space after we know how many there are
  // for now, reserve space (calculate actual size during emission)

  // round up to 16-byte alignment
  stack_size = (stack_size + 15) & ~15;

  // === LABEL ===
  fprintf(f, ".global _%s\n", sym->name);
  fprintf(f, ".align 2\n");
  fprintf(f, "_%s:\n", sym->name);

  // === PROLOGUE ===
  fprintf(f, "    ; prologue\n");

  if (needs_frame || !is_leaf) {
    // save fp and lr
    fprintf(f, "    stp x29, x30, [sp, #-16]!\n");
    fprintf(f, "    mov x29, sp\n");

    // allocate remaining space
    if (stack_size > 16) {
      fprintf(f, "    sub sp, sp, #%d\n", stack_size - 16);
    }

    // save parameters from registers to stack
    for (int i = 0; i < func->data.func_decl.param_count && i < 8; i++) {
      struct honey_local_var* param = &frame->locals[i];
      int size = honey_type_size(param->type);

      if (size == 8) {
        fprintf(f, "    str x%d, [x29, #%d]\n", i, param->stack_offset);
      } else if (size == 4) {
        fprintf(f, "    str w%d, [x29, #%d]\n", i, param->stack_offset);
      }
    }
  } else {
    // leaf without frame - minimal prologue
    if (stack_size > 0) {
      fprintf(f, "    sub sp, sp, #%d\n", stack_size);
    }
  }

  fprintf(f, "\n");

  // === BODY ===
  fprintf(f, "    ; body\n");
  if (!emit_block(&ctx, func->data.func_decl.body)) {
    return false;
  }

  fprintf(f, "\n");

  // === EPILOGUE ===
  fprintf(f, "    ; epilogue\n");

  if (needs_frame || !is_leaf) {
    // deallocate stack space
    if (stack_size > 16) {
      fprintf(f, "    add sp, sp, #%d\n", stack_size - 16);
    }

    // restore fp and lr
    fprintf(f, "    ldp x29, x30, [sp], #16\n");
  } else {
    if (stack_size > 0) {
      fprintf(f, "    add sp, sp, #%d\n", stack_size);
    }
  }

  fprintf(f, "    ret\n");
  fprintf(f, "\n");

  honey_stackframe_destroy(frame);

  return true;
}

static bool
emit_test(FILE* f, struct honey_symbol* sym, struct honey_symbol_table* symtab)
{
  struct honey_ast_node* test = sym->test_node;

  // tests are treated as regular functions
  struct honey_stackframe* frame = honey_stackframe_create();

  struct codegen_context ctx = {
    .output = f,
    .symtab = symtab,
    .frame = frame,
  };

  fprintf(f, ".global _%s\n", sym->name);
  fprintf(f, ".align 2\n");
  fprintf(f, "_%s:\n", sym->name);

  fprintf(f, "    ; prologue\n");
  fprintf(f, "    stp x29, x30, [sp, #-16]!\n");
  fprintf(f, "    mov x29, sp\n");
  fprintf(f, "    sub sp, sp, #64\n");
  fprintf(f, "\n");

  fprintf(f, "    ; body\n");
  if (!emit_block(&ctx, test->data.test_decl.body)) {
    return false;
  }
  fprintf(f, "\n");

  fprintf(f, "    ; epilogue\n");
  fprintf(f, "    add sp, sp, #64\n");
  fprintf(f, "    ldp x29, x30, [sp], #16\n");
  fprintf(f, "    ret\n");
  fprintf(f, "\n");

  honey_stackframe_destroy(frame);

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

  fprintf(f, "; Generated by Honey compiler\n");
  fprintf(f, ";\n");
  fprintf(f, "; arm64 calling convention:\n");
  fprintf(f, ";   • arguments: x0-x7 (first 8 args, rest on stack)\n");
  fprintf(f, ";   • return: x0\n");
  fprintf(f, ";   • caller-saved: x0-x18\n");
  fprintf(f, ";   • callee-saved: x19-x28, x29 (fp), x30 (lr)\n");
  fprintf(f, ";   • stack: 16-byte aligned\n\n");

  // emit comptime constants
  bool has_comptime_data = false;
  for (int i = 0; i < symtab->count; i += 1) {
    struct honey_symbol* sym = &symtab->symbols[i];
    if (sym->kind == HONEY_SYMBOL_COMPTIME) {
      if (!has_comptime_data) {
        fprintf(f, ".const\n");
        fprintf(f, ".align 3\n\n");
        has_comptime_data = true;
      }

      fprintf(f, "_COMPTIME_%s:\n", sym->name);

      if (sym->type.kind >= HONEY_TYPE_I8 && sym->type.kind <= HONEY_TYPE_U64) {
        fprintf(f, "    .quad %lld\n\n", sym->comptime_value.int_value);
      } else if (sym->type.kind == HONEY_TYPE_F32 ||
                 sym->type.kind == HONEY_TYPE_F64) {
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

    if (sym->kind == HONEY_SYMBOL_FUNCTION) {
      if (!emit_function(f, sym, symtab)) {
        fclose(f);
        return false;
      }
    } else if (sym->kind == HONEY_SYMBOL_TEST) {
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

  // if applicable, emit test runner
  if (include_tests && !emit_test_runner(f, tests, test_count)) {
    fclose(f);
    return false;
  }
  // otherwise, generate minimal _test_runner label (required by honey runtime)
  else {
    fprintf(f, ".global _test_runner\n");
    fprintf(f, ".align 2\n");
    fprintf(f, "_test_runner:\n");
    fprintf(f, "    ret\n\n");
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
  fprintf(f, "    ; prologue\n");
  fprintf(f, "    stp x29, x30, [sp, #-16]!\n");
  fprintf(f, "    mov x29, sp\n");
  fprintf(f, "    sub sp, sp, #16\n");
  fprintf(f, "\n");

  for (int i = 0; i < count; i += 1) {
    fprintf(f, "    ; test: %s\n", tests[i]->name);
    fprintf(f, "    bl _%s\n", tests[i]->name);
    fprintf(f, "\n");
  }

  fprintf(f, "    mov x0, #0\n");
  fprintf(f, "\n");
  fprintf(f, "    ; epilogue\n");
  fprintf(f, "    add sp, sp, #16\n");
  fprintf(f, "    ldp x29, x30, [sp], #16\n");
  fprintf(f, "    ret\n\n");

  return true;
}
