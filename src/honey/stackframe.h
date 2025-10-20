#ifndef HONEY_STACKFRAME_H
#define HONEY_STACKFRAME_H

#include "semantic.h"
#include <stdbool.h>

// represents a local variable on the stack
struct honey_local_var
{
  char* name;
  enum honey_type_kind type;
  int stack_offset;  // offset from frame pointer (negative)
  bool is_parameter; // true if function parameter
  bool is_mutable;   // true if can be modified
};

#define HONEY_MAX_LOCAL_VARS 256

// stack frame for a single function
struct honey_stackframe
{
  struct honey_local_var locals[HONEY_MAX_LOCAL_VARS];
  int local_count;
  int current_offset; // current stack offset (grows downwards)

  // track which callee-saved registers we use
  bool used_registers[10]; // x19-x28
  int used_register_count;
};

// initialize a new stack frame
struct honey_stackframe*
honey_stackframe_create(void);

// deinitialize stack frame
void
honey_stackframe_destroy(struct honey_stackframe* frame);

// add local variable to the stack frame
int
honey_stackframe_add_local(struct honey_stackframe* frame,
                           const char* name,
                           enum honey_type_kind type,
                           bool is_parameter,
                           bool is_mutable);

// lookup a local variable by name
struct honey_local_var*
honey_stackframe_find_local(struct honey_stackframe* frame, const char* name);

// calculate total stack space needed for locals
int
honey_stackframe_compute_size(struct honey_stackframe* frame);

// calculate space needed for callee-saved registers
int
honey_stackframe_compute_saved_regs_size(struct honey_stackframe* frame);

#endif
