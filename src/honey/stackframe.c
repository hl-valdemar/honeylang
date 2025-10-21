#include "stackframe.h"
#include "honey/semantic.h"
#include <stdlib.h>
#include <string.h>

// initialize a new stack frame
// note: caller owns memory
struct honey_stackframe*
honey_stackframe_create(void)
{
  // return zero initialized stack frame
  return calloc(1, sizeof(struct honey_stackframe));
}

void
honey_stackframe_destroy(struct honey_stackframe* frame)
{
  for (int i = 0; i < frame->local_count; i += 1) {
    free(frame->locals[i].name);
  }
  free(frame);
}

// add local variable to the stack frame
int
honey_stackframe_add_local(struct honey_stackframe* frame,
                           const char* name,
                           enum honey_type_kind type,
                           bool is_parameter,
                           bool is_mutable)
{
  if (frame->local_count >= HONEY_MAX_LOCAL_VARS) {
    return -1; // error: too many locals (for now)
  }

  int size = honey_type_size(type);

  // align to type size (but at least 4 bytes)
  // NOTE: we should be able to align to smaller types, no?
  int alignment = size < 4 ? 4 : size;

  // move offset down by size
  frame->current_offset -= size;

  // align offset
  frame->current_offset = (frame->current_offset / alignment) * alignment;

  struct honey_local_var* local = &frame->locals[frame->local_count];
  local->name = strdup(name);
  local->type = type;
  local->stack_offset = frame->current_offset;
  local->is_parameter = is_parameter;
  local->is_mutable = is_mutable;

  frame->local_count += 1;

  return local->stack_offset;
}

// lookup a local variable by name
struct honey_local_var*
honey_stackframe_find_local(struct honey_stackframe* frame, const char* name)
{
  for (int i = 0; i < frame->local_count; i += 1) {
    if (strcmp(name, frame->locals[i].name) == 0) {
      return &frame->locals[i];
    }
  }
  return NULL;
}

// calculate total stack space needed for locals
int
honey_stackframe_compute_size(struct honey_stackframe* frame)
{
  // return absolute value of current offset (offset is negative)
  int size = -frame->current_offset;

  // round up to 16 byte alignment
  return (size + 15) & ~15;
}

// calculate space needed for callee-saved registers
int
honey_stackframe_compute_saved_regs_size(struct honey_stackframe* frame)
{
  // each register is 8 bytes, and we save in pairs (16 bytes per pair)
  int count = frame->used_register_count;

  // round up to even number (we save in pairs)
  if (count % 2 != 0) {
    count += 1;
  }

  return count * 8;
}
