# honeylang runtime draft

.text

# entry point - should be set by compiler runtime
.global _start
.align 2
_start:
  # stack layout at _start entry:
  # [sp + 0]  = argc (argument count)
  # [sp + 8]  = argv[0] (pointer to program name)
  # [sp + 16] = argv[1] (pointer to first argument)
  # [sp + 24] = argv[2] (pointer to second argument)
  # ...
  # [sp + ?]  = NULL (terminator)

  # function prologue start
  sub sp, sp, #16  ; allocate stack space
  ; doesn't return so don't save link register
  # function prologue end

  bl _main  ; call user defined main function

  # function epilogue start
  add sp, sp, #16  ; deallocate stack space
  # function epilogue end
  
  # exit with the return code from main
  mov x16, #1   ; syscall number for exit
  svc #0        ; trigger syscall

# USER FUNCTIONS BELOW

# main function (mandatory) - user-facing entry point
.global _main
.align 2
_main:
  # function prologue start
  sub sp, sp, #16  ; allocate stack space
  str x30, [sp]    ; save link register
  # function prologue end
  
  # function body start
  bl _get_my_number  ; call get_my_number, result in x0
  # function body end
  
  # function epilogue start
  ldr x30, [sp]    ; restore link register
  add sp, sp, #16  ; deallocate stack space
  # function epilogue end
  
  ret

.global _get_my_number
.align 2
_get_my_number:
  # function prologue start
  ; doesn't use locals or call other functions
  ; hence, doesn't need stack allocation
  # function prologue end

  # function body start
  mov x0, #42  ; return 42
  # function body end

  # function epilogue start
  ; nothing to clean up...
  # function epilogue end

  ret
