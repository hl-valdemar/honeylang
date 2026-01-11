# honey arm64 runtime

.text

# main entry point
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
  # function prologue end

  bl _main  ; call user defined main function

  # function epilogue start
  # function epilogue end

  # exit with the return code from main
  mov x16, #1   ; syscall number for exit
  svc #0        ; trigger syscall

; # test runner entry point
; .global _test_start
; .align 2
; _test_start:
;   # function prologue start
;   # function prologue end
;
;   bl _test_runner  ; call test runner
;
;   # function epilogue start
;   # function epilogue end
;
;   # exit with the return code from test runner
;   mov x16, #1   ; syscall number for exit
;   svc #0        ; trigger syscall
