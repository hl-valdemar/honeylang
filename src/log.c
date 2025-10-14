#include "log.h"
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

bool
honey_log_init()
{
  // NOTE: for eventual file logging setup
  return true;
}
void
honey_log_shutdown()
{
  // NOTE: for eventual file logging setup
}

void
honey_log(enum honey_log_level level, const char* msg, ...)
{
  const char* level_names[6] = { "fatal", "error", "warn",
                                 "info",  "debug", "trace" };

  const char* level_colors[6] = {
    ANSI_COLOR_RED,    // fatal
    ANSI_COLOR_RED,    // error
    ANSI_COLOR_YELLOW, // warn
    ANSI_COLOR_GREEN,  // info
    ANSI_COLOR_CYAN,   // debug
    ANSI_COLOR_MAGENTA // trace
  };

  FILE* out = (level == LOG_LEVEL_WARN) ? stdout : stderr;

  fprintf(out,
          "[%s%s%s] %s(pine)%s: ",
          level_colors[level],
          level_names[level],
          ANSI_COLOR_RESET,
          ANSI_COLOR_DARK_WHITE,
          ANSI_COLOR_RESET);

  va_list arg_ptr;
  va_start(arg_ptr, msg);
  vfprintf(out, msg, arg_ptr);
  va_end(arg_ptr);

  fprintf(out, "\n");
  fflush(out); // make sure logs appear immediately (in case of buffering)
}

void
honey_log_assertion_failure(const char* expr,
                            const char* msg,
                            const char* file,
                            int line)
{
  honey_log(LOG_LEVEL_FATAL,
            "assertion failure: %s, message: '%s', in file: %s, on line: %d",
            expr,
            msg,
            file,
            line);
}
