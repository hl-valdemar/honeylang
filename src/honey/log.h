#ifndef HONEY_LOG_H
#define HONEY_LOG_H

#include <stdbool.h>
#include <stdlib.h>

// ansi color codes
#define ANSI_COLOR_RESET "\x1b[0m"
#define ANSI_COLOR_RED "\x1b[31m"
#define ANSI_COLOR_YELLOW "\x1b[33m"
#define ANSI_COLOR_BRIGHT_YELLOW "\x1b[93m"
#define ANSI_COLOR_CYAN "\x1b[36m"
#define ANSI_COLOR_GREEN "\x1b[32m"
#define ANSI_COLOR_GRAY "\x1b[90m"
#define ANSI_COLOR_DARK_WHITE "\x1b[37m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_BRIGHT_RED "\x1b[91m"

// enable or disable log levels
#define HONEY_LOG_WARN_ENABLED true
#define HONEY_LOG_INFO_ENABLED true

#if RELEASE
#define HONEY_LOG_DEBUG_ENABLED false
#define HONEY_LOG_TRACE_ENABLED false
#else
#define HONEY_LOG_DEBUG_ENABLED true
#define HONEY_LOG_TRACE_ENABLED true
#endif

enum honey_log_level
{
  HONEY_LOG_LEVEL_FATAL = 0,
  HONEY_LOG_LEVEL_ERROR = 1,
  HONEY_LOG_LEVEL_WARN = 2,
  HONEY_LOG_LEVEL_INFO = 3,
  HONEY_LOG_LEVEL_DEBUG = 4,
  HONEY_LOG_LEVEL_TRACE = 5,
};

bool
honey_log_init();
void
honey_log_shutdown();

void
honey_log(enum honey_log_level level, const char* msg, ...);

// logging utility macros

#if LOG_DISABLED
#define honey_fatal(msg, ...)
#define honey_error(msg, ...)
#define honey_warn(msg, ...)
#define honey_info(msg, ...)
#define honey_debug(msg, ...)
#define honey_trace(msg, ...)
#else

#define honey_fatal(msg, ...)                                                  \
  honey_log(HONEY_LOG_LEVEL_FATAL, msg, ##__VA_ARGS__)
#define honey_error(msg, ...)                                                  \
  honey_log(HONEY_LOG_LEVEL_ERROR, msg, ##__VA_ARGS__)

#if HONEY_LOG_WARN_ENABLED
#define honey_warn(msg, ...) honey_log(HONEY_LOG_LEVEL_WARN, msg, ##__VA_ARGS__)
#else
#define honey_warn(msg, ...)
#endif

#if HONEY_LOG_INFO_ENABLED
#define honey_info(msg, ...) honey_log(HONEY_LOG_LEVEL_INFO, msg, ##__VA_ARGS__)
#else
#define honey_info(msg, ...)
#endif

#if HONEY_LOG_DEBUG_ENABLED
#define honey_debug(msg, ...)                                                  \
  honey_log(HONEY_LOG_LEVEL_DEBUG, msg, ##__VA_ARGS__)
#else
#define honey_debug(msg, ...)
#endif

#if HONEY_LOG_TRACE_ENABLED
#define honey_trace(msg, ...)                                                  \
  honey_log(HONEY_LOG_LEVEL_TRACE, msg, ##__VA_ARGS__)
#else
#define honey_trace(msg, ...)
#endif

#endif

void
honey_log_assertion_failure(const char* expr,
                            const char* msg,
                            const char* file,
                            int line);

#define honey_assert(expr, msg)                                                \
  {                                                                            \
    if (expr) {                                                                \
    } else {                                                                   \
      honey_log_assertion_failure(#expr, msg, __FILE__, __LINE__);             \
      abort();                                                                 \
    }                                                                          \
  }

#endif
