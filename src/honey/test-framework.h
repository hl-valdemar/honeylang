#ifndef HONEY_TEST_FRAMEWORK_H
#define HONEY_TEST_FRAMEWORK_H

#include "honey/log.h"
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

// test statistics
static struct
{
  int total;
  int passed;
  int failed;
} test_stats = { 0, 0, 0 };

// color codes
#define TEST_COLOR_GREEN "\x1b[32m"
#define TEST_COLOR_RED "\x1b[31m"
#define TEST_COLOR_YELLOW "\x1b[33m"
#define TEST_COLOR_RESET "\x1b[0m"

// assert macros
#define assert(expr, msg)                                                      \
  do {                                                                         \
    if (!(expr)) {                                                             \
      printf("[%sASSERT FAILED%s: %s%s%s] ",                                   \
             ANSI_COLOR_RED,                                                   \
             ANSI_COLOR_RESET,                                                 \
             ANSI_COLOR_DARK_WHITE,                                            \
             msg,                                                              \
             ANSI_COLOR_RESET);                                                \
      return false;                                                            \
    }                                                                          \
  } while (0)

#define assert_eq(a, b, msg)                                                   \
  do {                                                                         \
    if ((a) != (b)) {                                                          \
      printf("[%sASSERT FAILED%s: %s%s%s] ",                                   \
             ANSI_COLOR_RED,                                                   \
             ANSI_COLOR_RESET,                                                 \
             ANSI_COLOR_DARK_WHITE,                                            \
             msg,                                                              \
             ANSI_COLOR_RESET);                                                \
      return false;                                                            \
    }                                                                          \
  } while (0)

#define assert_str_eq(a, b, msg)                                               \
  do {                                                                         \
    if (strcmp((a), (b)) != 0) {                                               \
      printf("[%sASSERT FAILED%s: %s%s%s] ",                                   \
             ANSI_COLOR_RED,                                                   \
             ANSI_COLOR_RESET,                                                 \
             ANSI_COLOR_DARK_WHITE,                                            \
             msg,                                                              \
             ANSI_COLOR_RESET);                                                \
      return false;                                                            \
    }                                                                          \
  } while (0)

#define assert_null(ptr, msg) assert((ptr) == NULL, msg)

#define assert_not_null(ptr, msg) assert((ptr) != NULL, msg)

// test runner
#define run_test(test_func)                                                    \
  do {                                                                         \
    printf("running: %s%s%s ... ",                                             \
           TEST_COLOR_YELLOW,                                                  \
           #test_func,                                                         \
           TEST_COLOR_RESET);                                                  \
    test_stats.total++;                                                        \
    if (test_func()) {                                                         \
      printf("%sPASSED%s\n", TEST_COLOR_GREEN, TEST_COLOR_RESET);              \
      test_stats.passed++;                                                     \
    } else {                                                                   \
      printf("%sFAILED%s\n", TEST_COLOR_RED, TEST_COLOR_RESET);                \
      test_stats.failed++;                                                     \
    }                                                                          \
  } while (0)

// print summary
static inline void
print_test_summary(void)
{
  printf("\ntest summary:\n");
  printf("├── total:  %d\n", test_stats.total);
  printf("├── %spassed: %d%s\n",
         TEST_COLOR_GREEN,
         test_stats.passed,
         TEST_COLOR_RESET);
  if (test_stats.failed > 0) {
    printf("└── %sfailed: %d%s\n",
           TEST_COLOR_RED,
           test_stats.failed,
           TEST_COLOR_RESET);
  } else {
    printf("└── failed: %d\n", test_stats.failed);
  }
}

#endif
