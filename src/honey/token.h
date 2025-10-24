#ifndef HONEY_TOKEN_H
#define HONEY_TOKEN_H

// define all token kinds in one place
#define HONEY_TOKEN_KINDS                                                      \
  X(HONEY_TOKEN_UNKNOWN)                                                       \
  X(HONEY_TOKEN_EOF)                                                           \
  X(HONEY_TOKEN_MUT)                                                           \
  X(HONEY_TOKEN_NAME)                                                          \
  X(HONEY_TOKEN_BOOL)                                                          \
  X(HONEY_TOKEN_COLON)                                                         \
  X(HONEY_TOKEN_DOUBLE_COLON)                                                  \
  X(HONEY_TOKEN_EQUAL)                                                         \
  X(HONEY_TOKEN_DOUBLE_EQUAL)                                                  \
  X(HONEY_TOKEN_INT)                                                           \
  X(HONEY_TOKEN_FLOAT)                                                         \
  X(HONEY_TOKEN_COMPTIME)                                                      \
  X(HONEY_TOKEN_FUNC)                                                          \
  X(HONEY_TOKEN_RETURN)                                                        \
  X(HONEY_TOKEN_DEFER)                                                         \
  X(HONEY_TOKEN_TEST)                                                          \
  X(HONEY_TOKEN_IF)                                                            \
  X(HONEY_TOKEN_ELSE)                                                          \
  X(HONEY_TOKEN_LBRACE)                                                        \
  X(HONEY_TOKEN_RBRACE)                                                        \
  X(HONEY_TOKEN_LPAREN)                                                        \
  X(HONEY_TOKEN_RPAREN)                                                        \
  X(HONEY_TOKEN_COMMA)                                                         \
  X(HONEY_TOKEN_PLUS)                                                          \
  X(HONEY_TOKEN_MINUS)                                                         \
  X(HONEY_TOKEN_STAR)                                                          \
  X(HONEY_TOKEN_SLASH)                                                         \
  X(HONEY_TOKEN_GREATER)                                                       \
  X(HONEY_TOKEN_LESS)                                                          \
  X(HONEY_TOKEN_GREATER_EQUAL)                                                 \
  X(HONEY_TOKEN_LESS_EQUAL)

enum honey_token_kind
{
#define X(name) name,
  HONEY_TOKEN_KINDS
#undef X
};

const char*
honey_token_kind_to_text(enum honey_token_kind kind);

struct honey_token
{
  enum honey_token_kind kind;
  char* value;

  // for error reporting
  int line;
  int column;
};

#endif
