#ifndef HONEY_TOKEN_H
#define HONEY_TOKEN_H

enum honey_token_kind
{
  HONEY_TOKEN_UNKNOWN,
  HONEY_TOKEN_NAME,
  HONEY_TOKEN_DOUBLE_COLON,
  HONEY_TOKEN_COLON,
  HONEY_TOKEN_INT,
  HONEY_TOKEN_FLOAT,
  HONEY_TOKEN_EOF,

  // keywords
  HONEY_TOKEN_FUNC,
  HONEY_TOKEN_RETURN,

  // delimiters
  HONEY_TOKEN_LBRACE, // {
  HONEY_TOKEN_RBRACE, // }
  HONEY_TOKEN_LPAREN, // (
  HONEY_TOKEN_RPAREN, // )
  HONEY_TOKEN_COMMA,  // ,

  // binary operators
  HONEY_TOKEN_PLUS,
  HONEY_TOKEN_STAR,
};

const char*
honey_token_kind_to_text(enum honey_token_kind kind);

struct honey_token
{
  enum honey_token_kind kind;
  struct
  {
    char* value;
  } data;

  // for better error reporting
  int line;
  int column;
};

#endif
