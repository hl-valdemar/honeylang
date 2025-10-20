#include "token.h"

const char*
honey_token_kind_to_text(enum honey_token_kind kind)
{
  switch (kind) {
    case HONEY_TOKEN_UNKNOWN:
      return "HONEY_TOKEN_UNKNOWN";
    case HONEY_TOKEN_NAME:
      return "HONEY_TOKEN_NAME";
    case HONEY_TOKEN_DOUBLE_COLON:
      return "HONEY_TOKEN_DOUBLE_COLON";
    case HONEY_TOKEN_COLON:
      return "HONEY_TOKEN_COLON";
    case HONEY_TOKEN_INT:
      return "HONEY_TOKEN_INT";
    case HONEY_TOKEN_FLOAT:
      return "HONEY_TOKEN_FLOAT";
    case HONEY_TOKEN_EOF:
      return "HONEY_TOKEN_EOF";
    case HONEY_TOKEN_MUT:
      return "HONEY_TOKEN_MUT";
    case HONEY_TOKEN_FUNC:
      return "HONEY_TOKEN_FUNC";
    case HONEY_TOKEN_TEST:
      return "HONEY_TOKEN_TEST";
    case HONEY_TOKEN_DEFER:
      return "HONEY_TOKEN_DEFER";
    case HONEY_TOKEN_RETURN:
      return "HONEY_TOKEN_RETURN";
    case HONEY_TOKEN_LBRACE:
      return "HONEY_TOKEN_LBRACE";
    case HONEY_TOKEN_RBRACE:
      return "HONEY_TOKEN_RBRACE";
    case HONEY_TOKEN_LPAREN:
      return "HONEY_TOKEN_LPAREN";
    case HONEY_TOKEN_RPAREN:
      return "HONEY_TOKEN_RPAREN";
    case HONEY_TOKEN_COMMA:
      return "HONEY_TOKEN_COMMA";
    case HONEY_TOKEN_EQUAL:
      return "HONEY_TOKEN_EQUAL";
    case HONEY_TOKEN_DOUBLE_EQUAL:
      return "HONEY_TOKEN_DOUBLE_EQUAL";
    case HONEY_TOKEN_PLUS:
      return "HONEY_TOKEN_PLUS";
    case HONEY_TOKEN_MINUS:
      return "HONEY_TOKEN_MINUS";
    case HONEY_TOKEN_STAR:
      return "HONEY_TOKEN_STAR";
    case HONEY_TOKEN_SLASH:
      return "HONEY_TOKEN_SLASH";
  }
  return "UNKNOWN";
}
