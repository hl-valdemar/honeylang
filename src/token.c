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
    case HONEY_TOKEN_INT:
      return "HONEY_TOKEN_INT";
    case HONEY_TOKEN_FLOAT:
      return "HONEY_TOKEN_FLOAT";
  }
}

