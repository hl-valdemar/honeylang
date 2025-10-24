#include "honey/token.h"

const char*
honey_token_kind_to_text(enum honey_token_kind kind)
{
  switch (kind) {
#define X(name)                                                                \
  case name:                                                                   \
    return #name;
    HONEY_TOKEN_KINDS
#undef X
  }
  return "UNKNOWN";
}
