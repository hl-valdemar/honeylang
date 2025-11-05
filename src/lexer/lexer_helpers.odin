package lexer

import "core:fmt"
import "core:unicode"

advance :: proc(lex: ^Lexer) {
	lex.next_src_idx += 1
	lex.current_col += 1
}

advance_n :: proc(lex: ^Lexer, n: int) {
	lex.next_src_idx += n
	lex.current_col += n
}

peek :: proc(lex: ^Lexer) -> Maybe(rune) {
	if lex.next_src_idx < len(lex.src_file.src) {
		return lex.src_file.src[lex.next_src_idx]
	}
	return nil
}

peek_offset :: proc(lex: ^Lexer, offset: int) -> Maybe(rune) {
	if lex.next_src_idx + offset < len(lex.src_file.src) {
		return lex.src_file.src[lex.next_src_idx + offset]
	}
	return nil
}

skip_whitespace_and_comments :: proc(lexer: ^Lexer) {
	for {
		r, ok := peek(lexer).?
		if !ok do break // end of file

		// consume whitespace
		if unicode.is_space(r) {
			if r == '\n' {
				lexer.current_line += 1
				lexer.current_col = 0
			}
			advance(lexer)
			continue
		}

		// consume comments
		if r == '#' {
			for {
				advance(lexer)
				r, ok := peek(lexer).?
				if !ok do break // eof in comment
				if r == '\n' {
					lexer.current_line += 1
					lexer.current_col = 0
					break
				}
			}
			continue
		}

		break
	}
}

print_tokens :: proc(tokens: ^[dynamic]Token) {
	for tok in tokens do fmt.println(token_to_string(tok))
}
