package lexer

import "../logger"
import "../scope"

import "core:unicode"
import "core:unicode/utf8"

LOG_SCOPE :: scope.Scope.lexer

// = = = = = = = = = = = = = =
// Lexer Implementation
// = = = = = = = = = = = = = =

LexerError :: enum {
	unrecognized_character,
}

Lexer :: struct {
	src:          []rune,
	next_src_idx: int,
	tokens:       [dynamic]Token,
}

init :: proc(src: string) -> Lexer {
	return Lexer {
		src = utf8.string_to_runes(src),
		next_src_idx = 0,
		tokens = make([dynamic]Token, 0, 1000),
	}
}

deinit :: proc(lex: ^Lexer) {
	delete(lex.src)
	delete(lex.tokens)
}

advance :: proc(lex: ^Lexer) {
	lex.next_src_idx += 1
}

peek :: proc(lex: ^Lexer) -> Maybe(rune) {
	if lex.next_src_idx < len(lex.src) {
		return lex.src[lex.next_src_idx]
	}
	return nil
}

peek_offset :: proc(lex: ^Lexer, offset: int) -> Maybe(rune) {
	if lex.next_src_idx + offset < len(lex.src) {
		return lex.src[lex.next_src_idx + offset]
	}
	return nil
}

skip_whitespace_and_comments :: proc(lexer: ^Lexer) {
	for {
		r, ok := peek(lexer).?
		if !ok do break // end of file

		// consume whitespace
		if unicode.is_space(r) {
			advance(lexer)
			continue
		}

		// consume comments
		if r == '#' {
			for {
				advance(lexer)
				r, ok := peek(lexer).?
				if !ok do break // eof in comment
				if r == '\n' do break
			}
			continue
		}

		break
	}
}

check_keyword :: proc(ident_name: []rune) -> TokenKind {
	name := utf8.runes_to_string(ident_name)
	if name == "true" || name == "false" {
		return .boolean
	}
	return .identifier
}

scan_identifier :: proc(lex: ^Lexer) -> Token {
	start := lex.next_src_idx
	end := start
	for {
		r, ok := peek(lex).?
		if !ok do break

		// first rune must be alpha or underscore
		// then can be alphanumeric or underscore
		if start == end {
			if !unicode.is_alpha(r) && r != '_' do break
		} else {
			if !unicode.is_alpha(r) && !unicode.is_number(r) && r != '_' do break
		}

		advance(lex)
		end += 1
	}

	name := lex.src[start:end]
	tok := Token {
		kind = check_keyword(name),
	}

	if tok.kind == .identifier || tok.kind == .boolean {
		tok.value = utf8.runes_to_string(name)
	}

	return tok
}

scan_number :: proc(lex: ^Lexer) -> Token {
	has_decimal := false

	start := lex.next_src_idx
	end := start
	for {
		r, ok := peek(lex).?
		if !ok do break

		if unicode.is_digit(r) {
			advance(lex)
			end += 1
		} else if r == '.' && !has_decimal {
			next, ok := peek_offset(lex, 1).?
			if !ok do break

			// check that next rune is a digit (avoid "10." being treated as float)
			if unicode.is_digit(next) {
				advance(lex)
				has_decimal = true
				end += 1
			} else {
				break
			}
		} else {
			break
		}
	}

	val := utf8.runes_to_string(lex.src[start:end])
	return Token{kind = .number, value = val}
}

scan :: proc(lex: ^Lexer) -> bool {
	for {
		skip_whitespace_and_comments(lex)

		r, ok := peek(lex).?
		if !ok do break

		if unicode.is_letter(r) || r == '_' { 	// identifiers and keywords
			ident_tok := scan_identifier(lex)
			append(&lex.tokens, ident_tok)
		} else if unicode.is_digit(r) { 	// numbers
			num_tok := scan_number(lex)
			append(&lex.tokens, num_tok)
		} else if r == ':' {
			// double colon
			if next, ok := peek_offset(lex, 1).?; ok && next == ':' {
				append(&lex.tokens, Token{kind = .double_colon})
				advance(lex)
				advance(lex)
			} else {
				// single colon
				append(&lex.tokens, Token{kind = .colon})
				advance(lex)
			}
		} else {
			// unknown character
			logger.warn(LOG_SCOPE, "unrecognized character \"%r\"", r)
			return false
		}
	}

	return true
}
