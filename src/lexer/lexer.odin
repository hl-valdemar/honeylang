package lexer

import "../logger"
import "../scope"

import "core:unicode"
import "core:unicode/utf8"

Scope :: scope.Scope

LOG_SCOPE :: Scope.lexer

// = = = = = = = = = = = = = =
// Lexer Implementation
// = = = = = = = = = = = = = =

LexerError :: enum {
	unrecognized_character,
}

Lexer :: struct {
	src:          []rune,
	next_src_idx: int,
}

init :: proc(src: string) -> Lexer {
	return Lexer{src = utf8.string_to_runes(src), next_src_idx = 0}
}

deinit :: proc(lexer: ^Lexer) {
	delete(lexer.src)
}

advance :: proc(lexer: ^Lexer) {
	lexer.next_src_idx += 1
}

peek :: proc(lexer: ^Lexer) -> Maybe(rune) {
	if lexer.next_src_idx < len(lexer.src) {
		return lexer.src[lexer.next_src_idx]
	}
	return nil
}

peek_offset :: proc(lexer: ^Lexer, offset: int) -> Maybe(rune) {
	if lexer.next_src_idx < len(lexer.src) + offset {
		return lexer.src[lexer.next_src_idx + offset]
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
	if utf8.runes_to_string(ident_name) == "true" ||
	   utf8.runes_to_string(ident_name) == "false" {
		return .boolean
	}
	return .identifier
}

scan_identifier :: proc(lexer: ^Lexer) -> Token {
	start := lexer.next_src_idx
	end := start
	for {
		r, ok := peek(lexer).?
		if !ok do break

		// first rune must be alpha or underscore
		// then can be alphanumeric or underscore
		if start == end {
			if !unicode.is_alpha(r) && r != '_' do break
		} else {
			if !unicode.is_alpha(r) && !unicode.is_number(r) && r != '_' do break
		}

		advance(lexer)
		end += 1
	}

	name := lexer.src[start:end]
	tok := Token {
		kind = check_keyword(name),
	}

	if tok.kind == .identifier || tok.kind == .boolean {
		tok.value = name
	}

	return tok
}

scan_number :: proc(lexer: ^Lexer) -> Token {
	has_decimal := false

	start := lexer.next_src_idx
	end := start
	for {
		r, ok := peek(lexer).?
		if !ok do break

		if unicode.is_digit(r) {
			advance(lexer)
			end += 1
		} else if r == '.' && !has_decimal {
			next, ok := peek_offset(lexer, 1).?
			if !ok do break

			// check that next rune is a digit (avoid "10." being treated as float)
			if unicode.is_digit(next) {
				advance(lexer)
				has_decimal = true
				end += 1
			} else {
				break
			}
		} else {
			break
		}
	}

	return Token{kind = .number, value = lexer.src[start:end]}
}

scan :: proc(lexer: ^Lexer) -> ([dynamic]Token, bool) {
	// pre-allocate capacity for tokens (reasonable assumption of 1000)
	tokens := make([dynamic]Token, 0, 1000)

	for {
		skip_whitespace_and_comments(lexer)

		r, ok := peek(lexer).?
		if !ok do break

		if unicode.is_letter(r) || r == '_' { 	// identifiers and keywords
			ident_tok := scan_identifier(lexer)
			append(&tokens, ident_tok)
		} else if unicode.is_digit(r) { 	// numbers
			num_tok := scan_number(lexer)
			append(&tokens, num_tok)
		} else if r == ':' {
			// double colon
			if next, ok := peek_offset(lexer, 1).?; ok && next == ':' {
				append(&tokens, Token{kind = .double_colon})
				advance(lexer)
				advance(lexer)
			} else {
				// single colon
				append(&tokens, Token{kind = .colon})
				advance(lexer)
			}
		} else {
			// unknown character
			logger.warn(LOG_SCOPE, "unrecognized character \"%r\"", r)
			return {}, false
		}
	}

	return tokens, true
}
