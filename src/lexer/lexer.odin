package lexer

import "../error"
import "../scope"

import "core:unicode"
import "core:unicode/utf8"

LOG_SCOPE :: scope.Scope.lexer

Lexer :: struct {
	src_file:     struct {
		name: string,
		src:  []rune,
	},
	tokens:       [dynamic]Token,
	next_src_idx: int,
	current_line: int,
	current_col:  int,
	errors:       ^error.ErrorList,
}

init :: proc(name, src: string, errors: ^error.ErrorList) -> Lexer {
	return Lexer {
		src_file = {name = name, src = utf8.string_to_runes(src)},
		next_src_idx = 0,
		tokens = make([dynamic]Token, 0, 1000),
		current_line = 1,
		current_col = 1,
		errors = errors,
	}
}

deinit :: proc(lex: ^Lexer) {
	delete(lex.src_file.src)
	delete(lex.tokens)
}

check_keyword :: proc(ident_name: []rune) -> TokenKind {
	name := utf8.runes_to_string(ident_name)
	if name == "true" || name == "false" {
		return .boolean
	}
	return .identifier
}

scan_identifier :: proc(lex: ^Lexer) -> Token {
	loc := error.SourceLocation {
		file   = lex.src_file.name,
		line   = lex.current_line,
		column = lex.current_col,
	}

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

	name := lex.src_file.src[start:end]
	tok := Token {
		kind = check_keyword(name),
		loc  = loc,
	}

	if tok.kind == .identifier || tok.kind == .boolean {
		tok.value = utf8.runes_to_string(name)
	}

	return tok
}

scan_number :: proc(lex: ^Lexer) -> Token {
	loc := error.SourceLocation {
		file   = lex.src_file.name,
		line   = lex.current_line,
		column = lex.current_col,
	}
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

	val := utf8.runes_to_string(lex.src_file.src[start:end])
	return Token{kind = .number, value = val, loc = loc}
}

scan :: proc(lex: ^Lexer) {
	for {
		skip_whitespace_and_comments(lex)

		loc := error.SourceLocation {
			file   = lex.src_file.name,
			line   = lex.current_line,
			column = lex.current_col,
		}

		r, ok := peek(lex).?
		if !ok {
			append(&lex.tokens, Token{kind = .eof, loc = loc})
			break
		}

		if unicode.is_letter(r) || r == '_' { 	// identifiers and keywords
			ident_tok := scan_identifier(lex)
			append(&lex.tokens, ident_tok)
		} else if unicode.is_digit(r) { 	// numbers
			num_tok := scan_number(lex)
			append(&lex.tokens, num_tok)
		} else if r == '-' {
			advance(lex)
			append(&lex.tokens, Token{kind = .minus, loc = loc})
		} else if r == '+' {
			advance(lex)
			append(&lex.tokens, Token{kind = .plus, loc = loc})
		} else if r == '*' {
			advance(lex)
			append(&lex.tokens, Token{kind = .star, loc = loc})
		} else if r == '/' {
			advance(lex)
			append(&lex.tokens, Token{kind = .slash, loc = loc})
		} else if r == ':' {
			// double colon
			if next, ok := peek_offset(lex, 1).?; ok && next == ':' {
				advance_n(lex, 2)
				append(&lex.tokens, Token{kind = .double_colon, loc = loc})
			} else {
				// single colon
				advance(lex)
				append(&lex.tokens, Token{kind = .colon, loc = loc})
			}
		} else {
			// unknown character
			advance(lex)
			error.add_error(
				lex.errors,
				.lexer_unrecognized_char,
				loc,
				"unrecognized character '%r'",
				r,
			)
		}
	}
}
