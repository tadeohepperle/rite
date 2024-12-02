package rite

import "core:fmt"
import "core:strconv"
import "core:strings"
import "core:unicode/utf8"
// import "core:strings"


Tokens :: struct {
	tokens: [dynamic]Token,
}

Token :: struct {
	ty:   TokenType,
	meta: TokenMetadata,
}

TokenMetadata :: struct #raw_union {
	int:                     i64,
	float:                   f64,
	string:                  string,
	bool:                    bool,
	char:                    rune,
	connected_to_last_token: ConnectedToLastToken, // is true for left parens and bracket if next to last token without whitespace in between: one expression: calc(3+3), these are two expressions: calc (3+3)
}

// true if no whitespace between this and the last token
// only used for '(' and '[' characters to decide if they should bind to the character before (fn call, indexing)
// or if they start a new expression (parents around calculation, list expression).
// This is necessary because the language does not have commas, semicolons or line breaks as seperators!
ConnectedToLastToken :: distinct bool


TokenType :: enum u8 {
	Error,
	StatementSeperator,
	Eof,
	LeftBrace,
	RightBrace,
	LeftBracket,
	RightBracket,
	LeftParen,
	RightParen,
	Dot, // .
	DotDot, // ..
	DotDotDot, //  ...
	Pipe, // |
	Add, // +
	AddAssign, // +=
	Sub, // -
	SubAssign, // -=
	Mul, // *
	MulAssign, // *= 
	Div, // /
	DivAssign, // 
	Colon, // :
	ColonAssign, // :=
	ColonColon, // ::
	Assign, // = 
	Not, // !
	NotEqual, // !=
	Greater, // >
	Less, // <
	GreaterEqual, // >=
	LessEqual, // <=
	Equal, // ==
	And, // &&, and
	Or, // ||, or
	Arrow, // ->
	Is, // is
	In, // in
	If, // if
	For, // for
	Dyn, // dyn
	Else, // else
	Enum, // enum
	Break, // break
	Return, // return
	Switch, // switch
	Case, // switch

	// multi character dynamic tokens:
	Ident, // e.g. hello
	LitBool, // true or false
	LitInt, // e.g. 383
	LitFloat, // e.g. 3.40
	LitString, // "Hello"
	LitChar, // 'Hello'
	LitNone, //
}


Scanner :: struct {
	source:                       string,
	current:                      Char,
	peek:                         Char,
	line:                         int,
	col:                          int,
	white_space_since_last_token: bool,
}

Char :: struct {
	byte: int,
	size: int,
	ch:   rune,
	ty:   CharType,
}
CharType :: enum u8 {
	Letter, // default
	Numeric,
	WhiteSpace,
	WhiteSpaceLineBreak,
	LeftBrace,
	RightBrace,
	LeftBracket,
	RightBracket,
	LeftParen,
	RightParen,
	Dot,
	Pipe,
	Ampersand,
	Minus,
	Plus,
	Asterisk,
	Slash,
	Colon,
	Bang,
	Equal,
	Greater,
	Less,
	DoubleQuote,
	SingleQuote,
}
char_type :: proc(ch: rune) -> CharType {
	if ch <= 1 << 7 - 1 {
		return CHAR_TYPES[u8(ch)]
	} else {
		return .Letter
	}
}
CHAR_TYPES: [256]CharType = {}
init_char_types :: proc() {
	set :: proc(s: string, c: CharType) {
		for ch in s {
			assert(utf8.rune_size(ch) == 1)
			CHAR_TYPES[u8(ch)] = c
		}
	}
	set("0123456789", .Numeric)
	set(",; \t\v", .WhiteSpace)
	set("\n\r", .WhiteSpaceLineBreak)
	set("{", .LeftBrace)
	set("}", .RightBrace)
	set("[", .LeftBracket)
	set("]", .RightBracket)
	set("(", .LeftParen)
	set(")", .RightParen)
	set(")", .RightParen)
	set(".", .Dot)
	set("|", .Pipe)
	set("-", .Minus)
	set("+", .Plus)
	set("*", .Asterisk)
	set("/", .Slash)
	set(":", .Colon)
	set("!", .Bang)
	set("=", .Equal)
	set(">", .Greater)
	set("<", .Less)
	set("\"", .DoubleQuote)
	set("'", .SingleQuote)
}

advance :: proc(using scanner: ^Scanner) {
	current = peek
	peek.byte += peek.size
	peek.ch, peek.size = utf8.decode_rune_in_string(source[peek.byte:])
	peek.ty = char_type(peek.ch)
	if current.ch == '\n' {
		line += 1
		col = 1
	} else {
		col += 1
	}
}

after_peek :: proc(scanner: Scanner) -> (ch: Char) {
	ch.byte = scanner.peek.byte + scanner.peek.size
	ch.ch, ch.size = utf8.decode_rune_in_string(scanner.source[ch.byte:])
	ch.ty = char_type(ch.ch)
	return ch
}

scan_number :: proc(s: ^Scanner) -> Token {
	start_byte := s.current.byte
	for s.peek.ty == .Numeric {
		advance(s)
	}
	is_float := false
	if s.peek.ty == .Dot {
		after_dot := after_peek(s^)
		if after_dot.ty == .Numeric {
			is_float = true
			advance(s) // for the decimal dot
			for s.peek.ty == .Numeric {
				advance(s)
			}
		}
	}
	numeric_string := s.source[start_byte:s.peek.byte]
	token: Token = ---
	if is_float {
		float_value, ok := strconv.parse_f64(numeric_string)
		token = Token {
			ty = .LitFloat,
			meta = {float = float_value},
		}
	} else {
		int_value, ok := strconv.parse_i64_of_base(numeric_string, 10)
		assert(ok)
		token = Token {
			ty = .LitInt,
			meta = {int = int_value},
		}
	}
	return token
}

ident_or_keyword_token :: proc(name: string) -> Token {
	switch name {
	case "if":
		return token(.If)
	case "in":
		return token(.In)
	case "is":
		return token(.Is)
	case "for":
		return token(.For)
	case "dyn":
		return token(.Dyn)
	case "and":
		return token(.And)
	case "or":
		return token(.Or)
	case "else":
		return token(.Else)
	case "enum":
		return token(.Enum)
	case "return":
		return token(.Return)
	case "break":
		return token(.Break)
	case "case":
		return token(.Case)
	case "true":
		return Token{.LitBool, {bool = true}}
	case "false":
		return Token{.LitBool, {bool = false}}
	case "None":
		return Token{.LitNone, {}}
	case "switch":
		return token(.Switch)

	case:
		return Token{.Ident, {string = name}}
	}
}

scan_token :: proc(s: ^Scanner) -> Token {
	#partial switch s.current.ty {
	case .WhiteSpace, .WhiteSpaceLineBreak:
		s.white_space_since_last_token = true
		advance(s)
		// zoom over all whitespace but dont emit a token for it.
		had_line_break_already := false
		for {
			if s.current.ty == .WhiteSpace {
				advance(s)
			} else if s.current.ty == .WhiteSpaceLineBreak {
				advance(s)
				if had_line_break_already { 	// two line breaks cause a break between statements 
					return token(.StatementSeperator)
				}
				had_line_break_already = true
			} else {
				break
			}
		}
		if s.current.byte == s.peek.byte {
			return token(.Eof)
		} else {
			return scan_token(s)
		}
	case .Letter:
		start_byte := s.current.byte
		for (s.peek.ty == .Letter || s.peek.ty == .Numeric) {
			advance(s)
			if s.peek.size == 0 {
				break
			}
		}
		ident_name := s.source[start_byte:s.peek.byte]
		return ident_or_keyword_token(ident_name)
	case .DoubleQuote:
		start_byte := s.current.byte
		for s.peek.ty != .DoubleQuote {
			advance(s)
		}
		string_content := s.source[start_byte + 1:s.peek.byte]
		print("string_content", string_content)
		token := Token{.LitString, {string = string_content}}
		advance(s) // skip over last doublequote
		return token
	case .SingleQuote:
		after_char := after_peek(s^)
		if after_char.ty != .SingleQuote { 	// todo: this does not work for chars like '\n' or '\t', change later to include escaped characters
			return error_token("invalid char literal")
		}
		token_char := s.peek.ch
		advance(s) // skip over char
		advance(s) // skip over ending single quote
		return Token{.LitChar, {char = token_char}}
	case .Numeric:
		return scan_number(s)
	case .Colon:
		if s.peek.ty == .Colon {
			advance(s)
			return token(.ColonColon)
		} else if s.peek.ty == .Equal {
			advance(s)
			return token(.ColonAssign)
		} else {
			return token(.Colon)
		}

	case .Equal:
		if s.peek.ty == .Equal {
			advance(s)
			return token(.Equal)
		} else {
			return token(.Assign)
		}
	case .Dot:
		if s.peek.ty == .Dot {
			advance(s)
			after_second_dot := after_peek(s^)
			if after_second_dot.ty == .Dot {
				advance(s)
				return token(.DotDotDot)
			} else {
				return token(.DotDot)
			}
		} else {
			return token(.Dot)
		}
	case .Minus:
		if s.peek.ty == .Equal {
			advance(s)
			return token(.SubAssign)
		} else if s.peek.ty == .Greater {
			advance(s)
			return token(.Arrow)
		} else {
			return token(.Sub)
		}
	case .Plus:
		if s.peek.ty == .Equal {
			advance(s)
			return token(.AddAssign)
		} else {
			return token(.Add)
		}
	case .Asterisk:
		if s.peek.ty == .Equal {
			advance(s)
			return token(.MulAssign)
		} else {
			return token(.Mul)
		}
	case .Slash:
		if s.peek.ty == .Equal {
			advance(s)
			return token(.DivAssign)
		} else if s.peek.ty == .Slash {
			// Double Slash is a comment. Skip over the comment line
			// skip everything until end of line:
			advance(s)
			for {
				advance(s)
				if s.current.ch == '\n' {
					break
				} else if s.current.byte == s.peek.byte {
					return token(.Eof)
				}
			}
			return scan_token(s)
		} else {
			return token(.Div)
		}
	case .Greater:
		if s.peek.ty == .Equal {
			advance(s)
			return token(.GreaterEqual)
		} else {
			return token(.Greater)
		}
	case .Less:
		if s.peek.ty == .Equal {
			advance(s)
			return token(.LessEqual)
		} else {
			return token(.Less)
		}
	case .Pipe:
		if s.peek.ty == .Pipe {
			advance(s)
			return token(.Or)
		} else {
			return token(.Pipe)
		}
	case .Ampersand:
		if s.peek.ty == .Ampersand {
			return token(.And)
		} else {
			return error_token("Did you mean '&&' ?")
		}
	case .Bang:
		if s.peek.ty == .Equal {
			return token(.NotEqual)
		} else {
			return token(.Not)
		}
	case .LeftBrace:
		return Token{.LeftBrace, {connected_to_last_token = !s.white_space_since_last_token}}
	case .RightBrace:
		return token(.RightBrace)
	case .LeftBracket:
		return Token{.LeftBracket, {connected_to_last_token = !s.white_space_since_last_token}}
	case .RightBracket:
		return token(.RightBracket)
	case .LeftParen:
		return Token{.LeftParen, {connected_to_last_token = !s.white_space_since_last_token}}
	case .RightParen:
		return token(.RightParen)
	}
	return error_token("unexpected")
}

token :: #force_inline proc(ty: TokenType) -> Token {
	return Token{ty = ty, meta = {}}
}
literal :: proc {
	lit_string,
	lit_bool,
	lit_int,
	lit_char,
	lit_float,
}
ident :: proc(name: string) -> Token {
	return Token{ty = .Ident, meta = {string = name}}
}
lit_string :: proc(s: string) -> Token {
	return Token{ty = .LitString, meta = {string = s}}
}
lit_bool :: proc(b: bool) -> Token {
	return Token{ty = .LitBool, meta = {bool = b}}
}
lit_int :: proc(i: int) -> Token {
	return Token{ty = .LitInt, meta = {int = i64(i)}}
}
lit_char :: proc(ch: rune) -> Token {
	return Token{ty = .LitChar, meta = {char = ch}}
}
lit_float :: proc(f: float) -> Token {
	return Token{ty = .LitFloat, meta = {float = f64(f)}}
}


error_token :: #force_inline proc(err: string) -> Token {
	return Token{ty = .Error, meta = {string = err}}
}

tokenize :: proc(source: string) -> (res: [dynamic]Token, err: Maybe(string)) {
	init_char_types()
	s := Scanner {
		source = source,
	}
	advance(&s)
	for {
		if s.peek.size == 0 {break}
		advance(&s)
		token := scan_token(&s)
		s.white_space_since_last_token = false
		if token.ty == .Error {
			return res, token.meta.string
		} else if token.ty == .Eof {
			break
		} else {
			append(&res, token)
		}
	}
	return res, nil

}

tokens_to_string :: proc(tokens: []Token, line_break := false) -> string {
	s: strings.Builder
	for token, i in tokens {
		strings.write_string(&s, tprint(token.ty))
		#partial switch token.ty {
		case .LitChar:
			strings.write_string(&s, tprint("('", token.meta.char, "')"))
		case .LitString:
			strings.write_string(&s, tprint("(\"", token.meta.string, "\")"))
		case .LitBool:
			strings.write_string(&s, tprint("(", token.meta.bool, ")"))
		case .LitFloat:
			strings.write_string(&s, tprint("(", token.meta.float, ")"))
		case .LitInt:
			strings.write_string(&s, tprint("(", token.meta.int, ")"))
		case .Ident:
			strings.write_string(&s, tprint("(", token.meta.string, ")"))
		case:
		}
		if i != len(tokens) - 1 {
			if line_break {
				strings.write_rune(&s, '\n')
			} else {
				strings.write_rune(&s, ' ')
			}
		}
	}
	return strings.to_string(s)
}

// token_could_be_start :: proc(t: TokenType) -> bool {
// 	#partial switch t {
// 	case .Ident, .For, .If, .Eof:
// 		return true
// 	case:
// 		return false
// 	}
// 	return false
// }
// token_could_be_end :: proc(t: TokenType) -> bool {
// 	#partial switch t {
// 	case .LitBool, .LitChar, .LitFloat, .LitString, .LitInt, .LitNone, .Ident:
// 		return true
// 	case:
// 		return false
// 	}
// 	return false
// }
binds_next_token :: proc(t: TokenType) -> bool {
	#partial switch t {
	case .Add,
	     .AddAssign,
	     .Sub,
	     .SubAssign,
	     .Mul,
	     .MulAssign,
	     .Div,
	     .DivAssign,
	     .Assign,
	     .ColonAssign,
	     .Colon,
	     .ColonColon,
	     .Arrow,
	     .If,
	     .Is,
	     .For,
	     .Switch,
	     .Else,
	     .LeftParen,
	     .LeftBrace,
	     .LeftBracket,
	     .And,
	     .Or,
	     .Pipe,
	     .Dot,
	     .DotDot,
	     .Case:
		return true
	}
	return false
}
binds_prev_token :: proc(t: TokenType) -> bool {
	#partial switch t {
	case .Add,
	     .AddAssign,
	     .Sub,
	     .SubAssign,
	     .Mul,
	     .MulAssign,
	     .Div,
	     .DivAssign,
	     .Assign,
	     .ColonAssign,
	     .Colon,
	     .ColonColon,
	     .Arrow,
	     .Is,
	     .Else,
	     .RightParen,
	     .RightBrace,
	     .RightBracket,
	     .LeftParen,
	     .LeftBrace,
	     .LeftBracket,
	     .And,
	     .Or,
	     .Pipe,
	     .Dot:
		return true
	}
	return false
}

scope_level_change :: proc(t: TokenType) -> int {
	#partial switch t {
	case .LeftBrace, .LeftBracket, .LeftParen:
		return 1
	case .RightBrace, .RightBracket, .RightParen:
		return -1

	}
	return 0
}

needs_zero_space_left :: proc(t: TokenType) -> bool {
	#partial switch t {
	case .Dot, .LeftBracket, .RightBracket, .LeftParen, .RightParen, .Colon:
		return true
	}
	return false

}
needs_zero_space_right :: proc(t: TokenType) -> bool {
	#partial switch t {
	case .Dot, .LeftBracket, .RightBracket, .LeftParen, .RightParen:
		return true
	}
	return false
}
tokens_as_code :: proc(tokens: []Token) -> string {
	s: strings.Builder
	last_t := Token{}
	scope_level := 0
	for t, i in tokens {
		scope_level += scope_level_change(t.ty)
		if i != 0 {
			if !binds_next_token(last_t.ty) && !binds_prev_token(t.ty) {
				if scope_level == 0 {
					strings.write_string(&s, "\n")
				} else {
					strings.write_string(&s, ", ")
				}

			} else {

				if needs_zero_space_left(t.ty) || needs_zero_space_right(last_t.ty) {
				} else {
					strings.write_string(&s, " ")
				}
			}
		}

		strings.write_string(&s, token_as_code(t))
		last_t = t
	}
	return strings.to_string(s)
}


token_as_code :: proc(t: Token) -> string {
	switch t.ty {
	case .Error:
		panic("error")
	case .StatementSeperator:
		return "\n\n"
	case .Eof:
		return "EOF"
	case .LeftBrace:
		return "{"
	case .RightBrace:
		return "}"
	case .LeftBracket:
		return "["
	case .RightBracket:
		return "]"
	case .LeftParen:
		return "("
	case .RightParen:
		return ")"
	case .Dot:
		return "."
	case .DotDot:
		return ".."
	case .DotDotDot:
		return "..."
	case .Pipe:
		return "|"
	case .Add:
		return "+"
	case .AddAssign:
		return "+="
	case .Sub:
		return "-"
	case .SubAssign:
		return "-="
	case .Mul:
		return "*"
	case .MulAssign:
		return "*="
	case .Div:
		return "/"
	case .DivAssign:
		return "/="
	case .Colon:
		return ":"
	case .ColonAssign:
		return ":="
	case .ColonColon:
		return "::"
	case .Assign:
		return "="
	case .Not:
		return "!"
	case .NotEqual:
		return "!="
	case .Greater:
		return ">"
	case .Less:
		return "<"
	case .GreaterEqual:
		return ">="
	case .LessEqual:
		return "<="
	case .Equal:
		return "=="
	case .And:
		return "&&"
	case .Or:
		return "||"
	case .Arrow:
		return "->"
	case .Is:
		return "is"
	case .In:
		return "in"
	case .If:
		return "if"
	case .Switch:
		return "switch"
	case .Case:
		return "case"
	case .For:
		return "for"
	case .Dyn:
		return "dyn" // maybe any instead???
	case .Else:
		return "else"
	case .Enum:
		return "enum"
	case .Break:
		return "break"
	case .Return:
		return "return"
	case .Ident:
		return t.meta.string
	case .LitInt:
		return tprint(t.meta.int)
	case .LitFloat:
		return tprint(t.meta.float)
	case .LitBool:
		return "true" if t.meta.bool else "false"
	case .LitString:
		return tprint("\"", t.meta.string, "\"")
	case .LitChar:
		return tprint(t.meta.char)
	case .LitNone:
		return "None"
	}
	unreachable()
}
