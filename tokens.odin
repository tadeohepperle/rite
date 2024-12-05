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
	ty:         TokenType,
	seperation: SeperationToPrevToken,
	data:       PrimitiveData,
}
PrimitiveType :: enum u8 {
	None,
	Bool,
	Int,
	Float,
	String,
	Char,
	Type,
}
primitive_type_to_string :: proc(prim: PrimitiveType) -> string {
	switch prim {
	case .None:
		return "None"
	case .Bool:
		return "bool"
	case .Int:
		return "int"
	case .Float:
		return "float"
	case .String:
		return "string"
	case .Char:
		return "char"
	case .Type:
		return "Type"
	}
	unreachable()
}

// in general, this language does not care about whitespace and commas, 
// but they can give hints to the parser in ambigous situations.
// 1. WhiteSpace:
// whitespace is only required to be omitted for function call, struct literal and indexing 
// 2. Commas: commas force terminate any individual expression, e.g. `i > 3, .Foo` vs. `i > 3.Foo`
SeperationToPrevToken :: enum u8 {
	None       = 0,
	WhiteSpace = 1, // is true for left parens and bracket if next to last token without whitespace in between: one expression: calc(3+3), these are two expressions: calc (3+3)
	Comma      = 2,
}

TokenType :: enum u8 {
	Error,
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
	PrimitiveType, // a few idents that are already builtin, e.g. int, float, string, Type
	LitBool, // true or false
	LitInt, // e.g. 383
	LitFloat, // e.g. 3.40
	LitString, // "Hello"
	LitChar, // 'Hello'
}


Scanner :: struct {
	source:     string,
	current:    Char,
	peek:       Char,
	line:       int,
	col:        int,
	seperation: SeperationToPrevToken,
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
	Comma,
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
	set(" \t\v\n\r", .WhiteSpace)
	set(",;", .Comma)
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
			data = {float = float_value},
		}
	} else {
		int_value, ok := strconv.parse_i64_of_base(numeric_string, 10)
		assert(ok)
		token = Token {
			ty = .LitInt,
			data = {int = int_value},
		}
	}
	return token
}
// Note: later this should be a prefix tree or something. But for now the switch statement is nice and flexible
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
		return lit_bool(true)
	case "false":
		return lit_bool(false)
	case "None":
		return primitive_type(.None)
	case "Type":
		return primitive_type(.Type)
	case "string":
		return primitive_type(.String)
	case "int":
		return primitive_type(.Int)
	case "float":
		return primitive_type(.Float)
	case "char":
		return primitive_type(.Char)
	case "bool":
		return primitive_type(.Bool)
	case "switch":
		return token(.Switch)
	case:
		return Token{.Ident, .None, {string = name}}
	}
}

scan_token :: proc(s: ^Scanner) -> Token {
	#partial switch s.current.ty {
	case .WhiteSpace, .Comma:
		if s.current.ty == .Comma {
			s.seperation = .Comma
		} else {
			s.seperation = .WhiteSpace
		}
		for {
			if s.current.ty == .WhiteSpace {
				advance(s)
			} else if s.current.ty == .Comma {
				advance(s)
				s.seperation = .Comma
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
		token := Token{.LitString, .None, {string = string_content}}
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
		return Token{.LitChar, .None, {char = token_char}}
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
		return token(.LeftBrace)
	case .RightBrace:
		return token(.RightBrace)
	case .LeftBracket:
		return token(.LeftBracket)
	case .RightBracket:
		return token(.RightBracket)
	case .LeftParen:
		return token(.LeftParen)
	case .RightParen:
		return token(.RightParen)
	}
	return error_token("unexpected")
}

token :: #force_inline proc(ty: TokenType) -> Token {
	return Token{ty = ty, data = {}}
}
literal :: proc {
	lit_string,
	lit_bool,
	lit_int,
	lit_char,
	lit_float,
}
ident :: proc(name: string) -> Token {
	return Token{ty = .Ident, data = {string = name}}
}
lit_string :: proc(s: string) -> Token {
	return Token{ty = .LitString, data = {string = s}}
}
lit_bool :: proc(b: bool) -> Token {
	return Token{ty = .LitBool, data = {bool = b}}
}
lit_int :: proc(i: int) -> Token {
	return Token{ty = .LitInt, data = {int = i64(i)}}
}
lit_char :: proc(ch: rune) -> Token {
	return Token{ty = .LitChar, data = {char = ch}}
}
lit_float :: proc(f: float) -> Token {
	return Token{ty = .LitFloat, data = {float = f}}
}
primitive_type :: proc(prim: PrimitiveType) -> Token {
	return Token{ty = .PrimitiveType, data = {primitive_type = prim}}
}

error_token :: #force_inline proc(err: string) -> Token {
	return Token{ty = .Error, data = {string = err}}
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
		if token.ty == .Error {
			return res, token.data.string
		} else if token.ty == .Eof {
			break
		}

		token.seperation, s.seperation = s.seperation, .None
		append(&res, token)
	}
	return res, nil

}

tokens_to_string :: proc(tokens: []Token, line_break := false) -> string {
	s: strings.Builder
	for token, i in tokens {
		strings.write_string(&s, tprint(token.ty))
		#partial switch token.ty {
		case .LitChar:
			strings.write_string(&s, tprint("('", token.data.char, "')"))
		case .LitString:
			strings.write_string(&s, tprint("(\"", token.data.string, "\")"))
		case .LitBool:
			strings.write_string(&s, tprint("(", token.data.bool, ")"))
		case .LitFloat:
			strings.write_string(&s, tprint("(", token.data.float, ")"))
		case .LitInt:
			strings.write_string(&s, tprint("(", token.data.int, ")"))
		case .Ident:
			strings.write_string(&s, tprint("(", token.data.string, ")"))
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
		return t.data.string
	case .LitInt:
		return tprint(t.data.int)
	case .LitFloat:
		return tprint(t.data.float)
	case .LitBool:
		return "true" if t.data.bool else "false"
	case .LitString:
		return tprint("\"", t.data.string, "\"")
	case .LitChar:
		return tprint(t.data.char)
	case .PrimitiveType:
		return primitive_type_to_string(t.data.primitive_type)
	}
	unreachable()
}
