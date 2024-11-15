package rite

Parser :: struct {
	tokens:  []Token,
	current: int,
}

current :: proc(p: ^Parser) -> Token {
	return p.tokens[p.current]
}

finished :: proc(p: ^Parser) -> bool {
	return p.current >= len(p.tokens)
}

next :: proc(p: ^Parser) -> Token {
	tok := p.tokens[p.current]
	p.current += 1
	return tok
}

// skips one token, useful if you already know what this token is.
skip :: #force_inline proc(p: ^Parser) {
	p.current += 1
}

peek :: proc(p: ^Parser) -> Token {
	idx := p.current + 1
	if idx >= len(p.tokens) {
		return error_token("out of bounds")
	}
	return p.tokens[idx]
}

accept_token :: proc(p: ^Parser, ty: TokenType) -> bool {
	if p.current >= len(p.tokens) {
		return false
	}
	is_ty := p.tokens[p.current].ty == ty
	if is_ty {
		p.current += 1
	}
	return is_ty
}


/*


B :: enum ( ::


LIST :: [

A :: [3 3 4]

A.filter()
 .see()
 .map(_ * 4)

*/


expect_statements :: proc(p: ^Parser) -> []Statement {
	return {}
}

expect_expressions :: proc(p: ^Parser, until: TokenType) -> []Expression {
	res: [dynamic]Expression
	_expect_expressions(p, until, &res)
	return res[:]
}

_expect_expressions :: #force_inline proc(
	p: ^Parser,
	until: TokenType,
	res: ^[dynamic]Expression,
) {
	for {
		cur := current(p).ty
		if cur == until || cur == .Eof {
			break
		}
		append(res, expect_expression(p))
	}
}

expect_expression :: proc(p: ^Parser) -> Expression {
	return expect_single_value(p)

	expect_single_value :: proc(p: ^Parser) -> Expression {
		if finished(p) {
			return invalid_expression(p.current, p.current, "no tokens left")
		} else if .CouldBeExpressionStart not_in TOKEN_TYPE_FLAGS[current(p).ty] {
			skip(p)
			return invalid_expression(p.current, p.current + 1)
		}
		tok := next(p)
		#partial switch tok.ty {
		case .LitBool:
			return expression(LitBool{tok.meta.bool}, p.current - 1, p.current)
		case .LitInt:
			return expression(LitInt{tok.meta.int}, p.current - 1, p.current)
		case .LitFloat:
			return expression(LitFloat{tok.meta.float}, p.current - 1, p.current)
		case .LitChar:
			return expression(LitChar{tok.meta.char}, p.current - 1, p.current)
		case .LitString:
			return expression(LitString{tok.meta.string}, p.current - 1, p.current)
		case .LitNone:
			return expression(LitNone{}, p.current - 1, p.current)
		case .Ident:
			return expression(Ident{tok.meta.string}, p.current - 1, p.current)
		case .Enum:
			todo()
		case .LeftParen:
			todo()
		case .LeftBracket:
			// [3,4,5]
			// could also be type e.g. [int] or [{s: int, f: float}] or [enum{Red, Black}]
			start := p.current
			values := expect_expressions(p, .RightBracket)
			accept_token(p, .RightBracket)
			return expression(LitArray{values}, start, p.current)
		case .LeftBrace:
			todo()
		}
		panic(
			"no token matched start of expression, this case should have been handled in .CouldBeExpressionStart not_in TOKEN_TYPE_FLAGS[current(p).ty]",
		)
	}
}

parse :: proc(tokens: []Token) -> Module {
	parser := Parser {
		tokens  = tokens,
		current = 0,
	}
	statements := expect_statements(&parser)
	return Module{statements}
}


parse_expression :: proc(tokens: []Token) -> Expression {
	parser := Parser {
		tokens  = tokens,
		current = 0,
	}
	return expect_expression(&parser)
}
