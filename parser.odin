package rite

import "core:log"

Parser :: struct {
	tokens:  []Token,
	current: int,
	errors:  [dynamic]ParserError,
}
ParserError :: struct {
	msg:   string,
	range: TokenRange,
}

add_error :: proc(p: ^Parser, range: TokenRange, msg: string = "error") {
	append(&p.errors, ParserError{msg, range})
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

// difference between expect and accept: expect advances the curser even if it fails.
expect_token :: proc(p: ^Parser, ty: TokenType) -> bool {
	tok := p.tokens[p.current]
	if tok.ty == ty {
		p.current += 1
		return true
	} else {
		return false
	}
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

expect_ident :: proc(p: ^Parser) -> (Ident, bool) {
	tok := next(p)
	if tok.ty == .Ident {
		return Ident{tok.meta.string}, true
	} else {
		return {}, false
	}
}
accept_ident :: proc(p: ^Parser) -> (Ident, bool) {
	if p.current >= len(p.tokens) {
		return {}, false
	}
	cur := p.tokens[p.current]
	if cur.ty == .Ident {
		p.current += 1
		return Ident{cur.meta.string}, true
	}
	return {}, false
}

accept_left_paren_connected_to_last_token :: proc(p: ^Parser) -> bool {
	cur := p.tokens[p.current]
	accepted := cur.ty == .LeftParen && bool(cur.meta.connected_to_last_token)
	if accepted {
		log.info("Accepted left paren connected to last token")
		p.current += 1
	}
	return accepted
}
accept_left_bracket_connected_to_last_token :: proc(p: ^Parser) -> bool {
	cur := p.tokens[p.current]
	accepted := cur.ty == .LeftBracket && bool(cur.meta.connected_to_last_token)
	if accepted {
		log.info("Accepted left bracket connected to last token")
		p.current += 1
	}
	return accepted
}
accept_left_brace_connected_to_last_token :: proc(p: ^Parser) -> bool {
	cur := p.tokens[p.current]
	accepted := cur.ty == .LeftBrace && bool(cur.meta.connected_to_last_token)
	if accepted {
		log.info("Accepted left brace connected to last token")
		p.current += 1
	}
	return accepted
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
	return expect_logical_or_or_higher(p)

	expect_logical_or_or_higher :: proc(p: ^Parser) -> (expr: Expression) {
		expr = expect_logical_and_or_higher(p)
		if expression_valid(expr) && current(p).ty == .Or {
			second := expect_logical_or_or_higher(p)
			return expression(
				LogicalOr{new_clone(expr), new_clone(second)},
				expr.range.start_idx,
				second.range.end_idx,
			)
		} else {
			return expr
		}
	}

	expect_logical_and_or_higher :: proc(p: ^Parser) -> (expr: Expression) {
		expr = expect_comparison_or_higher(p)
		if expression_valid(expr) && current(p).ty == .And {
			second := expect_logical_and_or_higher(p)
			return expression(
				LogicalOr{new_clone(expr), new_clone(second)},
				expr.range.start_idx,
				second.range.end_idx,
			)
		} else {
			return expr
		}
	}

	expect_comparison_or_higher :: proc(p: ^Parser) -> Expression {
		first := expect_add_or_sub_or_higher(p)
		if _, is_cmp := cmp_operator_token_ty_to_kind(current(p).ty); !is_cmp {
			return first
		}
		cmp_operator_token_ty_to_kind :: proc(
			ty: TokenType,
		) -> (
			kind: ComparisonKind,
			is_cmp: bool,
		) {
			#partial switch ty {
			case .Greater:
				return .Greater, true
			case .GreaterEqual:
				return .GreaterEqual, true
			case .Less:
				return .Less, true
			case .LessEqual:
				return .LessEqual, true
			case .Equal:
				return .Equal, true
			case .NotEqual:
				return .NotEqual, true
			}
			return {}, false
		}
		others: [dynamic]ComparisonElement
		for {
			kind, is_cmp := cmp_operator_token_ty_to_kind(current(p).ty)
			if !is_cmp {
				break
			}
			// comparison_operator
			next(p)
			next_expr := expect_add_or_sub_or_higher(p)
			append(&others, ComparisonElement{kind, next_expr})
		}
		start := first.range.start_idx
		return expression(
			Comparison{first = new_clone(first), others = others[:]},
			start,
			p.current,
		)
	}
	expect_add_or_sub_or_higher :: proc(p: ^Parser) -> Expression {
		first := expect_mul_or_div_or_higher(p)
		if accept_token(p, .Add) {
			second := expect_add_or_sub_or_higher(p)
			return math_op_expr(.Add, first, second)
		} else if accept_token(p, .Sub) {
			second := expect_add_or_sub_or_higher(p)
			return math_op_expr(.Sub, first, second)
		}
		return first
	}
	expect_mul_or_div_or_higher :: proc(p: ^Parser) -> Expression {
		first := expect_unary_like_or_higher(p)
		if accept_token(p, .Mul) {
			second := expect_mul_or_div_or_higher(p)
			return math_op_expr(.Mul, first, second)
		} else if accept_token(p, .Div) {
			second := expect_mul_or_div_or_higher(p)
			return math_op_expr(.Div, first, second)
		}
		return first
	}
	expect_unary_like_or_higher :: proc(p: ^Parser) -> (expr: Expression) {
		if accept_token(p, .Sub) {
			// negative numeric expressions e.g. -2
			first := expect_unary_like_or_higher(p)
			return expression(
				NegateExpression(new_clone(first)),
				first.range.start_idx - 1,
				first.range.end_idx,
			)
		} else if accept_token(p, .Not) {
			// not operator e.g. !myfun()
			first := expect_unary_like_or_higher(p)
			return expression(
				NotExpression(new_clone(first)),
				first.range.start_idx - 1,
				first.range.end_idx,
			)
		} else {
			// single value or parens around some expression, then check if function call or indexing directly behind:

			// TODO: check for continuation of the expression via .Dot
			// - either accesses some value like so:     age := get_person(3).age
			// - or calls a function with dot noration:  10.mul(3).print()
			expr_ok: bool
			expr = expect_single_value(p)
			if finished(p) || !expression_valid(expr) {
				return expr
			}

			for {
				if accept_token(p, .Dot) {
					if ident, ident_ok := expect_ident(p); ident_ok {
						parent := new_clone(expr)
						expr = expression(
							AccessOp{parent, ident},
							parent.range.start_idx,
							p.current,
						)
					} else {
						expr = invalid_expression(
							expr.range.start_idx,
							p.current,
							"an ident needs to follow the dot",
						)
					}
				} else if accept_left_paren_connected_to_last_token(p) {
					args := expect_expressions(p, .RightParen)
					right_paren_afterwards := expect_token(p, .RightParen)
					assert(right_paren_afterwards) // todo, deal with error
					start := expr.range.start_idx
					fn_expr := new_clone(expr)
					expr = expression(CallOp{fn_expr, args}, start, p.current)
				} else if accept_left_bracket_connected_to_last_token(p) {
					index := expect_expression(p)
					right_bracket_afterwards := expect_token(p, .RightBracket)
					assert(right_bracket_afterwards) // todo, deal with error
					start := expr.range.start_idx
					place := new_clone(expr)
					expr = expression(IndexOp{place, new_clone(index)}, start, p.current)
				} else {
					// check if this is named struct literal
					_, is_ident := expr.kind.(Ident)
					if is_ident && accept_left_brace_connected_to_last_token(p) {
						struct_lit: LitStruct
						struct_lit.fields = expect_struct_literal_fields(p)
						right_brace_afterwards := expect_token(p, .RightBrace)
						assert(right_brace_afterwards) // todo, deal with error
						struct_lit.name = new_clone(expr)
						start := expr.range.start_idx
						expr = expression(struct_lit, start, p.current)
					} else {
						break
					}
				}
			}
			return expr
		}
	}
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
			if !accept_token(p, .LeftBrace) {
				return invalid_expression(
					p.current - 1,
					p.current,
					"following enum keyword there should be '{'",
				)
			}
			start := p.current - 2
			variants: [dynamic]Ident
			for {
				cur := current(p).ty
				if cur == .RightBrace || cur == .Eof {
					break
				}
				if ident, ok := accept_ident(p); ok {
					append(&variants, ident)
				} else {
					// error! should only have idents in here! (skip until closing brace or Eof, then set this as an error expression)
					for {
						tok := next(p).ty
						if tok == .RightBrace || tok == .Eof {
							break
						}
					}
					return invalid_expression(start, p.current, "invalid enum definition")
				}
			}
			accept_token(p, .RightBrace)
			return expression(EnumDecl{variants[:]}, start, p.current)
		case .LeftParen:
			start := p.current - 1
			this := current(p)
			next := peek(p)
			if this.ty == .Ident && next.ty == .Colon {
				// parse a function definition
				// expect_function_def_arguments ()
				fun_def: FunctionDefinition
				fun_def.args = expect_function_definition_args(p)
				if !expect_token(p, .RightParen) {
					return invalid_expression(start, p.current)
				}

				// return type arrow is optional, if not present -> None is assumed.

				if accept_token(p, .Arrow) {
					return_type := expect_expression(p)
					fun_def.return_type = new_clone(return_type)
				}
				if !expect_token(p, .LeftBrace) {
					return invalid_expression(start, p.current)
				}
				fun_def.body = expect_statements(p)
				if !expect_token(p, .RightBrace) {
					return invalid_expression(start, p.current)
				}
				return expression(fun_def, start, p.current)
			} else {
				expr := expect_expression(p)
				this = current(p)
				next = peek(p)
				if this.ty == .RightParen && next.ty != .Arrow {
					// expression in parentheses, e.g. (4+5)

					if accept_token(p, .RightParen) {
						return expr // Q: should we extend the token range by 1 in both directions to include the parens?
					} else {
						err_range, _found_right_paren := skip_to_next(p, .RightParen)
						add_error(
							p,
							err_range,
							"expected a single expression in parens, got additional tokens",
						)
						return invalid_expression(
							start,
							p.current,
							"more than one expression in parens",
						)
					}
				} else {
					// function type/signature
					// parse the rest of the expressions in the arg type list
					arg_types: [dynamic]Expression = {expr}
					_expect_expressions(p, .RightParen, &arg_types)
					expect_token(p, .RightParen)
					expect_token(p, .Arrow)
					return_type := expect_expression(p)
					function_signature := FunctionSignature{arg_types[:], new_clone(return_type)}
					return expression(function_signature, start, p.current)
				}
			}
			unreachable()
		case .LeftBracket:
			// [3,4,5]
			// could also be type e.g. [int] or [{s: int, f: float}] or [enum{Red, Black}]
			start := p.current
			values := expect_expressions(p, .RightBracket)
			accept_token(p, .RightBracket)
			return expression(LitArray{values}, start, p.current)
		case .LeftBrace:
			start := p.current - 1
			struct_lit: LitStruct
			struct_lit.fields = expect_struct_literal_fields(p)
			right_brace_afterwards := expect_token(p, .RightBrace)
			assert(right_brace_afterwards) // todo, deal with error
			struct_lit.name = nil
			return expression(struct_lit, start, p.current)
		}
		panic(
			"no token matched start of expression, this case should have been handled in .CouldBeExpressionStart not_in TOKEN_TYPE_FLAGS[current(p).ty]",
		)
	}
}

skip_to_next :: proc(p: ^Parser, until: TokenType) -> (range: TokenRange, success: bool) {
	start := p.current
	for {
		cur := current(p).ty
		if cur == until {
			return TokenRange{start, p.current}, true
		} else if cur == .Eof {
			return TokenRange{start, p.current}, false
		}
	}
}

expect_function_definition_args :: proc(p: ^Parser) -> []FunctionArg {
	args_list: [dynamic]FunctionArg
	for {
		if current(p).ty == .RightParen {
			break
		}
		if ident, ok := accept_ident(p); ok {
			if accept_token(p, .Colon) {
				ty := expect_expression(p)
				append(&args_list, FunctionArg{ident, ty})
			} else {
				err_range, found_colon := skip_to_next(p, .Colon)
				add_error(p, err_range)
				if found_colon {
					skip(p)
					ty := expect_expression(p)
					append(&args_list, FunctionArg{ident, ty})
				} else {
					break // returning the current args list
				}
			}
		} else {
			// skip over tokens until next int is found
			err_range, found_ident := skip_to_next(p, .Ident)
			add_error(p, err_range)
			if found_ident {
				continue
			} else {
				break // returning the current args list
			}
		}
	}
	return args_list[:]
}

expect_struct_literal_fields :: proc(p: ^Parser) -> []LitStructField {
	fields: [dynamic]LitStructField
	for {
		cur := current(p).ty
		if cur == .RightBrace || cur == .Eof {
			break
		}
		val_or_field_name := expect_expression(p)
		if accept_token(p, .Colon) {
			// named field
			value := expect_expression(p)
			append(
				&fields,
				LitStructField{name = new_clone(val_or_field_name), value = new_clone(value)},
			)
		} else {
			// unnamed field
			append(&fields, LitStructField{name = nil, value = new_clone(val_or_field_name)})
		}
	}
	return fields[:]
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
