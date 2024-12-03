package rite

import "core:log"

Parser :: struct {
	tokens:  []Token,
	current: int,
}


// todo: add #no_bounds_check
current :: proc(p: ^Parser) -> Token {
	if len(p.tokens) > p.current {
		return p.tokens[p.current]
	} else {
		return token(.Eof)
	}
}
next :: proc(p: ^Parser) -> Token {
	if len(p.tokens) > p.current {
		ret := p.tokens[p.current]
		p.current += 1
		return ret
	} else {
		return token(.Eof)
	}
}

// skips one token, useful if you already know what this token is.
skip :: #force_inline proc(p: ^Parser) {
	p.current += 1
}

peek :: proc(p: ^Parser) -> Token {
	idx := p.current + 1
	if idx < len(p.tokens) {
		return p.tokens[idx]
	} else {
		return token(.Eof)
	}
}
expect_token :: proc(p: ^Parser, ty: TokenType) -> bool {
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
		return Ident{tok.meta.string, p.current - 1}, true
	} else {
		return {}, false
	}
}
accept_ident :: proc(p: ^Parser) -> (Ident, bool) {
	cur := current(p)
	if cur.ty == .Ident {
		ident := Ident{cur.meta.string, p.current}
		p.current += 1
		return ident, true
	}
	return {}, false
}

accept_left_paren_connected_to_last_token :: proc(p: ^Parser) -> bool {
	cur := current(p)
	accepted := cur.ty == .LeftParen && cur.seperation == .None
	if accepted {
		log.info("Accepted left paren connected to last token")
		p.current += 1
	}
	return accepted
}
accept_left_bracket_connected_to_last_token :: proc(p: ^Parser) -> bool {
	cur := current(p)
	accepted := cur.ty == .LeftBracket && cur.seperation == .None
	if accepted {
		log.info("Accepted left bracket connected to last token")
		p.current += 1
	}
	return accepted
}
accept_left_brace_connected_to_last_token :: proc(p: ^Parser) -> bool {
	cur := current(p)
	accepted := cur.ty == .LeftBrace && cur.seperation == .None
	if accepted {
		log.info("Accepted left brace connected to last token")
		p.current += 1
	}
	return accepted
}

expect_statements :: proc(p: ^Parser) -> []Statement {
	res: [dynamic]Statement
	for (.CouldBeStatementStart in TOKEN_TYPE_FLAGS[current(p).ty]) {
		expr := expect_statement(p)
		append(&res, expr)
	}
	cur := current(p).ty
	if cur != .RightBrace && cur != .Eof {
		print(current(p))
		invalid_range, _ := skip_until(p, .RightBrace)
		append(&res, invalid_expression(p, "invalid tokens after statements", invalid_range))
	}
	return res[:]
}

expect_statement :: proc(p: ^Parser) -> Statement {
	current_ty := current(p).ty
	if current_ty == .Break {
		skip(p) // the .Break
		return Statement(BreakStatement{})
	} else if current_ty == .Return {
		skip(p) // the .Return
		return_stmt: ReturnStatement
		if has_flag(current(p).ty, .CouldBeExpressionStart) {
			return_stmt.value = expect_expression(p)
		}
		return Statement(return_stmt)
	} else if current_ty == .For {
		todo() // XXX
		// for_loop := expect_for_loop(p) 
		// return Statement(for_loop), nil
	} else if current_ty == .If {

		switch if_or_invalid in expect_if_statement(p) {
		case IfStatement:
			return Statement(if_or_invalid)
		case InvalidExpression:
			return Statement(expression(if_or_invalid))
		}
	} else if current_ty == .Switch {
		todo()
	} else {
		return expect_assignment_declaration_or_expression(p)
	}
	unreachable()
}

// only call if you already know the current token is .If

IfStatementOrErr :: union #no_nil {
	IfStatement,
	InvalidExpression,
}
expect_if_statement :: proc(p: ^Parser) -> (statement: IfStatementOrErr) {
	start := p.current
	assert(expect_token(p, .If))
	if_stmt: IfStatement
	if_stmt.condition = expect_expression(p)
	if !expect_token(p, .LeftBrace) {
		err_range := TokenRange{start, p.current}
		return _invalid_expression(p, "expected `{` after condition of If-Statement", err_range)
	}
	if_stmt.body = expect_statements(p)

	if invalid_ex, is_invalid := _eat_right_brace(p, start).(InvalidExpression); is_invalid {
		return invalid_ex
	}
	if !expect_token(p, .Else) {
		return if_stmt
	}

	cur := current(p).ty
	if cur == .If {
		// else if block:
		switch else_if in expect_if_statement(p) {
		case IfStatement:
			if_stmt.else_block = new_clone(else_if)
			return if_stmt
		case InvalidExpression:
			return _invalid_expression(p, else_if.msg, TokenRange{start, else_if.tokens.end_idx})
		}
	} else if cur == .LeftBrace {
		skip(p) // skip over `{`
		// else block
		else_block: ElseBlock
		else_block.body = expect_statements(p)
		if invalid_ex, is_invalid := _eat_right_brace(p, start).(InvalidExpression); is_invalid {
			return invalid_ex
		} else {
			if_stmt.else_block = else_block
			return if_stmt
		}
	} else {
		// no `{` after else, error:
		return _invalid_expression(p, "expected `{` after `else`", TokenRange{start, p.current})
	}
	unreachable()

	_eat_right_brace :: proc(p: ^Parser, start: int) -> Maybe(InvalidExpression) {
		if expect_token(p, .RightBrace) {
			return nil
		}
		range, found_brace := skip_until(p, .RightBrace)
		if found_brace {
			skip(p)
			range.end_idx += 1
		}
		range.start_idx = start
		return _invalid_expression(p, "expected `}` after statements", range)
	}
}

expect_assignment_declaration_or_expression :: proc(p: ^Parser) -> (statement: Statement) {
	start := p.current
	first_expr := expect_expression(p)
	current_ty := current(p).ty

	// maybe this is the target place of an assignment with =, +=, -=, *= or /=:
	TokenAndAssignKind :: struct {
		token_ty:    TokenType,
		assign_kind: AssignmentKind,
	}
	CHECK_ASSIGNMENTS :: [?]TokenAndAssignKind {
		{.Assign, .Assign},
		{.AddAssign, .AddAssign},
		{.SubAssign, .SubAssign},
		{.MulAssign, .MulAssign},
		{.DivAssign, .DivAssign},
	}
	for check in CHECK_ASSIGNMENTS {
		if expect_token(p, check.token_ty) {
			second_expr := expect_expression(p)
			return Statement(
				Assignment{place = first_expr, kind = check.assign_kind, value = second_expr},
			)
		}
	}

	// maybe this is a declaration with ::, : Ty :, :=, : Ty or : Ty =  
	if current_ty == .ColonColon || current_ty == .Colon || current_ty == .ColonAssign {
		// place needs to be single ident for declaration now:
		ident: Ident = ---
		if idnt, is_idnt := first_expr.kind.(Ident); is_idnt {
			ident = idnt
		} else {
			skip(p) // skip the current_ty
			return Statement(
				invalid_expression(
					p,
					"first expression in declaration must be ident",
					TokenRange{start, p.current},
				),
			)
		}
		decl := Declaration {
			ident = ident,
		}
		if expect_token(p, .ColonColon) {
			decl.kind = .ConstInferred
			decl.value = expect_expression(p)
		} else if expect_token(p, .ColonAssign) {
			decl.kind = .RuntimeInferred
			decl.value = expect_expression(p)
		} else if expect_token(p, .Colon) {
			decl.ty = expect_expression(p)
			if expect_token(p, .Colon) {
				decl.kind = .ConstExplicit
				decl.value = expect_expression(p)
			} else if expect_token(p, .Assign) {
				decl.kind = .RuntimeExplicit
				decl.value = expect_expression(p)
			} else {
				decl.kind = .RuntimeExplicitDefault
			}
		}
		assert(decl.ty != nil || decl.value != nil)
		return Statement(decl)
	}
	// just return the parsed expression, there seems to be no assignment or declaration here:
	return Statement(first_expr)
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
	log.info("expect_expression")

	expr := expect_logical_or_or_higher(p)
	log.info("-> got expr:", expr)
	return expr

	expect_logical_or_or_higher :: proc(p: ^Parser) -> (expr: Expression) {
		log.info("  expect_logical_or_or_higher")
		expr = expect_logical_and_or_higher(p)
		if current(p).ty == .Or {
			second := expect_logical_or_or_higher(p)
			return expression(LogicalOr{new_clone(expr), new_clone(second)})
		} else {
			return expr
		}
	}

	expect_logical_and_or_higher :: proc(p: ^Parser) -> (expr: Expression) {
		log.info("    expect_logical_and_or_higher")
		expr = expect_comparison_or_higher(p)
		if current(p).ty == .And {
			second := expect_logical_and_or_higher(p)
			return expression(LogicalOr{new_clone(expr), new_clone(second)})
		} else {
			return expr
		}
	}

	expect_comparison_or_higher :: proc(p: ^Parser) -> Expression {
		log.info("      expect_comparison_or_higher")
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
			// skip the comparison_operator
			skip(p)
			next_expr := expect_add_or_sub_or_higher(p)
			append(&others, ComparisonElement{kind, next_expr})
		}
		return expression(Comparison{first = new_clone(first), others = others[:]})
	}
	expect_add_or_sub_or_higher :: proc(p: ^Parser) -> Expression {
		log.info("      expect_add_or_sub_or_higher")
		first := expect_mul_or_div_or_higher(p)
		if expect_token(p, .Add) {
			second := expect_add_or_sub_or_higher(p)
			return math_op_expr(.Add, first, second)
		} else if expect_token(p, .Sub) {
			second := expect_add_or_sub_or_higher(p)
			return math_op_expr(.Sub, first, second)
		}
		return first
	}
	expect_mul_or_div_or_higher :: proc(p: ^Parser) -> Expression {
		log.info("        expect_mul_or_div_or_higher")
		first := expect_unary_like_or_higher(p)
		if expect_token(p, .Mul) {
			second := expect_mul_or_div_or_higher(p)
			return math_op_expr(.Mul, first, second)
		} else if expect_token(p, .Div) {
			second := expect_mul_or_div_or_higher(p)
			return math_op_expr(.Div, first, second)
		}
		return first
	}
	expect_unary_like_or_higher :: proc(p: ^Parser) -> (expr: Expression) {
		log.info("        expect_unary_like_or_higher")
		if expect_token(p, .Sub) {
			// negative numeric expressions e.g. -2
			first := expect_unary_like_or_higher(p)
			return expression(NegateExpression{inner = new_clone(first)})
		} else if expect_token(p, .Not) {
			// not operator e.g. !myfun()
			first := expect_unary_like_or_higher(p)
			return expression(NotExpression{inner = new_clone(first)})
		} else {
			// single value or parens around some expression, then check if function call or indexing directly behind:

			// TODO: check for continuation of the expression via .Dot
			// - either accesses some value like so:     age := get_person(3).age
			// - or calls a function with dot noration:  10.mul(3).print()
			expr_ok: bool
			expr = expect_single_value(p)
			log.info("    -> got single value:", expr)
			if !expression_valid(expr) {
				return expr
			}

			for {
				if expect_token(p, .Dot) {
					if ident, ident_ok := expect_ident(p); ident_ok {
						parent := new_clone(expr)
						expr = expression(AccessOp{parent, ident})
					} else {
						range := expression_token_range(expr)
						expr = invalid_expression(
							p,
							"an ident needs to follow the dot",
							TokenRange{range.start_idx, p.current},
						)
					}
				} else if accept_left_paren_connected_to_last_token(p) {
					args := expect_expressions(p, .RightParen)
					right_paren_afterwards := expect_token(p, .RightParen)
					assert(right_paren_afterwards) // todo, deal with error
					fn_expr := new_clone(expr)
					expr = expression(CallOp{fn_expr, args})
				} else if accept_left_bracket_connected_to_last_token(p) {
					index := expect_expression(p)
					right_bracket_afterwards := expect_token(p, .RightBracket)
					assert(right_bracket_afterwards) // todo, deal with error
					place := new_clone(expr)
					expr = expression(IndexOp{place, new_clone(index)})
				} else {
					// check if this is named struct literal // todo! should allow other expressions later too, e.g. Vec(int){2,3,3}
					_, is_ident := expr.kind.(Ident)
					signify_start_of_named_struct :=
						is_ident && accept_left_brace_connected_to_last_token(p)
					if !signify_start_of_named_struct {
						break
					}
					lit_struct: LitStruct
					fields_ok: bool
					lit_struct.fields, fields_ok = expect_struct_literal_fields(p)
					if fields_ok {
						lit_struct.name_or_brace_token_idx = new_clone(expr)
						expr = expression(lit_struct)
					} else {
						name_range := expression_token_range(expr)
						skip_range, found_right_brace := skip_until(p, .RightBrace)
						invalid_range := TokenRange{name_range.start_idx, skip_range.end_idx}
						if found_right_brace {
							invalid_range.end_idx += 1
							skip(p)
						}
						expr = invalid_expression(
							p,
							"invalid struct literal fields",
							invalid_range,
						)
						if !found_right_brace {
							return expr
						}
					}
				}
			}
			return expr
		}
	}
	expect_single_value :: proc(p: ^Parser) -> Expression {
		log.info("        expect_single_value")
		if .CouldBeExpressionStart not_in TOKEN_TYPE_FLAGS[current(p).ty] {
			skip(p)
			return invalid_expression(p, "cannot be expression start", {p.current, p.current + 1})
		}
		tok := next(p)
		#partial switch tok.ty {
		case .LitBool:
			return expression(LitBool{tok.meta.bool, p.current - 1})
		case .LitInt:
			return expression(LitInt{tok.meta.int, p.current - 1})
		case .LitFloat:
			return expression(LitFloat{tok.meta.float, p.current - 1})
		case .LitChar:
			return expression(LitChar{tok.meta.char, p.current - 1})
		case .LitString:
			return expression(LitString{tok.meta.string, p.current - 1})
		case .PrimitiveType:
			return expression(LitPrimitiveType{tok.meta.primitive, p.current - 1})
		case .Ident:
			return expression(Ident{tok.meta.string, p.current - 1})
		case .Enum:
			if !expect_token(p, .LeftBrace) {
				return invalid_expression(
					p,
					"following enum keyword there should be '{'",
					TokenRange{p.current - 1, p.current},
				)
			}
			enum_token_idx := p.current - 2
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
					return invalid_expression(
						p,
						"invalid enum definition",
						TokenRange{enum_token_idx, p.current},
					)
				}
			}
			expect_token(p, .RightBrace)
			return expression(LitEnumType{variants[:], enum_token_idx})
		case .LeftParen:
			start := p.current - 1
			this := current(p)
			next := peek(p)

			if this.ty == .Ident && next.ty == .Colon {
				// parse a function definition e.g. (a: int, b: string) {}    or () -> None { print("Hello") } 
				fn_def: FunctionDefinition
				fn_def.paren_token_start_idx = start
				args_are_ok: bool
				fn_def.args, args_are_ok = expect_function_definition_args(p)
				if !args_are_ok || !expect_token(p, .RightParen) {
					return invalid_expression(p, "invalid fn args", TokenRange{start, p.current})
				}

				// return type arrow is optional, if not present -> None is assumed.

				if expect_token(p, .Arrow) {
					return_type := expect_expression(p)
					fn_def.return_type = new_clone(return_type)
				}
				if !expect_token(p, .LeftBrace) {
					return invalid_expression(p, "Expected '{'", TokenRange{start, p.current})
				}
				fn_def.body = expect_statements(p)
				if !expect_token(p, .RightBrace) {
					return invalid_expression(p, "Expected '}'", TokenRange{start, p.current})
				}
				return expression(fn_def)
			} else if this.ty == .RightParen {
				// function definition or signature type without arguments
				// so `() -> Foo {}`, `() {}` or `() -> Foo`
				skip(p) // skip over the `)`.
				return_type: Maybe(^Expression) = nil
				if expect_token(p, .Arrow) {
					return_type = new_clone(expect_expression(p))
				}
				if expect_token(p, .LeftBrace) {
					// start of function definition
					fn_def := FunctionDefinition {
						args                  = nil,
						return_type           = return_type,
						paren_token_start_idx = start,
					}
					fn_def.body = expect_statements(p)
					print(fn_def.body)
					if expect_token(p, .RightBrace) {
						return expression(fn_def)
					} else {
						invalid_range, found_brace := skip_until(p, .RightBrace)
						if found_brace {
							skip(p)
							invalid_range.end_idx += 1
						}
						return invalid_expression(
							p,
							"function body not closed",
							TokenRange{start, invalid_range.end_idx},
						)
					}
				} else {
					// function signature without body or just `()`
					if return_type, ok := return_type.(^Expression); ok {
						return expression(
							FunctionSignature{arg_types = nil, return_type = return_type},
						)
					} else {
						return invalid_expression(
							p,
							"`()` is not an expression",
							{start, start + 2},
						)
					}
				}
			} else {
				expr := expect_expression(p)
				this = current(p)
				next = peek(p)
				if this.ty == .RightParen && next.ty != .Arrow {
					// expression in parentheses, e.g. (4+5)

					if expect_token(p, .RightParen) {
						return expr
					} else {
						err_range, _found_right_paren := skip_until(p, .RightParen)
						return invalid_expression(
							p,
							"more than one expression in parens",
							TokenRange{start, p.current},
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
					return expression(function_signature)
				}
			}
			unreachable()
		case .LeftBracket:
			// [3,4,5]
			// could also be type e.g. [int] or [{s: int, f: float}] or [enum{Red, Black}]
			start_bracket_token_idx := p.current - 1
			values := expect_expressions(p, .RightBracket)
			if !expect_token(p, .RightBracket) {
				return invalid_expression(
					p,
					"list or map literal not ended",
					{start_bracket_token_idx, p.current + 1},
				)
			} else {
				return expression(LitArray{values, start_bracket_token_idx})
			}

		case .LeftBrace:
			brace_idx := p.current - 1
			lit_struct: LitStruct
			lit_struct.name_or_brace_token_idx = brace_idx

			fields_ok: bool
			lit_struct.fields, fields_ok = expect_struct_literal_fields(p)

			if fields_ok {
				if expect_token(p, .RightBrace) {
					return expression(lit_struct)
				} else {
					return invalid_expression(
						p,
						"struct literal not ended with right brace",
						{brace_idx, p.current + 1},
					)
				}
			} else {
				skip_range, found_right_brace := skip_until(p, .RightBrace)
				invalid_range := TokenRange{brace_idx, skip_range.end_idx}
				if found_right_brace {
					invalid_range.end_idx += 1
					skip(p)
				}
				return invalid_expression(p, "invalid struct literal fields", invalid_range)
			}
		}
		print("tok", tok, p.current, p.tokens)
		panic(
			"token cannot be start of expression, this case should have been handled before: .CouldBeExpressionStart not_in TOKEN_TYPE_FLAGS[current(p).ty]",
		)
	}
}

// the range does not include the `until` token itelf!
// if the current token is already until, the range returned by this is p.current:p.current
skip_until :: proc(p: ^Parser, until: TokenType) -> (range: TokenRange, success: bool) {
	start := p.current
	for {
		tok_ty := current(p).ty
		if tok_ty == until {
			return TokenRange{start, p.current}, true
		} else if tok_ty == .Eof {
			return TokenRange{start, p.current}, false
		}
		skip(p)
	}
}


// if this returns _, true, p.current is one element out of `until`
skip_until_any_of :: proc(p: ^Parser, any_of: []TokenType) -> (range: TokenRange, success: bool) {
	start := p.current
	for {
		tok_ty := current(p).ty
		if tok_ty == .Eof {
			return TokenRange{start, p.current}, false
		}
		for until_ty in any_of {
			if tok_ty == until_ty {
				return TokenRange{start, p.current}, true
			}
		}
		skip(p)
	}
}

// e.g `i: int, person: {int, string}`
expect_function_definition_args :: proc(p: ^Parser) -> (args: []FunctionArg, success: bool) {
	args_list: [dynamic]FunctionArg
	for {
		if current(p).ty == .RightParen {
			break
		}
		if ident, ok := accept_ident(p); ok {
			if expect_token(p, .Colon) {
				ty := expect_expression(p)
				append(&args_list, FunctionArg{ident, ty})
			} else {
				err_range, found_colon := skip_until(p, .Colon)
				if found_colon {
					skip(p)
					ty := expect_expression(p)
					append(&args_list, FunctionArg{ident, ty})
				} else {
					break // returning the current args list
				}
			}
		} else {
			return args_list[:], false
		}
	}
	return args_list[:], true
}


// fields inside the `{` and `}` curcly braces. Could be no fields (nil), named fields, or unnamed fields
// but not mixing named and unnamed fields!
// the error handling here is not great to be honest...
expect_struct_literal_fields :: proc(p: ^Parser) -> (ret: StructFields, success: bool) {
	cur := current(p)
	if cur.ty == .RightBrace {
		return nil, true
	}
	next := peek(p)
	if cur.ty == .Ident && next.ty == .Colon {
		// expect named fields e.g. `foo: 45  bar: "Hello"`
		fields: [dynamic]NamedField
		for {
			pair_ok: bool
			if ident, ok := accept_ident(p); ok {
				if expect_token(p, .Colon) {
					cur = current(p)
					if cur.ty != .Eof && cur.ty != .RightBrace {
						value := expect_expression(p)
						append(&fields, NamedField{ident, value})
						pair_ok = true
					}
				}
			}
			if !pair_ok {
				return nil, false
			}
			if current(p).ty == .RightBrace {
				break
			}
		}
		return fields[:], true
	} else {
		return expect_expressions(p, .RightBrace), true
	}
}


parse :: proc(tokens: []Token) -> Module {
	parser := Parser {
		tokens  = tokens,
		current = 0,
	}
	statements := expect_statements(&parser)
	return Module{statements, nil}
}


parse_expression :: proc(tokens: []Token) -> Expression {
	parser := Parser {
		tokens  = tokens,
		current = 0,
	}
	return expect_expression(&parser)
}
