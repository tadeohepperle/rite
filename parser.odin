package rite

import "core:log"

Parser :: struct {
	tokens:  []Token,
	current: int,
	errors:  ^Errors,
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
		return Ident{tok.data.string, p.current - 1}, true
	} else {
		return {}, false
	}
}
accept_ident :: proc(p: ^Parser) -> (Ident, bool) {
	cur := current(p)
	if cur.ty == .Ident {
		ident := Ident{cur.data.string, p.current}
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
	for {
		cur := current(p).ty
		if cur == .RightBrace || cur == .Eof {
			break
		}

		stmt, stmt_ok := expect_statement(p)
		if stmt_ok {
			append(&res, stmt)
		}
	}
	return res[:]
}

expect_statement :: proc(p: ^Parser) -> (res: Statement, success: bool) {
	current_ty := current(p).ty
	if current_ty == .Break {
		skip(p) // the .Break
		return Statement(BreakStatement{}), true
	} else if current_ty == .Return {
		skip(p) // the .Return
		return_stmt: ReturnStatement
		if has_flag(current(p).ty, .CouldBeExpressionStart) {
			return_stmt.value = expect_expression(p) or_return
		}
		return Statement(return_stmt), true
	} else if current_ty == .For {
		todo() // XXX
		// for_loop := expect_for_loop(p) 
		// return Statement(for_loop), nil
	} else if current_ty == .If {
		if_stmt := expect_if_statement(p) or_return
		return Statement(if_stmt), true
	} else if current_ty == .Switch {
		todo()
	} else {
		return expect_assignment_declaration_or_expression(p)
	}
	unreachable()
}

// only call if you already know the current token is .If
expect_if_statement :: proc(p: ^Parser) -> (if_stmt: IfStatement, success: bool) {
	start := p.current
	assert(expect_token(p, .If))
	if_stmt.condition = expect_expression(p) or_return
	if !expect_token(p, .LeftBrace) {
		errors_add(
			p.errors,
			TokenRange{start, p.current},
			"expected `{` after condition of If-Statement",
		)
		return {}, false
	}
	if_stmt.body = expect_statements(p)
	expect_token(p, .RightBrace) or_return
	if !expect_token(p, .Else) {
		return if_stmt, true
	}


	#partial switch current(p).ty {


	case .If:
		// an `else if` block
		nested_if, nested_if_ok := expect_if_statement(p)
		if !nested_if_ok {
			return {}, false
		}
		if_stmt.else_block = new_clone(nested_if)
		return if_stmt, true
	case .LeftBrace:
		// a normal `else` block
		skip(p) // skip over the `{` after the `else` token
		else_block: ElseBlock
		else_block.body = expect_statements(p)
		if !expect_token(p, .RightBrace) {
			errors_add(
				p.errors,
				TokenRange{start, p.current},
				"expected right brace token after statements in else block",
			)
			return {}, false
		}
		if_stmt.else_block = else_block
		return if_stmt, true
	case:
		errors_add(p.errors, TokenRange{start, p.current}, "expected `{` or `if` after `else`")
		return {}, false
	}
	unreachable()
}

expect_assignment_declaration_or_expression :: proc(
	p: ^Parser,
) -> (
	stmt: Statement,
	success: bool,
) {
	start := p.current
	first_expr := expect_expression(p) or_return
	cur := current(p).ty

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
			second_expr := expect_expression(p) or_return
			assignment := Assignment {
				place = first_expr,
				kind  = check.assign_kind,
				value = second_expr,
			}
			return Statement(assignment), true
		}
	}

	// maybe this is a declaration with ::, : Ty :, :=, : Ty or : Ty =  
	if cur == .ColonColon || cur == .Colon || cur == .ColonAssign {
		// declaration place needs to be single ident for now: (future: support e.g. Vec2(T) :: {T,T})
		ident, is_ident := first_expr.kind.(Ident)
		if !is_ident {
			skip(p) // skip the current_ty
			errors_add(
				p.errors,
				TokenRange{start, p.current},
				"first expression in declaration must be ident",
			)
			return {}, false
		}
		decl := Declaration {
			ident = ident,
		}
		if expect_token(p, .ColonColon) {
			decl.kind = .ConstInferred
			value_ok: bool
			decl.value = expect_expression(p) or_return
		} else if expect_token(p, .ColonAssign) {
			decl.kind = .RuntimeInferred
			value_ok: bool
			decl.value = expect_expression(p) or_return
		} else if expect_token(p, .Colon) {
			decl.ty = expect_expression(p) or_return
			if expect_token(p, .Colon) {
				decl.kind = .ConstExplicit
				decl.value = expect_expression(p) or_return
				// future: maybe if decl.value is not valid expression, we can still move on because at least the type is known already,
				// set Placeholder value maybe
			} else if expect_token(p, .Assign) {
				decl.kind = .RuntimeExplicit
				decl.value = expect_expression(p) or_return
			} else {
				decl.kind = .RuntimeExplicitDefault
			}
		}
		assert(decl.ty != nil || decl.value != nil)
		return Statement(decl), true
	}
	// just return the parsed expression as a single statement, there seems to be no assignment or declaration here:
	return Statement(first_expr), true
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
		expr := expect_expression(p) or_continue
		append(res, expr)
	}
}

expect_expression :: proc(p: ^Parser) -> (Expression, bool) {
	log.info("expect_expression")

	expr, ok := expect_logical_or_or_higher(p)
	log.info("-> got expr:", expr)
	return expr, ok

	expect_logical_or_or_higher :: proc(p: ^Parser) -> (expr: Expression, success: bool) {
		log.info("  expect_logical_or_or_higher")
		expr = expect_logical_and_or_higher(p) or_return
		if current(p).ty == .Or {
			second := expect_logical_or_or_higher(p) or_return
			return expression(LogicalOp{.Or, new_clone(expr), new_clone(second)}), true
		} else {
			return expr, true
		}
	}

	expect_logical_and_or_higher :: proc(p: ^Parser) -> (expr: Expression, success: bool) {
		log.info("    expect_logical_and_or_higher")
		expr = expect_comparison_or_higher(p) or_return
		if current(p).ty == .And {
			second := expect_logical_and_or_higher(p) or_return
			return expression(LogicalOp{.And, new_clone(expr), new_clone(second)}), true
		} else {
			return expr, true
		}
	}

	expect_comparison_or_higher :: proc(p: ^Parser) -> (expr: Expression, success: bool) {
		log.info("      expect_comparison_or_higher")
		first := expect_add_or_sub_or_higher(p) or_return
		if _, is_cmp := cmp_operator_token_ty_to_kind(current(p).ty); !is_cmp {
			return first, true
		}
		cmp_operator_token_ty_to_kind :: proc(
			ty: TokenType,
		) -> (
			kind: CompareOpKind,
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
		others: [dynamic]CompareOpElement
		for {
			kind, is_cmp := cmp_operator_token_ty_to_kind(current(p).ty)
			if !is_cmp {
				break
			}
			// skip the comparison_operator
			skip(p)
			next_expr := expect_add_or_sub_or_higher(p) or_return
			append(&others, CompareOpElement{kind, next_expr})
		}
		return expression(CompareOp{first = new_clone(first), others = others[:]}), true
	}
	expect_add_or_sub_or_higher :: proc(p: ^Parser) -> (expr: Expression, success: bool) {
		log.info("      expect_add_or_sub_or_higher")
		first := expect_mul_or_div_or_higher(p) or_return
		if expect_token(p, .Add) {
			second := expect_add_or_sub_or_higher(p) or_return
			return math_op_expr(.Add, first, second), true
		} else if expect_token(p, .Sub) {
			second := expect_add_or_sub_or_higher(p) or_return
			return math_op_expr(.Sub, first, second), true
		}
		return first, true
	}
	expect_mul_or_div_or_higher :: proc(p: ^Parser) -> (expr: Expression, success: bool) {
		log.info("        expect_mul_or_div_or_higher")
		first := expect_unary_like_or_higher(p) or_return
		if expect_token(p, .Mul) {
			second := expect_mul_or_div_or_higher(p) or_return
			return math_op_expr(.Mul, first, second), true
		} else if expect_token(p, .Div) {
			second := expect_mul_or_div_or_higher(p) or_return
			return math_op_expr(.Div, first, second), true
		}
		return first, true
	}
	expect_unary_like_or_higher :: proc(p: ^Parser) -> (expr: Expression, success: bool) {
		log.info("        expect_unary_like_or_higher")
		if expect_token(p, .Sub) {
			// negative numeric expressions e.g. -2
			first := expect_unary_like_or_higher(p) or_return
			return expression(NegateExpression{inner = new_clone(first)}), true
		} else if expect_token(p, .Not) {
			// not operator e.g. !myfun()
			first := expect_unary_like_or_higher(p) or_return
			return expression(NotExpression{inner = new_clone(first)}), true
		} else {
			start := p.current
			// single value or parens around some expression, then check if function call or indexing directly behind:

			// TODO: check for continuation of the expression via .Dot
			// - either accesses some value like so:     age := get_person(3).age
			// - or calls a function with dot noration:  10.mul(3).print()
			expr = expect_single_value(p) or_return
			log.info("    -> got single value:", expr)
			for {
				if expect_token(p, .Dot) {
					if ident, ident_ok := expect_ident(p); ident_ok {
						parent := new_clone(expr)
						expr = expression(AccessOp{parent, ident})
					} else {
						errors_add(
							p.errors,
							TokenRange{start, p.current},
							"ident expected, following `.`, instead found",
							token_as_code(current(p)),
						)
						return {}, false
					}
				} else if accept_left_paren_connected_to_last_token(p) {
					args := expect_expressions(p, .RightParen)
					right_paren_afterwards := expect_token(p, .RightParen)
					assert(right_paren_afterwards) // todo, deal with error
					fn_expr := new_clone(expr)
					expr = expression(CallOp{fn_expr, args})
				} else if accept_left_bracket_connected_to_last_token(p) {
					index := expect_expression(p) or_return

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
					lit_struct: StructLiteral
					lit_struct.fields = expect_struct_literal_fields(p) or_return
					lit_struct.name_or_brace_token_idx = new_clone(expr)
					expr = expression(lit_struct)
				}
			}
			return expr, true
		}
	}
	expect_single_value :: proc(p: ^Parser) -> (expr: Expression, success: bool) {
		log.info("        expect_single_value")
		if .CouldBeExpressionStart not_in TOKEN_TYPE_FLAGS[current(p).ty] {
			errors_add(
				p.errors,
				TokenRange{p.current, p.current + 1},
				current(p).ty,
				" token cannot be expression start",
			)
			skip(p)
			return {}, false
		}
		start := p.current
		tok := next(p)
		#partial switch tok.ty {
		case .LitBool:
			value := PrimitiveValue{.Bool, {bool = tok.data.bool}}
			return expression(Primitive{value, start}), true
		case .LitInt:
			value := PrimitiveValue{.Int, {int = tok.data.int}}
			return expression(Primitive{value, start}), true
		case .LitFloat:
			value := PrimitiveValue{.Int, {float = tok.data.float}}
			return expression(Primitive{value, start}), true
		case .LitChar:
			value := PrimitiveValue{.Int, {char = tok.data.char}}
			return expression(Primitive{value, start}), true
		case .LitString:
			value := PrimitiveValue{.Int, {string = tok.data.string}}
			return expression(Primitive{value, start}), true
		case .PrimitiveType:
			value := PrimitiveValue{.Type, {primitive_type = tok.data.primitive_type}}
			return expression(Primitive{value, start}), true
		case .Ident:
			return expression(Ident{tok.data.string, start}), true
		case .Enum:
			if !expect_token(p, .LeftBrace) {
				errors_add(
					p.errors,
					TokenRange{start, p.current},
					"enum keyword without `{` following",
				)
				return {}, false
			}
			variants: [dynamic]Ident
			for {
				if ident, ok := accept_ident(p); ok {
					append(&variants, ident)
				} else if expect_token(p, .RightBrace) {
					break
				} else {
					delete(variants)
					skip_until_after(p, .RightBrace)
					errors_add(p.errors, {start, p.current}, "non ident in enum decl")
					return {}, false
				}
			}
			return expression(EnumDecl{variants[:], start}), true
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
					skip_until_after(p, .RightParen)
					errors_add(p.errors, TokenRange{start, p.current}, "invalid fn args")
					return {}, false
				}

				// return type arrow is optional, if not present -> None is assumed.
				if expect_token(p, .Arrow) {
					return_type, return_type_ok := expect_expression(p)
					if !return_type_ok {
						errors_add(
							p.errors,
							TokenRange{start, p.current},
							"invalid return type expression after `->`",
						)
						return {}, false
					}
					fn_def.return_type = new_clone(return_type)
				}
				if !expect_token(p, .LeftBrace) {
					errors_add(
						p.errors,
						TokenRange{start, p.current},
						"Expected `{` to start function body",
					)
					return {}, false
				}
				fn_def.body = expect_statements(p)
				if !expect_token(p, .RightBrace) {
					errors_add(
						p.errors,
						TokenRange{start, p.current},
						"Expected '}' to close function body",
					)
					return {}, false
				}
				return expression(fn_def), true
			} else if this.ty == .RightParen {
				// function definition or signature type without arguments
				// so `() -> Foo {}`, `() {}` or `() -> Foo`
				skip(p) // skip over the `)`.
				return_type: Maybe(^Expression) = nil
				if expect_token(p, .Arrow) {
					arrow_idx := p.current - 1
					return_ty, return_type_ok := expect_expression(p)
					if !return_type_ok {
						errors_add(
							p.errors,
							TokenRange{arrow_idx, p.current},
							"Invalid return type expression after `->`",
						)
						return {}, false
					}
					return_type = new_clone(return_ty)
				}
				if expect_token(p, .LeftBrace) {
					// start of function definition
					fn_def := FunctionDefinition {
						args                  = nil,
						return_type           = return_type,
						paren_token_start_idx = start,
					}
					fn_def.body = expect_statements(p)
					if expect_token(p, .RightBrace) {
						return expression(fn_def), true
					} else {
						skip_until_after(p, .RightBrace)
						errors_add(
							p.errors,
							TokenRange{start, p.current},
							"Expected '}' to close function body",
						)
						return {}, false
					}
				} else {
					// function signature without body or just `()`
					if return_type, ok := return_type.(^Expression); ok {
						function_signature := FunctionSignature {
							arg_types   = nil,
							return_type = return_type,
						}
						return expression(function_signature), true
					} else {
						errors_add(p.errors, {start, start + 2}, "`()` is not an expression")
						return {}, false
					}
				}
			} else {
				expr := expect_expression(p) or_return
				this = current(p)
				next = peek(p)
				if this.ty == .RightParen && next.ty != .Arrow {
					// expression in parentheses, e.g. (4+5)
					if expect_token(p, .RightParen) {
						return expr, true
					} else {
						skip_until_after(p, .RightParen)
						errors_add(
							p.errors,
							TokenRange{start, p.current},
							"more than one expression in parens",
						)
						return {}, false
					}
				} else {
					// function type/signature
					// parse the rest of the expressions in the arg type list
					arg_types: [dynamic]Expression = {expr}
					_expect_expressions(p, .RightParen, &arg_types)
					if !expect_token(p, .RightParen) {
						errors_add(
							p.errors,
							TokenRange{start, p.current},
							"expected `)` after arg types",
						)
						return {}, false
					}
					arrow_idx := p.current
					if !expect_token(p, .Arrow) {
						errors_add(
							p.errors,
							TokenRange{start, p.current},
							"expected `->` after ( ...arg_types ) in function signature",
						)
						return {}, false
					}
					return_type, return_type_ok := expect_expression(p)
					if !return_type_ok {
						errors_add(
							p.errors,
							TokenRange{arrow_idx, p.current},
							"Invalid return type expression after `->`",
						)
						return {}, false
					}
					function_signature := FunctionSignature{arg_types[:], new_clone(return_type)}
					return expression(function_signature), true
				}
			}
			unreachable()
		case .LeftBracket:
			// [3,4,5]
			// could also be type e.g. [int] or [{s: int, f: float}] or [enum{Red, Black}]
			start_bracket_token_idx := p.current - 1
			values := expect_expressions(p, .RightBracket)
			if !expect_token(p, .RightBracket) {
				errors_add(
					p.errors,
					{start_bracket_token_idx, p.current + 1},
					"list or map literal not ended, expected `]` after expressions",
				)
			} else {
				return expression(ArrayLiteral{values, start_bracket_token_idx}), true
			}
		case .LeftBrace:
			brace_idx := p.current - 1
			lit_struct: StructLiteral
			lit_struct.name_or_brace_token_idx = brace_idx

			fields_ok: bool
			lit_struct.fields = expect_struct_literal_fields(p) or_return
			if !expect_token(p, .RightBrace) {
				errors_add(
					p.errors,
					{brace_idx, p.current + 1},
					"expected `}` to close struct literal, found",
					token_as_code(current(p)),
				)
				skip_until_after(p, .RightBrace)
				return {}, false
			}
			return expression(lit_struct), true
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

skip_until_after :: proc(p: ^Parser, until: TokenType) -> (found_until: bool) {
	for {
		tok_ty := current(p).ty
		if tok_ty == until {
			skip(p)
			return true
		} else if tok_ty == .Eof {
			return false
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
			return args_list[:], true
		}
		ident := accept_ident(p) or_return
		expect_token(p, .Colon) or_return
		ty := expect_expression(p) or_return
		append(&args_list, FunctionArg{ident, ty})
	}
}


// fields inside the `{` and `}` curcly braces. Could be no fields (nil), named fields, or unnamed fields
// but not mixing named and unnamed fields!
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
			last_ok_idx := p.current
			pair_ok: bool
			if ident, ok := accept_ident(p); ok {
				if expect_token(p, .Colon) {
					cur = current(p)
					if cur.ty != .Eof && cur.ty != .RightBrace {
						if value, value_ok := expect_expression(p); value_ok {
							append(&fields, NamedField{ident, value})
							pair_ok = true
						}
					}
				}
			}
			if !pair_ok {
				skip_until_after(p, .RightBrace)
				errors_add(
					p.errors,
					TokenRange{last_ok_idx, p.current},
					"invalid expression in struct literal",
				)
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


parse :: proc(tokens: []Token, errors: ^Errors) -> Module {
	parser := Parser {
		tokens  = tokens,
		current = 0,
		errors  = errors,
	}
	statements := expect_statements(&parser)
	return Module{statements = statements, tokens = tokens, scope = nil}
}


parse_expression :: proc(tokens: []Token) -> Expression {
	parser := Parser {
		tokens  = tokens,
		current = 0,
	}
	return expect_expression(&parser) or_else panic("could not parse expr")
}
