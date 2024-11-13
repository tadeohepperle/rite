package rite

import "core:fmt"

Parser :: struct {
	tokens:  []Token,
	current: int,
}
finished :: proc(p: ^Parser) -> bool {
	return p.current >= len(p.tokens)
}

log :: proc(args: ..any) {
	fmt.println(..args)
}

current :: proc(p: ^Parser) -> (tok: Token) {
	return p.tokens[p.current]
}

next :: proc(p: ^Parser) -> (tok: Token) {
	tok = p.tokens[p.current]
	p.current += 1
	log("Consumed token ", tok.ty)
	return tok
}

skip :: #force_inline proc(p: ^Parser) {
	p.current += 1
}

peek :: proc(p: ^Parser) -> (tok: Token) {
	idx := p.current + 1
	if idx >= len(p.tokens) {
		return error_token("out of bounds")
	}
	return p.tokens[idx]
}

peek_2 :: proc(p: ^Parser) -> (tok: Token) {
	idx := p.current + 2
	if idx >= len(p.tokens) {
		return error_token("out of bounds")
	}
	return p.tokens[idx]
}

expect_token :: proc(p: ^Parser, ty: TokenType) -> Err {
	log("  Expect token ", ty)
	tok := p.tokens[p.current]
	if tok.ty == ty {
		p.current += 1
		return nil
	} else {
		return tprint("Expected token ", ty)
	}
}
accept_token :: proc(p: ^Parser, ty: TokenType) -> bool {
	if p.current >= len(p.tokens) {
		return false
	}
	is_ty := p.tokens[p.current].ty == ty
	if is_ty {
		log("Accepted Token ", ty)
		p.current += 1
	}
	return is_ty
}

expect_ident :: proc(p: ^Parser) -> (Ident, Err) {
	log("Expect Ident")
	tok := next(p)
	if tok.ty == .Ident {
		return Ident{tok.meta.string}, nil
	} else {
		return {}, "expected ident!"
	}
}
accept_ident :: proc(p: ^Parser) -> (Ident, bool) {
	if p.current >= len(p.tokens) {
		return {}, false
	}
	cur := p.tokens[p.current]
	if cur.ty == .Ident {
		log("Accepted ident: ", cur.meta.string)
		p.current += 1
		return Ident{cur.meta.string}, true
	}
	return {}, false
}

accept_left_paren_connected_to_last_token :: proc(p: ^Parser) -> bool {
	cur := p.tokens[p.current]
	accepted := cur.ty == .LeftParen && bool(cur.meta.connected_to_last_token)
	if accepted {
		log("Accepted left paren connected to last token")
		p.current += 1
	}
	return accepted
}

accept_left_bracket_connected_to_last_token :: proc(p: ^Parser) -> bool {
	cur := p.tokens[p.current]
	accepted := cur.ty == .LeftBracket && bool(cur.meta.connected_to_last_token)
	if accepted {
		log("Accepted left bracket connected to last token")
		p.current += 1
	}
	return accepted
}


accept_left_brace_connected_to_last_token :: proc(p: ^Parser) -> bool {
	cur := p.tokens[p.current]
	accepted := cur.ty == .LeftBrace && bool(cur.meta.connected_to_last_token)
	if accepted {
		log("Accepted left brace connected to last token")
		p.current += 1
	}
	return accepted
}

expect_expressions :: proc(p: ^Parser) -> (expressions: []Expression, err: Err) {
	res: [dynamic]Expression
	_expect_expressions(p, &res) or_return
	return res[:], nil
}


_expect_expressions :: #force_inline proc(
	p: ^Parser,
	expressions: ^[dynamic]Expression,
) -> (
	err: Err,
) {
	log("Expect Expressions")
	for (.CouldBeExpressionStart in TOKEN_TYPE_FLAGS[current(p).ty]) {
		expr, err := expect_expression(p)
		if err != nil {
			return err
		}
		append(expressions, expr)
	}
	return nil
}

expect_for_loop :: proc(p: ^Parser) -> (loop: ForLoop, err: Err) {
	log("Detected For Loop")
	expect_token(p, .For) or_return
	if current(p).ty == .LeftBrace {
		// for {} , infinite loop
	} else if current(p).ty == .Ident && peek(p).ty == .In {
		// for variable in iterator {}
		variable := expect_ident(p) or_return
		expect_token(p, .In) or_return
		iterator := expect_expression(p) or_return
		loop.kind = IteratorLoop{variable, iterator}
	} else {
		// for condition {}
		condition := expect_expression(p) or_return
		loop.kind = ConditionalLoop(condition)
	}
	expect_token(p, .LeftBrace) or_return
	loop.body = expect_statements(p) or_return
	expect_token(p, .RightBrace) or_return
	return loop, nil
}

expect_if_block :: proc(p: ^Parser) -> (if_block: IfBlock, err: Err) {
	log("Expect If Block")
	expect_token(p, .If) or_return
	if_block.condition = expect_expression(p) or_return
	expect_token(p, .LeftBrace) or_return
	if_block.body = expect_statements(p) or_return
	expect_token(p, .RightBrace) or_return
	if accept_token(p, .Else) {
		if current(p).ty == .If {
			// if else block incoming:
			log("    Detected Else If")
			else_if_block := expect_if_block(p) or_return
			if_block.else_block = new_clone(else_if_block)
		} else {
			log("    Detected Else")
			// else block:
			expect_token(p, .LeftBrace) or_return
			else_body := expect_statements(p) or_return
			if_block.else_block = new_clone(ElseBlock{else_body})
			expect_token(p, .RightBrace) or_return
		}
	}
	return if_block, nil
}


expression_as_assignment_place :: proc(expr: Expression) -> (place: AssignmentPlace, err: Err) {
	#partial switch place in expr.kind {
	case Ident:
		return place, nil
	case AccessOp:
		return place, nil
	case IndexOp:
		return place, nil
	}
	return {}, tprint("expected assignment place, got: ", expr)
}

expect_assignment_declaration_or_expression :: proc(
	p: ^Parser,
) -> (
	statement: Statement,
	err: Err,
) {
	first_expr := expect_expression(p) or_return
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
		if accept_token(p, check.token_ty) {
			place := expression_as_assignment_place(first_expr) or_return
			second_expr := expect_expression(p) or_return
			return Statement(Assignment{place, check.assign_kind, second_expr}), nil
		}
	}

	// maybe this is a declaration with ::, : Ty :, :=, : Ty or : Ty =  
	if current_ty == .ColonColon || current_ty == .Colon || current_ty == .ColonAssign {
		// place needs to be single ident for declaration now:
		ident: Ident = ---
		if idnt, is_idnt := first_expr.kind.(Ident); is_idnt {
			ident = idnt
		} else {
			return {}, "first expression in declaration must be ident!"
		}
		decl := Declaration {
			ident = ident,
		}
		if accept_token(p, .ColonColon) {
			decl.kind = .ConstInferred
			decl.value = expect_expression(p) or_return
		} else if accept_token(p, .ColonAssign) {
			decl.kind = .RuntimeInferred
			decl.value = expect_expression(p) or_return
		} else if accept_token(p, .Colon) {
			decl.ty = expect_expression(p) or_return
			if accept_token(p, .Colon) {
				decl.kind = .ConstExplicit
				decl.value = expect_expression(p) or_return
			} else if accept_token(p, .Assign) {
				decl.kind = .RuntimeExplicit
				decl.value = expect_expression(p) or_return
			} else {
				decl.kind = .RuntimeExplicitDefault
			}
		}
		assert(decl.ty != nil || decl.value != nil)
		return Statement(decl), nil
	}
	// just return the parsed expression, there seems to be no assignment or declaration here:
	return Statement(first_expr), nil
}

expect_statement :: proc(p: ^Parser) -> (statement: Statement, err: Err) {
	log("Expect Statement")
	current_ty := current(p).ty
	if current_ty == .Break {
		expect_token(p, .Break) or_return
		return Statement(BreakStatement{}), nil
	} else if current_ty == .Return {
		return_stmt := expect_return_statement(p) or_return
		return Statement(return_stmt), nil
	} else if current_ty == .For {
		for_loop := expect_for_loop(p) or_return
		return Statement(for_loop), nil
	} else if current_ty == .If {
		if_block := expect_if_block(p) or_return
		return Statement(if_block), nil
	} else if current_ty == .Switch {
		todo()
	} else {
		return expect_assignment_declaration_or_expression(p)
	}
}

expect_statements :: proc(p: ^Parser) -> (statements: []Statement, err: Err) {
	log("Expect Statements ------------------")
	res: [dynamic]Statement
	for (.CouldBeStatementStart in TOKEN_TYPE_FLAGS[current(p).ty]) {
		expr := expect_statement(p) or_return
		append(&res, expr)
	}
	current_ty := current(p).ty
	if .CouldBeAfterStatements not_in TOKEN_TYPE_FLAGS[current_ty] {
		return res[:], tprint("Unexpected Token after a statement: '", current_ty, "'")
	}
	log("End Statements ------------------")
	return res[:], nil
}

expect_expression :: proc(p: ^Parser) -> (expr: Expression, err: Err) {
	log("Expect Expression")
	return expect_logical_or_or_higher(p)

	expect_logical_or_or_higher :: proc(p: ^Parser) -> (expr: Expression, err: Err) {
		expr = expect_logical_and_or_higher(p) or_return
		if current(p).ty == .Or {
			next := expect_logical_or_or_higher(p) or_return
			return Expression{kind = LogicalOr{new_clone(expr), new_clone(next)}}, nil
		} else {
			return expr, nil
		}
	}

	expect_logical_and_or_higher :: proc(p: ^Parser) -> (expr: Expression, err: Err) {
		expr = expect_comparison_or_higher(p) or_return
		if current(p).ty == .And {
			next := expect_logical_and_or_higher(p) or_return
			return Expression{kind = LogicalOr{new_clone(expr), new_clone(next)}}, nil
		} else {
			return expr, nil
		}
	}

	expect_comparison_or_higher :: proc(p: ^Parser) -> (expr: Expression, err: Err) {
		first := expect_add_or_sub_or_higher(p) or_return
		if _, is_cmp := cmp_operator_token_ty_to_kind(current(p).ty); !is_cmp {
			print("is not comparision return other expr")
			return first, nil
		}

		cmp_operator_token_ty_to_kind :: proc(
			ty: TokenType,
		) -> (
			kind: ComparisonKind,
			is_cmp: bool,
		) {
			print("cmp_operator_token_ty_to_kind token ty:", ty)
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
			log("Comparition operator: ", current(p).ty, kind, is_cmp)
			next(p)
			next_expr := expect_add_or_sub_or_higher(p) or_return
			append(&others, ComparisonElement{kind, next_expr})
		}
		return Expression{kind = Comparison{first = new_clone(first), others = others[:]}}, nil
	}
	expect_add_or_sub_or_higher :: proc(p: ^Parser) -> (expr: Expression, err: Err) {
		first := expect_mul_or_div_or_higher(p) or_return
		if accept_token(p, .Add) {
			second := expect_add_or_sub_or_higher(p) or_return
			return Expression{kind = MathOp{.Add, new_clone(first), new_clone(second)}}, nil
		} else if accept_token(p, .Sub) {
			second := expect_add_or_sub_or_higher(p) or_return
			return Expression{kind = MathOp{.Sub, new_clone(first), new_clone(second)}}, nil
		}
		return first, nil
	}
	expect_mul_or_div_or_higher :: proc(p: ^Parser) -> (expr: Expression, err: Err) {
		first := expect_unary_like_or_higher(p) or_return
		if accept_token(p, .Mul) {
			second := expect_mul_or_div_or_higher(p) or_return
			return Expression{kind = MathOp{.Mul, new_clone(first), new_clone(second)}}, nil
		} else if accept_token(p, .Div) {
			second := expect_mul_or_div_or_higher(p) or_return
			return Expression{kind = MathOp{.Div, new_clone(first), new_clone(second)}}, nil
		}
		return first, nil
	}
	expect_unary_like_or_higher :: proc(p: ^Parser) -> (expr: Expression, err: Err) {
		if accept_token(p, .Sub) {
			// negative numeric expressions e.g. -2
			first := expect_unary_like_or_higher(p) or_return
			return Expression{kind = NegateExpression(new_clone(first))}, nil
		} else if accept_token(p, .Not) {
			// not operator e.g. !myfun()
			first := expect_unary_like_or_higher(p) or_return
			return Expression{kind = NotExpression(new_clone(first))}, nil
		} else {
			// single value or parens around some expression, then check if function call or indexing directly behind:

			// TODO: check for continuation of the expression via .Dot
			// - either accesses some value like so:     age := get_person(3).age
			// - or calls a function with dot noration:  10.mul(3).print()
			expr = expect_single_value(p) or_return
			if finished(p) {
				return expr, nil
			}

			for {
				if accept_token(p, .Dot) {
					ident := expect_ident(p) or_return
					parent := new_clone(expr)
					expr = Expression {
						kind = AccessOp{parent, ident},
					}
				} else if accept_left_paren_connected_to_last_token(p) {
					args := expect_expressions(p) or_return
					expect_token(p, .RightParen) or_return
					fn_expr := new_clone(expr)
					expr = Expression {
						kind = CallOp{fn_expr, args},
					}
				} else if accept_left_bracket_connected_to_last_token(p) {
					index := expect_expression(p) or_return // todo: later support multiple operators in index
					expect_token(p, .RightBracket) or_return
					place := new_clone(expr)
					expr = Expression {
						kind = IndexOp{place, new_clone(index)},
					}
				} else {
					could_be_named_struct_literal := false
					#partial switch ex in expr.kind {
					case Ident:
						could_be_named_struct_literal = true
					case AccessOp:
						could_be_named_struct_literal = true
					case CallOp:
						could_be_named_struct_literal = true
					}
					if could_be_named_struct_literal &&
					   accept_left_brace_connected_to_last_token(p) {
						struct_lit := expect_inside_of_struct_literal(p) or_return
						expect_token(p, .RightBrace) or_return
						struct_name := new_clone(expr)
						struct_lit.name = struct_name
						expr = Expression {
							kind = struct_lit,
						}
					} else {
						break
					}

				}

			}
			return expr, nil
		}
	}
	expect_single_value :: proc(p: ^Parser) -> (val: Expression, err: Err) {
		log("    Detected single value")
		if finished(p) {
			return {}, "unexpected end of parser"
		} else if .CouldBeExpressionStart not_in TOKEN_TYPE_FLAGS[current(p).ty] {
			print_tokens(p.tokens[p.current:])
			print(p.current, p.tokens[p.current])
			return {}, tprint("token is not a valid start of an expression: ", current(p).ty)
		}
		tok := next(p)
		#partial switch tok.ty {
		case .LitBool:
			return Expression{kind = LitBool{tok.meta.bool}}, nil
		case .LitInt:
			return Expression{kind = LitInt{tok.meta.int}}, nil
		case .LitFloat:
			return Expression{kind = LitFloat{tok.meta.float}}, nil
		case .LitChar:
			return Expression{kind = LitChar{tok.meta.char}}, nil
		case .LitString:
			return Expression{kind = LitString{tok.meta.string}}, nil
		case .LitNone:
			return Expression{kind = LitNone{}}, nil
		case .Ident:
			ident := Ident{tok.meta.string}
			return Expression{kind = ident}, nil
		case .Enum:
			// enum {High, Low, Mid}
			expect_token(p, .LeftBrace) or_return
			first_variant := expect_ident(p) or_return // must have at least 1 variant
			variants: [dynamic]Ident = {first_variant}
			for {
				ident, is_ident := accept_ident(p)
				if !is_ident {
					break
				}
				append(&variants, ident)
			}
			expect_token(p, .RightBrace) or_return
			return Expression{kind = EnumDecl{variants[:]}}, nil
		case .LeftParen:
			// there are 3 cases now: 
			// case 1: a function type:       ( TYPE*  ) -> TYPE
			// case 2: a function definition: ( [name: TYPE]*  ) [-> TYPE]? { STATEMENT* }
			// case 3: a paranthesized expression: (EXPRESSION)

			// case 2 always starts by IDENT and then a COLON, which can never be an expression, so this is easy to see.
			// it is only case 3, if ther is a single expression and after the `)` NO `->`. Otherwise assume this is a function type

			this := current(p)
			next := peek(p)
			if this.ty == .Ident && next.ty == .Colon {
				// parse a function definition
				// expect_function_def_arguments ()
				fun_def: FunctionDefinition
				fun_def.args = expect_function_definition_args(p) or_return
				expect_token(p, .RightParen) or_return
				// return type arrow is optional, if not present -> None is assumed.

				if accept_token(p, .Arrow) {
					return_type := expect_expression(p) or_return
					fun_def.return_type = new_clone(return_type)
				}
				expect_token(p, .LeftBrace) or_return
				fun_def.body = expect_statements(p) or_return
				expect_token(p, .RightBrace) or_return
				return Expression{kind = fun_def}, nil
			} else {
				expr := expect_expression(p) or_return
				this = current(p)
				next = peek(p)
				if this.ty == .RightParen && next.ty != .Arrow {
					// expression in parentheses
					skip(p) // skip over the closing .RightParen
					return expr, nil
				} else {
					// function type/signature
					// parse the rest of the expressions in the arg type list
					arg_types: [dynamic]Expression = {expr}
					_expect_expressions(p, &arg_types) or_return
					expect_token(p, .RightParen)
					expect_token(p, .Arrow) or_return
					return_type := expect_expression(p) or_return
					function_signature := FunctionSignature{arg_types[:], new_clone(return_type)}
					return Expression{kind = function_signature}, nil
				}
			}
			unreachable()
		case .LeftBracket:
			// [3,4,5]
			// could also be type e.g. [int] or [{s: int, f: float}] or [enum{Red, Black}]
			values := expect_expressions(p) or_return
			expect_token(p, .RightBracket) or_return
			return Expression{kind = LitArray{values}}, nil
		case .LeftBrace:
			struct_lit := expect_inside_of_struct_literal(p) or_return
			expect_token(p, .RightBrace) or_return
			return Expression{kind = struct_lit}, nil
		// { age: int, name: string} 
		// { foo: 2+4, "Hello": 34, "Noob": 40404 }
		// could also be tuple:
		// what about named struct literals?   Foo{int: i, var: "3ha"} or Foo {int: i, var: "3ha"}
		// // todo: struct 
		// we probably need to be MUCH more context sensitive if the language should work with so little syntax...
		// for example we need to say if its capitalized, its a type, if its lowercase its a variable or function and so on...
		// if it starts with underscore it is private
		}
		panic(
			"no token matched start of expression, this case should have been handled in .CouldBeExpressionStart not_in TOKEN_TYPE_FLAGS[current(p).ty]",
		)
	}
}

expect_function_definition_args :: proc(p: ^Parser) -> (args: []FunctionArg, err: Err) {
	args_list: [dynamic]FunctionArg
	for {
		if current(p).ty == .RightParen {
			break
		}
		name := expect_ident(p) or_return
		expect_token(p, .Colon) or_return
		ty := expect_expression(p) or_return
		append(&args_list, FunctionArg{name, ty})
	}
	return args_list[:], nil
}

// e.g. examples:
expect_inside_of_struct_literal :: proc(p: ^Parser) -> (lit_struct: LitStruct, err: Err) {
	fields: [dynamic]LitStructField
	for {
		if current(p).ty == .RightBrace {
			break
		}
		val_or_field_name := expect_expression(p) or_return
		if accept_token(p, .Colon) {
			// named field
			value := expect_expression(p) or_return
			append(
				&fields,
				LitStructField{name = new_clone(val_or_field_name), value = new_clone(value)},
			)
		} else {
			// unnamed field
			append(&fields, LitStructField{name = nil, value = new_clone(val_or_field_name)})
		}
	}
	return LitStruct{fields[:], nil}, nil
}


expect_return_statement :: proc(p: ^Parser) -> (return_stmt: ReturnStatement, err: Err) {
	log("Expect Return statement")
	expect_token(p, .Return) or_return
	if !finished(p) && has_flag(current(p).ty, .CouldBeExpressionStart) {
		return_stmt.value = expect_expression(p) or_return
	}
	return return_stmt, nil
}
parse :: proc(tokens: []Token) -> (mod: Module, err: Err) {
	init_token_type_flags()
	p := Parser {
		tokens  = tokens,
		current = 0,
	}
	statements := expect_statements(&p) or_return
	return Module{statements = statements}, nil
}

Module :: struct {
	statements: []Statement,
}

Expression :: struct {
	type: Type,
	kind: ExpressionKind,
}

// todo: should be struct for tracking type in type inference!
ExpressionKind :: union #no_nil {
	LogicalOr,
	LogicalAnd,
	Comparison,
	MathOp, // 583/3==4+32    a.foo[3] > - 4   a > b > c
	// other high precedence operations
	NegateExpression,
	NotExpression,
	CallOp,
	IndexOp,
	AccessOp,
	// simple expressions:
	Ident,
	LitBool,
	LitInt,
	LitFloat,
	LitString,
	LitChar,
	LitStruct, // this could be a struct definition or a value. e.g. { age: Int, name: String } vs. {age: }
	LitArray,
	FunctionSignature,
	FunctionDefinition,
	EnumDecl,
	LitUnionDecl,
	LitNone,
}

// e.g. (int, int, MyArr(string), {a: string, b: {int, int}}) -> {}
FunctionSignature :: struct {
	arg_types:   []Expression,
	return_type: ^Expression,
}

FunctionDefinition :: struct {
	args:        []FunctionArg,
	return_type: Maybe(^Expression), // if nil, this means -> None
	body:        []Statement,
}

FunctionArg :: struct {
	name: Ident,
	type: Expression,
}

LogicalAnd :: struct {
	first:  ^Expression,
	second: ^Expression,
}

LogicalOr :: struct {
	first:  ^Expression,
	second: ^Expression,
}
MathOp :: struct {
	kind:   MathOpKind,
	first:  ^Expression,
	second: ^Expression,
}
MathOpKind :: enum {
	Add,
	Sub,
	Mul,
	Div,
}
Comparison :: struct {
	first:  ^Expression,
	others: []ComparisonElement,
}
ComparisonElement :: struct {
	kind: ComparisonKind,
	expr: Expression,
}
ComparisonKind :: enum {
	NotEqual,
	Equal,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,
}

Statement :: union #no_nil {
	Assignment,
	Declaration,
	Expression,
	IfBlock,
	ForLoop,
	BreakStatement,
	ReturnStatement,
}

BreakStatement :: struct {}
ReturnStatement :: struct {
	value: Maybe(Expression),
}

IfBlock :: struct {
	condition:  Expression,
	body:       []Statement,
	else_block: union {
		// can be nil!
		^ElseBlock,
		^IfBlock,
	},
}
ElseBlock :: struct {
	body: []Statement,
}


ForLoopKind :: union {
	ConditionalLoop,
	IteratorLoop,
}
ConditionalLoop :: distinct Expression
IteratorLoop :: struct {
	variable: Ident,
	iterator: Expression,
}
ForLoop :: struct {
	kind: ForLoopKind, // can be nil, then infinite loop
	body: []Statement,
}

Assignment :: struct {
	place: AssignmentPlace,
	kind:  AssignmentKind,
	value: Expression,
}
AssignmentPlace :: union #no_nil {
	Ident,
	AccessOp,
	IndexOp,
}
AssignmentKind :: enum {
	Assign,
	AddAssign,
	SubAssign,
	MulAssign,
	DivAssign,
}
NotExpression :: distinct ^Expression
NegateExpression :: distinct ^Expression
// IdentPath :: distinct []Ident
CallOp :: struct {
	function: ^Expression,
	args:     []Expression, // todo: named args
}
IndexOp :: struct {
	place: ^Expression,
	index: ^Expression, // todo: support df["col1", "col3"] with []Expression
}
AccessOp :: struct {
	parent: ^Expression,
	ident:  Ident,
}
Ident :: struct {
	name: string,
}
LitBool :: struct {
	value: bool,
}
LitInt :: struct {
	value: i64,
}
LitFloat :: struct {
	value: f64,
}
LitString :: struct {
	value: string,
}
LitChar :: struct {
	value: rune,
}
LitNone :: struct {}

LitStruct :: struct {
	fields: []LitStructField,
	name:   Maybe(^Expression), // e.g. Foo{1,2}
	// todo: map literals in struct, e.g. {name: "Tadeo", 3: .Nice, 6: .Large}
	// for a type that is {name: string, int: MyEnum}
}
LitStructField :: struct {
	name:  Maybe(^Expression), // expression? or ident??? What about maps like { {2,3}: "Hello", {3,4}: "What"  }
	value: ^Expression,
}
LitTuple :: struct {
	values: []Expression,
}
EnumDecl :: struct {
	variants: []Ident,
}
LitUnionDecl :: struct {
	variants: []Expression,
}
LitArray :: struct {
	values: []Expression,
}

Declaration :: struct {
	ident: Ident,
	kind:  DeclarationKind,
	ty:    Maybe(Expression),
	value: Maybe(Expression),
}

DeclarationKind :: enum {
	ConstExplicit, // foo : float : 4
	ConstInferred, // foo :: 4
	RuntimeExplicit, // foo : int = 4
	RuntimeInferred, // foo :: 4
	RuntimeExplicitDefault, // foo: int
}


TokenFlags :: bit_set[TokenFlag]
TokenFlag :: enum {
	CouldBeStatementStart,
	CouldBeExpressionStart,
	IsComparisonOperator,
	CouldBeAfterStatements,
}


has_flag :: proc(ty: TokenType, flag: TokenFlag) -> bool {
	return flag in TOKEN_TYPE_FLAGS[ty]
}

TOKEN_TYPE_FLAGS: [TokenType]TokenFlags = {}
init_token_type_flags :: proc() {
	set :: proc(flag: TokenFlag, tys: []TokenType) {
		for ty in tys {
			TOKEN_TYPE_FLAGS[ty] += {flag}
		}
	}
	set(
		.CouldBeStatementStart,
		{
			.If,
			.For,
			.Switch,
			.Ident,
			.LitBool,
			.LitInt,
			.LitFloat,
			.LitString,
			.LitChar,
			.LitNone,
			.LeftParen,
			.LeftBracket,
			.LeftBrace,
			.Break,
			.Return,
		},
	)
	set(
		.CouldBeExpressionStart,
		{
			.Ident,
			.LitBool,
			.LitInt,
			.LitFloat,
			.LitString,
			.LitChar,
			.LitNone,
			.LeftParen,
			.LeftBracket,
			.LeftBrace,
			.Sub,
			.Not,
			.Enum,
		},
	)
	set(.IsComparisonOperator, {.Greater, .GreaterEqual, .Less, .LessEqual, .Equal, .NotEqual})
	set(.CouldBeAfterStatements, {.Eof, .RightBrace})
}


/*
// this stuff should be legal:

i := j := 3

21

32432 + 323


if i := print(2) {


}


foo :: () -> int | None {
    if bool.random(0.4) {
        return 19
    } else {
        return None
    }
}

for val := foo() {



}

*/
