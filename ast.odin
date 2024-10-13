package rite


Parser :: struct {
	tokens:  []Token,
	current: int,
}
finished :: proc(p: ^Parser) -> bool {
	return p.current < len(p.tokens)
}

next :: proc(p: ^Parser) -> (tok: Token) {
	tok = p.tokens[p.current]
	p.current += 1
	return tok
}

current :: proc(p: ^Parser) -> (tok: Token) {
	return p.tokens[p.current]
}


expect_token :: proc(p: ^Parser, ty: TokenType) -> bool {
	tok := next(p)
	return tok.ty == ty
}

accept_token :: proc(p: ^Parser, ty: TokenType) -> bool {
	is_ty := p.tokens[p.current].ty == ty
	if is_ty {
		p.current += 1
	}
	return is_ty
}

parse_ident :: proc(p: ^Parser) -> (ident: Ident, ok: bool) {
	tok := next(p)
	if tok.ty == .Ident {
		return Ident{tok.meta.string}, true
	} else {
		return {}, false
	}
}


expect_expressions :: proc(p: ^Parser) -> (expressions: []Expression, ok: bool) {
	res: [dynamic]Expression
	for !finished(p) {
		expr := expect_expression(p) or_return
		append(&res, expr)
	}
	return res[:], true
}

expect_for_loop :: proc(p: ^Parser) -> (loop: ForLoop, ok: bool) {
	expect_token(p, .For) or_return
	if accept_token(p, .LeftBrace) {
		// infinite loop without condition
		loop.body = expect_statements(p) or_return
		expect_token(p, .RightBrace) or_return
	} else {
		loop.condition = expect_expression(p) or_return
		expect_token(p, .LeftBrace) or_return
		loop.body = expect_statements(p) or_return
		expect_token(p, .RightBrace) or_return
	}
	return loop, true
}

expect_if_block :: proc(p: ^Parser) -> (if_block: IfBlock, ok: bool) {
	expect_token(p, .If) or_return
	if_block.condition = expect_expression(p) or_return
	expect_token(p, .LeftBrace) or_return
	if_block.body = expect_statements(p) or_return
	expect_token(p, .RightBrace) or_return
	if accept_token(p, .Else) {
		if current(p).ty == .If {
			// if else block incoming:
			else_if_block := expect_if_block(p) or_return
			if_block.else_block = new_clone(else_if_block)
		} else {
			// else block:
			expect_token(p, .LeftBrace) or_return
			else_body := expect_statements(p) or_return
			if_block.else_block = new_clone(ElseBlock{else_body})
			expect_token(p, .RightBrace) or_return
		}
	}
	return if_block, true
}


expression_as_place :: proc(expr: Expression) -> (place: Place, ok: bool) {
	if single, is_single := expr.(SingleValue); is_single {
		if place, ok = single.(Place); ok {
			return place, true
		}
	}
	return {}, false
}

expect_assignment_declaration_or_expression :: proc(
	p: ^Parser,
) -> (
	statement: Statement,
	ok: bool,
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
			place := expression_as_place(first_expr) or_return
			second_expr := expect_expression(p) or_return
			return Statement(Assignment{place, check.assign_kind, second_expr}), true
		}
	}

	// maybe this is a declaration with ::, : Ty :, :=, : Ty or : Ty =  
	if current_ty == .ColonColon || current_ty == .Colon || current_ty == .ColonAssign {
		// place needs to be single ident for declaration now:
		place := expression_as_place(first_expr) or_return
		if place.indexed != nil || len(place.path) != 1 {
			return {}, false
		}
		decl := Declaration {
			ident = place.path[0],
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
			} else if accept_token(p, .Equal) {
				decl.kind = .RuntimeExplicit
				decl.value = expect_expression(p) or_return
			} else {
				decl.kind = .RuntimeExplicitDefault
			}
		}
		assert(decl.ty != nil || decl.value != nil)
		return Statement(decl), true
	}
	// just return the parsed expression, there seems to be no assignment or declaration here:
	return Statement(first_expr), true
}

expect_statement :: proc(p: ^Parser) -> (statement: Statement, ok: bool) {
	current_ty := current(p).ty
	if current_ty == .Break {
		return Statement(BreakStatement{}), true
	} else if current_ty == .Return {
		return_stmt := expect_return_statement(p) or_return
		return Statement(return_stmt), true
	} else if current_ty == .For {
		for_loop := expect_for_loop(p) or_return
		return Statement(for_loop), true
	} else if current_ty == .If {
		if_block := expect_if_block(p) or_return
		return Statement(if_block), true
	} else if current_ty == .Switch {
		todo()
	} else {
		return expect_assignment_declaration_or_expression(p)
	}
}

expect_statements :: proc(p: ^Parser) -> (statements: []Statement, ok: bool) {
	res: [dynamic]Statement
	for finished(p) || !could_be_statement_start(current(p).ty) {
		expr := expect_statement(p) or_return
		append(&res, expr)
	}
	return res[:], true
}

could_be_statement_start :: proc(ty: TokenType) -> bool {
	#partial switch ty {
	case .If,
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
	     .Dot:
		// Dot for enum variant   .Nice
		return true
	}
	return false
}

could_be_expression_start :: proc(ty: TokenType) -> bool {
	#partial switch ty {
	case .Ident,
	     .LitBool,
	     .LitInt,
	     .LitFloat,
	     .LitString,
	     .LitChar,
	     .LitNone,
	     .LeftParen,
	     .LeftBracket,
	     .LeftBrace,
	     .Dot:
		// Dot for enum variant   .Nice
		return true
	}
	return false
}
expect_expression :: proc(p: ^Parser) -> (expr: Expression, ok: bool) {
	if finished(p) || !could_be_expression_start(current(p).ty) {
		return {}, false
	}

	// if comparison := parse_comparison(p); ok {
	// 	// this is retarded, we cannot do parse_comparison to find an == somewhere and then go back the entire tree in case we dont find it...
	// 	// we need a smarter parser here..
	// }

	// parse_comparison :: !

	return {}, false
}

expect_return_statement :: proc(p: ^Parser) -> (return_stmt: ReturnStatement, ok: bool) {
	expect_token(p, .Return) or_return
	if !finished(p) && could_be_expression_start(current(p).ty) {
		return_stmt.value = expect_expression(p) or_return
	}
	return return_stmt, true
}
parse :: proc(tokens: []Token) -> (mod: Module, ok: bool) {
	p := Parser {
		tokens  = tokens,
		current = 0,
	}
	statements := expect_statements(&p) or_return
	return Module{statements = statements}, true
}

Module :: struct {
	statements: []Statement,
}

// todo: should be struct for tracking type in type inference!
Expression :: union #no_nil {
	Comparison, // 583/3==4+32    a.foo[3] > - 4
	Addition, // 4*9+3-1234/6
	Multiplication, // 54.foo() * 4030 / 9
	Unary, // !true      ,-(a+4)
	SingleValue, // a,   ident.foo.bar[3],    foo["Hello"], "Hello", 320, 54.3, 's', true, {4,8,9}, [4,7,9, foo]
}

Comparison :: struct {
	kind:     MultiplicationKind,
	elements: []Expression,
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
	value: Expression,
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

ForLoop :: struct {
	condition: Maybe(Expression), // if nil: infinite loop
	body:      []Statement,
}

Assignment :: struct {
	place: Place,
	kind:  AssignmentKind,
	value: Expression,
}
AssignmentKind :: enum {
	Assign,
	AddAssign,
	SubAssign,
	MulAssign,
	DivAssign,
}
Addition :: struct {
	kind:     AdditionKind,
	elements: []Expression,
}
AdditionKind :: enum {
	Add,
	Sub,
}
Multiplication :: struct {
	kind:     MultiplicationKind,
	elements: []Expression,
}
MultiplicationKind :: enum {
	Mul,
	Div,
}
Unary :: struct {
	expr: ^Expression,
	kind: UnaryKind,
}
UnaryKind :: enum {
	Minus,
	Not,
}


SingleValue :: union #no_nil {
	Call,
	Place,
	LitBool,
	LitInt,
	LitFloat,
	LitString,
	LitChar,
	LitStruct,
	LitArray,
	LitEnumDecl,
	LitUnionDecl,
}
Place :: struct {
	path:    []Ident, //
	indexed: []Expression, // e.g. foo.bar[5,5+3]
}
Call :: struct {
	fn_name: Ident,
	args:    []Expression,
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
LitStruct :: struct {
	fields: []Expression,
	// todo: map literals in struct, e.g. {name: "Tadeo", 3: .Nice, 6: .Large}
}
Field :: struct {
	name:  Ident,
	value: Expression,
}
LitTuple :: struct {
	values: []Expression,
}
LitEnumDecl :: struct {
	variants: []Ident,
}
LitUnionDecl :: struct {
	variants: []Expression,
}
LitEnum :: struct {
	// .Large
	variant: Ident,
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
