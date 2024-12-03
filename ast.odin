package rite

import "core:fmt"
import "core:slice"


Module :: struct {
	statements: []Statement,
	tokens:     []Token,
	scope:      ^Scope,
}
Expression :: struct {
	type: Type,
	kind: ExpressionKind,
}
expression :: #force_inline proc "contextless" (kind: ExpressionKind) -> Expression {
	return Expression{nil, kind}
}

invalid_expression :: proc "contextless" (
	p: ^Parser,
	msg: string,
	token_range: TokenRange,
) -> Expression {
	return Expression{nil, _invalid_expression(p, msg, token_range)}
}
_invalid_expression :: proc "contextless" (
	p: ^Parser,
	msg: string,
	token_range: TokenRange,
) -> InvalidExpression {
	return InvalidExpression{msg, token_range, p.tokens[token_range.start_idx:token_range.end_idx]}
}

// todo: should be struct for tracking type in type inference!
ExpressionKind :: union #no_nil {
	InvalidExpression,
	LogicalOr,
	LogicalAnd,
	Comparison,
	MathOp,
	NegateExpression,
	NotExpression,
	CallOp,
	IndexOp,
	AccessOp,
	Ident,
	PrimitiveTypeIdent,
	PrimitiveLiteral,
	StructLiteral,
	ArrayLiteral,
	MapLiteral,
	FunctionSignature,
	FunctionDefinition,
	EnumDecl,
	UnionDecl,
}
INVALID_EXPRESSION :: InvalidExpression{}
InvalidExpression :: struct {
	msg:          string,
	tokens:       TokenRange,
	tokens_slice: []Token,
}
expression_valid :: proc(ex: Expression) -> bool {
	if _, invalid := ex.kind.(InvalidExpression); invalid {
		return false
	} else {
		return true
	}
}
// e.g. (int, int, MyArr(string), {a: string, b: {int, int}}) -> {}
FunctionSignature :: struct {
	arg_types:   []Expression,
	return_type: ^Expression,
}

// (i: int, j: string) -> Foo { print(i+j) return Foo{j,j} }
FunctionDefinition :: struct {
	args:                  []FunctionArg,
	return_type:           Maybe(^Expression), // if nil, this means -> None
	body:                  []Statement,
	paren_token_start_idx: int,
	scope:                 ^Scope,
}

FunctionArg :: struct {
	name: Ident,
	ty:   Expression,
	// default_value: Expression
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
math_op_expr :: proc(kind: MathOpKind, first: Expression, second: Expression) -> Expression {
	return Expression{type = nil, kind = MathOp{kind, new_clone(first), new_clone(second)}}
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
	IfStatement,
	ForStatement,
	BreakStatement,
	ReturnStatement,
}
BreakStatement :: struct {
	break_token_idx: int,
}
ReturnStatement :: struct {
	return_token_idx: int,
	value:            Maybe(Expression),
}
IfStatement :: struct {
	condition:  Expression,
	body:       []Statement,
	else_block: union {
		// can be nil!
		ElseBlock,
		^IfStatement,
	},
}
ElseBlock :: struct {
	body: []Statement,
}
ForLoopKind :: union {
	// nil means a forever loop
	ConditionalLoop,
	IteratorLoop,
}
ConditionalLoop :: struct {
	condition: Expression,
}
IteratorLoop :: struct {
	variable: Ident,
	iterator: Expression,
}
ForStatement :: struct {
	kind:          ForLoopKind, // can be nil, then infinite loop
	body:          []Statement,
	for_token_idx: int,
}

Assignment :: struct {
	place: Expression,
	kind:  AssignmentKind,
	value: Expression,
}
// AssignmentPlace :: union #no_nil {
// 	Ident,
// 	AccessOp,
// 	IndexOp,
// }
AssignmentKind :: enum {
	Assign,
	AddAssign,
	SubAssign,
	MulAssign,
	DivAssign,
}
NotExpression :: struct {
	inner: ^Expression,
}
NegateExpression :: struct {
	inner: ^Expression,
}
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
	name:      string,
	token_idx: int,
}
PrimitiveLiteral :: struct {
	value:     PrimitiveValue,
	token_idx: int,
}
PrimitiveValue :: struct {
	type: PrimitiveType,
	data: struct #raw_union {
		int:    i64,
		float:  f64,
		string: string,
		bool:   bool,
		char:   rune,
	},
}
PrimitiveTypeIdent :: struct {
	value:     PrimitiveType,
	token_idx: int,
}


// valid struct literals:
// Foo{ .. }          or   { .. }
// Foo{ a: 3, .. }    or   { a: 3, .. }
// Foo{ 3, 4, .. }    or   { 3, 4, .. }
// Foo{ 3, 4 }        or   { 3, 4 }
// Foo{ a: 3, b: 4 }  or   { a: 3, b: 4 }
// { a: 3, b: 4 }
StructLiteral :: struct {
	fields:                  StructFields,
	name_or_brace_token_idx: union #no_nil {
		int,
		^Expression,
	},
	// ellipsis + ellipsis value
	// ellipsises: []Expression
	// has_all_ellipsis: bool
}
StructFields :: union {
	// nil: means no fields in this struct
	[]NamedField,
	[]Expression,
}
NamedField :: struct {
	name:  Ident, // expression? or ident??? What about maps like [{2,3}: "Hello", {3,4}: "What"]
	value: Expression,
}
EnumDecl :: struct {
	variants:       []Ident,
	enum_token_idx: int,
}
UnionDecl :: struct {
	variants: []Expression,
}
ArrayLiteral :: struct {
	values:                  []Expression,
	start_bracket_token_idx: int,
}
MapLiteral :: struct {
	entries: []MapEntry,
}
MapEntry :: struct {
	key:   Expression,
	value: Expression,
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
}
has_flag :: proc(ty: TokenType, flag: TokenFlag) -> bool {
	return flag in TOKEN_TYPE_FLAGS[ty]
}

TOKEN_TYPE_FLAGS: [TokenType]TokenFlags = init_token_type_flags()
init_token_type_flags :: proc() -> (flags: [TokenType]TokenFlags) {
	set :: proc(flags: ^[TokenType]TokenFlags, flag: TokenFlag, tys: []TokenType) {
		for ty in tys {
			flags[ty] += {flag}
		}
	}
	set(
		&flags,
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
			.PrimitiveType,
			.LeftParen,
			.LeftBracket,
			.LeftBrace,
			.Break,
			.Return,
		},
	)
	set(
		&flags,
		.CouldBeExpressionStart,
		{
			.Ident,
			.LitBool,
			.LitInt,
			.LitFloat,
			.LitString,
			.LitChar,
			.PrimitiveType,
			.LeftParen,
			.LeftBracket,
			.LeftBrace,
			.Sub,
			.Not,
			.Enum,
		},
	)
	set(
		&flags,
		.IsComparisonOperator,
		{.Greater, .GreaterEqual, .Less, .LessEqual, .Equal, .NotEqual},
	)
	return flags
}


TokenRange :: struct {
	start_idx: int,
	end_idx:   int,
}
// we try to store a TokenRange only with atomic expressions, i.e. literals and idents.
// the ranges for other expressions can be computed instead, 
// supposed that the AST is made of the exact tokens that are expected to form e.g. an if statement
// this should be the case, because otherwise the parse would not have succeeded anyway.
expression_token_range :: proc(expr: Expression) -> TokenRange {
	_from_to :: proc(first: ^Expression, second: ^Expression) -> TokenRange {
		return TokenRange {
			expression_token_range(first^).start_idx,
			expression_token_range(second^).end_idx,
		}
	}

	switch ex in expr.kind {
	case InvalidExpression:
		return ex.tokens
	case LogicalOr:
		return _from_to(ex.first, ex.second)
	case LogicalAnd:
		return _from_to(ex.first, ex.second)
	case Comparison:
		assert(len(ex.others) > 0)
		return _from_to(ex.first, &ex.others[len(ex.others) - 1].expr)
	case MathOp:
		return _from_to(ex.first, ex.second)
	case NegateExpression:
		inner := expression_token_range(ex.inner^)
		return {inner.start_idx - 1, inner.end_idx}
	case NotExpression:
		inner := expression_token_range(ex.inner^)
		return {inner.start_idx - 1, inner.end_idx}
	case CallOp:
		fn_name := expression_token_range(ex.function^)
		if len(ex.args) == 0 {
			return TokenRange{fn_name.start_idx, fn_name.end_idx + 2} // e.g. bar.foo()
		} else {
			last_arg_end := expression_token_range(ex.args[len(ex.args) - 1]).end_idx
			return TokenRange{fn_name.start_idx, last_arg_end + 1} // e.g. bar.foo()

		}
	case IndexOp:
		range := _from_to(ex.place, ex.index)
		return TokenRange{range.start_idx, range.end_idx + 1} // e.g my_arr[3+4]
	case AccessOp:
		parent := expression_token_range(ex.parent^)
		return TokenRange{parent.start_idx, ex.ident.token_idx + 1}
	case Ident:
		return TokenRange{ex.token_idx, ex.token_idx + 1}
	case PrimitiveLiteral:
		return TokenRange{ex.token_idx, ex.token_idx + 1}
	case PrimitiveTypeIdent:
		return TokenRange{ex.token_idx, ex.token_idx + 1}
	case StructLiteral:
		range: TokenRange
		switch name_or_idx in ex.name_or_brace_token_idx {
		case int:
			range = TokenRange{name_or_idx, name_or_idx + 2} // in case no fields and ellipsis, skip over `{ }`
		case ^Expression:
			name_range := expression_token_range(name_or_idx^)
			range = TokenRange{name_range.start_idx, name_range.end_idx + 2} // in case no fields and ellipsis, skip over `{ }`
		}

		switch fields in ex.fields {
		case nil:
		// already handled above, with +2 skipping over `{ }` tokens
		case []NamedField:
			assert(len(fields) > 0)
			last_field_range := expression_token_range(fields[len(fields) - 1].value)
			range.end_idx = last_field_range.end_idx + 1
		case []Expression:
			assert(len(fields) > 0)
			last_field_range := expression_token_range(fields[len(fields) - 1])
			range.end_idx = last_field_range.end_idx + 1
		}
		// todo: ellipsis support behind fields
		return range
	case ArrayLiteral:
		range := TokenRange{ex.start_bracket_token_idx, 0}
		if len(ex.values) > 0 {
			range.end_idx = expression_token_range(ex.values[len(ex.values) - 1]).end_idx + 1
		} else {
			range.end_idx = range.start_idx + 2
		}
		return range
	case MapLiteral:
		assert(len(ex.entries) > 0)
		first_key := expression_token_range(ex.entries[0].key)
		last_val := expression_token_range(ex.entries[len(ex.entries) - 1].value)
		return TokenRange{first_key.start_idx - 1, last_val.end_idx + 1} // including the `[` and `]` around the expressions inside
	case FunctionSignature:
		range := expression_token_range(ex.return_type^)
		if len(ex.arg_types) > 0 {
			range.start_idx =
				expression_token_range(ex.arg_types[len(ex.arg_types) - 1]).start_idx - 1
		} else {
			range.start_idx -= 3 // skipping over `( ) ->` like in `() -> int` 
		}
		return range
	case FunctionDefinition:
		range: TokenRange = {ex.paren_token_start_idx, 0}
		// figure out end_idx:
		if len(ex.body) > 0 {
			range.end_idx = statement_token_range(ex.body[len(ex.body) - 1]).end_idx + 1
		} else if ret_ty, ok := ex.return_type.(^Expression); ok {
			range.end_idx = expression_token_range(ret_ty^).end_idx + 2 // skip over {}, e.g. in `(i: int) -> None {}`
		} else if len(ex.args) > 0 {
			range.start_idx = expression_token_range(ex.args[len(ex.args) - 1].ty).end_idx + 3 // skip over `) {}` like in (foo: Foo) {}
		} else {
			range.end_idx = range.start_idx + 4 // i.e `() { }`, skip over the 4 tokens
		}
		return range
	case EnumDecl:
		range: TokenRange = {ex.enum_token_idx, 0}
		if len(ex.variants) > 0 {
			range.end_idx = ex.variants[len(ex.variants) - 1].token_idx + 2
		} else {
			range.end_idx = ex.enum_token_idx + 3 // i.e `enum {}`
		}
	case UnionDecl:
		todo()
	}
	unreachable()
}


statement_tokens_as_string :: proc(stmt: Statement, all_tokens: []Token) -> string {
	return tokens_as_code(statement_tokens(stmt, all_tokens))
}
statement_tokens :: proc(stmt: Statement, all_tokens: []Token) -> []Token {
	range := statement_token_range(stmt)
	return all_tokens[range.start_idx:range.end_idx]
}
statement_token_range :: proc(stmt: Statement) -> TokenRange {
	switch st in stmt {
	case Assignment:
		return TokenRange {
			expression_token_range(st.place).start_idx,
			expression_token_range(st.value).end_idx,
		}
	case Declaration:
		range := TokenRange{st.ident.token_idx, 0}
		if value, v_ok := st.value.(Expression); v_ok {
			range.end_idx = expression_token_range(value).end_idx
		} else if ty, ty_ok := st.ty.(Expression); ty_ok {
			range.end_idx = expression_token_range(ty).end_idx
		} else {
			panic("a declaration should have the type and/or value specified.")
		}
		return range
	case Expression:
		return expression_token_range(st)
	case IfStatement:
		return if_statement_token_range(st)
	case ForStatement:
		range: TokenRange = {st.for_token_idx, 0}
		if len(st.body) > 0 {
			range.end_idx = statement_token_range(st.body[len(st.body) - 1]).end_idx + 1 // skip over `}`
		} else {
			switch kind in st.kind {
			case nil:
				range.end_idx = st.for_token_idx + 3 // skip over `for { }`
			case ConditionalLoop:
				range.end_idx = expression_token_range(kind.condition).end_idx + 2
			case IteratorLoop:
				range.end_idx = expression_token_range(kind.iterator).end_idx + 2
			}
		}
		return range
	case BreakStatement:
		return TokenRange{st.break_token_idx, st.break_token_idx + 1}
	case ReturnStatement:
		range := TokenRange{st.return_token_idx, 0}
		if value, ok := st.value.(Expression); ok {
			range.end_idx = expression_token_range(value).end_idx
		} else {
			range.end_idx = range.start_idx + 1
		}
		return range
	}
	unreachable()
}

if_statement_token_range :: proc(st: IfStatement) -> TokenRange {
	condition_range := expression_token_range(st.condition)
	range := TokenRange{condition_range.start_idx - 1, 0}
	switch el in st.else_block {
	case ElseBlock:
		if len(el.body) > 0 {
			// eg. if a is int {} else { print("not int") }
			range.end_idx = statement_token_range(el.body[len(el.body) - 1]).end_idx + 1 // skipping `}`
		} else if len(st.body) > 0 {
			// e.g. if a == 2 { print("Hello") } else {}
			range.end_idx = statement_token_range(st.body[len(st.body) - 1]).end_idx + 4 // skipping `} else {}`
		} else {
			// e.g. if a == 2 {} else {}
			range.end_idx = condition_range.end_idx + 5
		}
	case ^IfStatement:
		range.end_idx = if_statement_token_range(el^).end_idx
	case nil:
		if len(st.body) > 0 {
			// eg. if a is int { print("not int") }
			range.end_idx = statement_token_range(st.body[len(st.body) - 1]).end_idx + 1 // skipping `}`
		} else {
			// e.g. `if a == 2 {}`
			range.end_idx = condition_range.end_idx + 2
		}
	}
	assert(range.end_idx != 0)
	return range
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
