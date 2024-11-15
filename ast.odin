package rite

import "core:fmt"
import "core:slice"

TokenRange :: struct {
	start_idx: int,
	end_idx:   int,
}
Module :: struct {
	statements: []Statement,
}
Expression :: struct {
	type:  Type,
	kind:  ExpressionKind,
	range: TokenRange,
}
expression :: proc "contextless" (
	kind: ExpressionKind,
	start_token_idx: int,
	end_token_idx: int,
) -> Expression {
	return Expression{nil, kind, TokenRange{start_token_idx, end_token_idx}}
}

invalid_expression :: proc "contextless" (
	start_token_idx: int,
	end_token_idx: int,
	msg := "",
) -> Expression {
	return Expression {
		nil,
		InvalidExpression{msg = msg},
		TokenRange{start_token_idx, end_token_idx},
	}
}

// todo: should be struct for tracking type in type inference!
ExpressionKind :: union #no_nil {
	InvalidExpression,
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
INVALID_EXPRESSION :: InvalidExpression{}
InvalidExpression :: struct {
	msg: string,
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
math_op_expr :: proc(kind: MathOpKind, first: Expression, second: Expression) -> Expression {
	return Expression {
		type = nil,
		kind = MathOp{kind, new_clone(first), new_clone(second)},
		range = TokenRange{first.range.start_idx, second.range.end_idx},
	}
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
BreakStatement :: struct {}
ReturnStatement :: struct {
	value: Maybe(Expression),
}
IfStatement :: struct {
	condition:  Expression,
	body:       []Statement,
	else_block: union {
		// can be nil!
		^ElseBlock,
		^IfStatement,
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
ForStatement :: struct {
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
			.LitNone,
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
			.LitNone,
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
	set(&flags, .CouldBeAfterStatements, {.Eof, .RightBrace})
	return flags
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
