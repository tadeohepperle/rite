package rite
import "base:runtime"
import "core:debug/trace"
import "core:log"
import "core:strings"
import "core:testing"

main :: proc() {
	// context.logger = log.create_console_logger()
	// expr := expression_from_string("4 + 5 * {1,2,3} + 5")

	SOURCE := `
	Foo :: {int, Bar}
	if true { print("Hello") } else {
		(3 - 20) * (3 - 18)
	}
	



	Bar :: {i : string, j : [int], tyty: make_ty() }
	make_ty :: (i: int) -> Type {  }
	main ::  () { foo()  bar() 
	
		Foo :: {int, Bar}
	
	}
	
	A :: 3 + C
	B :: 302 + C
	C :: 3232

	// add :: (i: int, j: int) -> int {
	// 	return i + j
	// }
	
	// add :: (foo: Foo, j: int) -> make_ty(323) {
	// 	return foo.x + j
	// }

	// add :: (bar: Bar) -> Bar {
	// 	return bar
	// }

	`


	print(SOURCE)
	mod, tokens := module_from_string(SOURCE)
	print(module_to_string(mod))
	// print(tokens)
	errors := errors_create(SOURCE, tokens)
	type_check(&mod, &errors)
	errors_print(errors)
	// parse_expressions_test(nil)
}


@(test)
parse_expressions_test :: proc(t: ^testing.T) {
	Case :: struct {
		source: string,
		expr:   Expression,
	}
	TEST_EXPRESSIONS := []Case {
		Case{"7 + 3 * 2", add(i(7), mul(i(3), i(2)))},
		Case {
			"(i: int, j: float) -> None {}",
			fn_def({{ident("i"), id("int")}, {ident("j"), id("float")}}, return_ty = ty(.None)),
		},
		Case{"(i: int, j: float){}", fn_def({{ident("i"), id("int")}, {ident("j"), id("float")}})},
		Case {
			"(int, int, Foo) -> string",
			fn_signature({id("int"), id("int"), id("Foo")}, id("string")),
		},
		Case{"foo(2, 3)", fn_call(id("foo"), {i(2), i(3)})},
		Case{"{int, string, Foo}", tuple({id("int"), id("string"), id("Foo")})},
		Case {
			"{foo: 3, bar: \"Hello\"}",
			record({{ident("foo"), i(3)}, {ident("bar"), s("Hello")}}),
		},
		Case {
			"{bar: {1 3 true}, arr_of_floats: [2.0,3.2] }",
			record(
				{
					NamedField{ident("bar"), tuple({i(1), i(3), b(true)})},
					NamedField{ident("arr_of_floats"), array({f(2.0), f(3.2)})},
				},
			),
		},
		Case {
			`() {
				print("Hello") * 99
			}`,
			fn_def({}, nil, []Statement{mul(fn_call(id("print"), {s("Hello")}), i(99))}),
		},
	}

	for test_case in TEST_EXPRESSIONS {
		expr := expression_from_string(test_case.source)
		testing.expect(
			t,
			expression_eq(expr, test_case.expr),
			tprint("expressions not equal. Expected: ", test_case.expr, "\n Parsed: ", expr),
		)
	}

	i :: proc(i: int) -> Expression {
		return expression(PrimitiveLiteral{{.Int, {int = i64(i)}}, 0})
	}
	f :: proc(f: float) -> Expression {
		return expression(PrimitiveLiteral{{.Float, {float = f}}, 0})
	}
	s :: proc(s: string) -> Expression {
		return expression(PrimitiveLiteral{{.String, {string = s}}, 0})
	}
	b :: proc(b: bool) -> Expression {
		return expression(PrimitiveLiteral{{.Bool, {bool = b}}, 0})
	}
	ty :: proc(prim: PrimitiveType) -> Expression {
		return expression(PrimitiveTypeIdent{prim, 0})
	}
	add :: proc(a: Expression, b: Expression) -> Expression {
		return expression(MathOp{.Add, new_clone(a), new_clone(b)})
	}
	sub :: proc(a: Expression, b: Expression) -> Expression {
		return expression(MathOp{.Sub, new_clone(a), new_clone(b)})
	}
	mul :: proc(a: Expression, b: Expression) -> Expression {
		return expression(MathOp{.Mul, new_clone(a), new_clone(b)})
	}
	div :: proc(a: Expression, b: Expression) -> Expression {
		return expression(MathOp{.Div, new_clone(a), new_clone(b)})
	}
	assign :: proc(place: Expression, kind: AssignmentKind, value: Expression) -> Statement {
		return Statement{}
	}
	decl :: proc(name: Ident) -> Statement {
		return Declaration{}
	}
	_if :: proc(condition: Expression, then: []Statement) -> Statement {
		return IfStatement{condition, then, nil}
	}
	_if_else :: proc(condition: Expression, then: []Statement, _else: []Statement) -> Statement {
		return IfStatement{condition, then, ElseBlock{_else}}
	}
	ident :: proc(name: string) -> Ident {
		return Ident{name, 0}
	}
	id :: proc(name: string) -> Expression {
		return expression(Ident{name, 0})
	}
	tuple :: proc(fields: []Expression, name: Maybe(string) = nil) -> Expression {
		lit: StructLiteral
		lit.name_or_brace_token_idx = 0
		if name, ok := name.(string); ok {
			lit.name_or_brace_token_idx = new_clone(expression(Ident{name, 0}))
		}
		lit.fields = fields
		return expression(lit)
	}
	record :: proc(fields: []NamedField, name: Maybe(string) = nil) -> Expression {
		lit: StructLiteral
		lit.name_or_brace_token_idx = 0
		if name, ok := name.(string); ok {
			lit.name_or_brace_token_idx = new_clone(expression(Ident{name, 0}))
		}
		lit.fields = fields
		return expression(lit)
	}
	fn_signature :: proc(arg_types: []Expression, return_type: Expression) -> Expression {
		return expression(FunctionSignature{arg_types, new_clone(return_type)})
	}
	fn_def :: proc(
		args: []FunctionArg,
		return_ty: Maybe(Expression) = nil,
		body: []Statement = nil,
	) -> Expression {
		ret: Maybe(^Expression)
		if r, ok := return_ty.(Expression); ok {
			ret = new_clone(r)
		}
		return expression(FunctionDefinition{args, ret, body, 0, nil})
	}
	fn_call :: proc(fn: Expression, args: []Expression) -> Expression {
		return expression(CallOp{new_clone(fn), args})
	}
	array :: proc(values: []Expression) -> Expression {
		return expression(ArrayLiteral{values, 0})
	}
	_map :: proc(values: []MapEntry) -> Expression {
		return expression(MapLiteral{values})
	}
}


@(test)
valid_expressions_test :: proc(t: ^testing.T) {
	ok_expressions := []string{`4`, `4+6`, `"hello"`}
	bad_expressions := []string{`:3`}
	for ok in ok_expressions {
		ex := expression_from_string(ok)
		testing.expectf(
			t,
			expression_valid(ex),
			"parser said expression %s is invalid",
			expression_to_string(ex),
		)
	}
	for bad in bad_expressions {
		ex := expression_from_string(bad)
		testing.expectf(
			t,
			!expression_valid(ex),
			"parser said expression %s is valid but it isnt",
			expression_to_string(ex),
		)
	}
}
@(test)
tokenize_test :: proc(t: ^testing.T) {
	Pair :: struct {
		source: string,
		tokens: []Token,
	}
	test_cases: []Pair = {
		{"hello", {ident("hello")}},
		{"1 3 4", {lit_int(1), lit_int(3), lit_int(4)}},
		{"A :: 3+ 4", {ident("A"), token(.ColonColon), lit_int(3), token(.Add), lit_int(4)}},
	}

	for c in test_cases {
		tokens, err := tokenize(c.source)
		test_expect_tokens_equal(t, tokens[:], c.tokens)
	}
}


// tokenize_test :: proc() {
// 	source := `
// 	Article :: {
// 		authors: [string],
// 		date: Date = {year: 2023, month: 7, day: 28},
// 	}


// 	MyU :: int | string | Article
// 	u: MyU = 3
// 	articles: [Article]
// 	switch u {
// 		case int:
// 			u = "Hello" // error because u is a refererence to an int already
// 		case string:
// 			u.len().print()
// 		case Article:
// 			articles.push(u)
// 	}

// 	`


// 	tokens, err := tokenize(source)
// 	if err, is_err := err.(string); is_err {
// 		print("Error: ", err)
// 		print_tokens_as_code(tokens[:])
// 	} else {
// 		print("Source:")
// 		print(source)
// 		print("Tokenized:")
// 		print_tokens(tokens[:])
// 		// print_tokens(tokens[:])
// 	}

// }


// tokens, terr := tokenize(source)
// assert(terr == nil)
// mod, errors := parse(tokens[:])
// if errors != nil {
// 	print(len(errors), " errors:")
// 	for e in errors {
// 		print("    ", e)
// 	}
// } else {
// 	print("Module:")
// 	print(module_to_string(mod))
// }

/*
	

// working:
a := 3
for a <=4 {
	print(34*a)
	a += 1
	if a ==2 { break }
}
if true {
	a = 4
	3 + 4 -a
} else if true {
	print("hello")
	print("nice")
	foo.bar.zoom(2,3,4.4)("Wtf")
} else {
	3 + 5
}
3 + 4 + 6

foo = 7-6*2+89/11

FOO :: enum {Black, Red}
BAR := 3


// also okay:
	my_val := {hello: 3 + 45 * 2, string: int}

		City :: enum {Ohio, Indiana, Colorado}
		Person :: {age: int, name: string, {string,string}: City}

		my_var: {age: int, city: City}

		a:= {"Hello", "you", "are"}
		a:= ["Hello", "you", "are"]

		i:= 10
		for i < 30 {
			// i.to_float().print()
			i +=3
			for j < 400 {
				print("Hello", 12, "nice")
			}
			if true {
				a = 4
				3 + 4 -a
			} else if true {
				print("hello")
				print("nice")
				foo.bar.zoom(2,3,4.4)("Wtf")
			} else {
				3 + 5
			}
		}

		i:= None

	print({"Hello"}, 1, 4)
	print({x: 4}, 4) 
	// foo :: () {
	// 	print("Hello")
	// }
	a:= b.foo.bar
	b:= (b.foo).bar
	// c:= b.(foo.bar) // should error!

	next : {age: int} = foo.arr[3].bar.zaa()
	a: int = 3 



	fn_list :: [
		(i: int) { print(i) }
		(i: int) { print(i) print(i+1)}
		(i: int) { print(i) print(i+2)}

		(int) -> string
	]

	MyType :: (int) -> string
	MyOtherFunctionType :: (int) -> int

	FunctionTuples :: {(int) -> string (int) -> int}

	// function type literal:
	A :: (int, t: t) -> int // lets 
	A :: (3 + 3)

	// if we encounter an expression in parens, it could just be normal parens round an expression,
	or a function signature
	// we probably only find out, if, immediately after parens close there is the arrow sign

	


*/

// main :: proc() {
// 	source := `

// 		A :  (int, int) -> string  : (a: int, b: int) -> string {
// 			print("boohoo")
// 			"boohoo".print()
// 			 return a + b * c + 3
// 		}

// 		FnTy :: (int, int) -> string

// 		for i {
// 			print(i)
// 		}

// 	`
// 	tokens_arr, err := tokenize(source)
// 	tokens := tokens_arr[:]
// 	if err, ok := err.(string); ok {
// 		print(err)
// 		return
// 	}
// 	print_tokens_as_code(tokens)
// 	print_tokens(tokens)
// 	mod, ok := parse(tokens)
// 	print("parse err: ", ok)
// 	print(module_to_string(mod))
// }
