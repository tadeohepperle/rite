package rite
import "core:log"
import "core:strings"
import "core:testing"

main :: proc() {

	// expr := expression_from_string("4 + 5 * {1,2,3} + 5")
	expr := expression_from_string("8 * (4-4:)")
	print(expression_to_string(expr))
	// parse_expressions_test(nil)
}


@(test)
parse_expressions_test :: proc(t: ^testing.T) {
	ok_expressions := []string{`4`, `4+6`, `"hello"`}
	bad_expressions := []string{`.5`}
	for ok in ok_expressions {
		ex := expression_from_string(ok)
		testing.expectf(
			t,
			expression_valid(ex),
			"expression %s not valid",
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
		assert(err == nil)
		test_expect_tokens(t, tokens[:], c.tokens)
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
