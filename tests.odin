package rite
import "core:strings"


main :: proc() {

	source := `
	// Foo :: {i: int, j: string}
	// f := Foo{ i: 0,  j: "string" }
	a : int = 0
	b := 2
	for a < b {
		a +=1
		"Hello".print()
	}
	`


	rand.reset(4)
	tokens := add_random_tokens_into_code(source, 3)
	print_tokens_as_code(tokens[:])

	// tokens := random_tokens(100)
	// print_tokens(tokens[:])

	// source := `
	// Foo :: {i: int, j: string}
	// f := Foo(int){ i: 0,  j: "string" }
	// `


	// words := [?]string{"Hello", "my", "love", "you", "are", "the", "best"}
	// arr: BucketArray(string)
	// for w in words {
	// 	ptr := bucket_array_insert(&arr, w)
	// 	print(cast(uintptr)ptr)
	// }
	// bucket_array_print(arr)
	// bucket_array_free(&arr)


	// tokens := tokenize(source) or_else panic("tokenization error")
	mod, err := parse(tokens[:])
	if err, ok := err.(string); ok {
		print("error: ", err)
		return
	} else {
		print("Module:")
		print(module_to_string(mod))
	}
	// err = type_check(&mod)
	// if err, ok := err.(string); ok {
	// 	print("Type check error: ", err)
	// } else {
	// 	print("Type check okay")
	// }
}

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

write :: proc(builder: ^strings.Builder, elements: ..string) {
	for s in elements {
		strings.write_string(builder, s)
	}
}
module_to_string :: proc(mod: Module) -> string {

	builder: strings.Builder
	s := &builder
	write(s, "Module(\n")
	indent := more_indent()
	for stmnt in mod.statements {
		write(s, statement_to_string(stmnt, indent), "\n")
	}
	write(s, ")")
	return strings.to_string(builder)
}

statement_to_string :: proc(stmnt: Statement, indent: string = "") -> string {
	switch s in stmnt {
	case Assignment:
		place := expression_to_string(assignment_place_as_expression(s.place))
		assign := assignment_kind_to_long_string(s.kind)
		value := expression_to_string(s.value, indent)
		return tprint(indent, assign, "(", place, ", ", value, ")")
	case Declaration:
		name := s.ident.name
		ty := ""
		value := ""
		if ty_expr, ok := s.ty.(Expression); ok {
			ty = expression_to_string(ty_expr)
		}
		if value_expr, ok := s.value.(Expression); ok {
			value = expression_to_string(value_expr, indent)
		}
		switch s.kind {
		case .ConstExplicit:
			return tprint(indent, "DECLARE_CONST(", name, ", ", ty, ", ", value, ")")
		case .ConstInferred:
			return tprint(indent, "DECLARE_CONST(", name, ", INFERRED, ", value, ")")
		case .RuntimeExplicit:
			return tprint(indent, "DECLARE(", name, ", ", ty, ", ", value, ")")
		case .RuntimeExplicitDefault:
			return tprint(indent, "DECLARE(", name, ", ", ty, ", DEFAULT)")
		case .RuntimeInferred:
			return tprint(indent, "DECLARE(", name, ", INFERRED, ", value, ")")
		}
	case Expression:
		return tprint(indent, expression_to_string(s, indent))
	case IfBlock:
		builder := strings.builder_make(context.temp_allocator)
		b := &builder
		write(b, indent, "if ", expression_to_string(s.condition), " {\n")
		inner_indent := more_indent(indent)
		for inner_stmnt in s.body {
			write(b, statement_to_string(inner_stmnt, inner_indent), "\n")
		}
		write(b, indent, "}")
		switch else_block in s.else_block {
		case ^ElseBlock:
			write(b, " else {\n")
			for inner_stmnt in else_block.body {
				write(b, statement_to_string(inner_stmnt, inner_indent), "\n")
			}
			write(b, indent, "}")
		case ^IfBlock:
			write(b, " else ")
			write(b, statement_to_string(else_block^, indent)[4:])
		}
		return strings.to_string(builder)
	case ForLoop:
		builder := strings.builder_make(context.temp_allocator)
		b := &builder
		kind_str := ""
		switch kind in s.kind {
		case ConditionalLoop:
			kind_str = tprint(" ", expression_to_string(Expression(kind)))
		case IteratorLoop:
			kind_str = tprint(" ", kind.variable.name, " in ", expression_to_string(kind.iterator))
		}
		write(b, indent, "for", kind_str, " {\n")
		inner_indent := more_indent(indent)
		for inner_stmnt in s.body {
			write(b, statement_to_string(inner_stmnt, inner_indent), "\n")
		}
		write(b, indent, "}")
		return strings.to_string(builder)
	case BreakStatement:
		return tprint(indent, "break")
	case ReturnStatement:
		value := ""
		if val, ok := s.value.(Expression); ok {
			value = expression_to_string(val)
		}
		return tprint(indent, "return ", value)
	}
	return tprint(indent, "UnknownStatement")
}


two_arg_expr_to_string :: proc(op: string, first: Expression, second: Expression) -> string {
	first := expression_to_string(first)
	second := expression_to_string(second)
	return tprint(op, "(", first, ", ", second, ")")
}

comparison_kind_to_string :: proc(cmp_kind: ComparisonKind) -> string {
	switch cmp_kind {
	case .Equal:
		return "EQUAL"
	case .NotEqual:
		return "NOT_EQUAL"
	case .Greater:
		return "GREATER"
	case .GreaterEqual:
		return "GREATER_EQUAL"
	case .Less:
		return "LESS"
	case .LessEqual:
		return "LESS_EQUAL"
	}
	unreachable()
}

expression_to_string :: proc(expr: Expression, indent: string = "") -> string {
	switch ex in expr.kind {
	case LogicalOr:
		return two_arg_expr_to_string("OR", ex.first^, ex.second^)
	case LogicalAnd:
		return two_arg_expr_to_string("AND", ex.first^, ex.second^)
	case Comparison:
		if len(ex.others) != 1 {
			panic("more than 2 way comparison not supported in expression printer atm!")
		}
		op := comparison_kind_to_string(ex.others[0].kind)
		return two_arg_expr_to_string(op, ex.first^, ex.others[0].expr)
	case MathOp:
		op := ""
		switch ex.kind {
		case .Add:
			op = "ADD"
		case .Sub:
			op = "SUB"
		case .Mul:
			op = "MUL"
		case .Div:
			op = "DIV"
		}
		return two_arg_expr_to_string(op, ex.first^, ex.second^)
	case FunctionSignature:
		builder := strings.builder_make(context.temp_allocator)
		b := &builder
		write(b, "(")
		for arg, i in ex.arg_types {
			if i != 0 {
				write(b, ", ")
			}
			write(b, expression_to_string(arg))
		}
		write(b, ") -> ")
		write(b, expression_to_string(ex.return_type^))
		return strings.to_string(builder)
	case FunctionDefinition:
		return function_definition_to_string(ex, indent)
	case NegateExpression:
		return tprint("MINUS(", expression_to_string(ex^), ")")
	case NotExpression:
		return tprint("NOT(", expression_to_string(ex^), ")")
	case CallOp:
		function := expression_to_string(ex.function^)
		builder := strings.builder_make(context.temp_allocator)
		b := &builder
		write(b, "CALL(", function)
		for arg, i in ex.args {
			write(&builder, ", ", expression_to_string(arg))
		}
		write(b, ")")
		return strings.to_string(builder)
	case IndexOp:
		place := expression_to_string(ex.place^)
		index := expression_to_string(ex.index^)
		return tprint("INDEX(", place, ", ", index, ")")
	case Ident:
		return ex.name
	case AccessOp:
		place := expression_to_string(ex.parent^)
		return tprint(place, ".", ex.ident.name)
	case LitBool:
		if ex.value {
			return "true"
		} else {
			return "false"
		}
	case LitInt:
		return tprint(ex.value)
	case LitFloat:
		return tprint(ex.value)
	case LitString:
		builder := strings.builder_make(context.temp_allocator)
		strings.write_quoted_string(&builder, ex.value)
		return strings.to_string(builder)
	case LitChar:
		builder := strings.builder_make(context.temp_allocator)
		strings.write_quoted_rune(&builder, ex.value)
		return strings.to_string(builder)
	case LitStruct:
		builder := strings.builder_make(context.temp_allocator)
		b := &builder
		if name, ok := ex.name.(^Expression); ok {
			write(b, expression_to_string(name^))
		}
		write(b, "{ ")
		for field, i in ex.fields {
			if i != 0 {
				write(b, ", ")
			}
			if name, is_named := field.name.(^Expression); is_named {
				write(b, expression_to_string(name^), ": ")
			}
			write(b, expression_to_string(field.value^))
		}
		write(b, " }")
		return strings.to_string(builder)
	case LitArray:
		builder := strings.builder_make(context.temp_allocator)
		b := &builder
		write(b, "[")
		for v, i in ex.values {
			if i != 0 {
				write(b, ", ")
			}
			write(b, expression_to_string(v))
		}
		write(b, "]")
		return strings.to_string(builder)
	case EnumDecl:
		builder := strings.builder_make(context.temp_allocator)
		b := &builder
		write(b, "ENUM_DEF(")
		for v, i in ex.variants {
			if i != 0 {
				write(b, ", ")
			}
			write(b, v.name)
		}
		write(b, ")")
		return strings.to_string(builder)
	case LitUnionDecl:
		todo()
	case LitNone:
		return "None"
	}
	return tprint("UnhandledExpression ", expr)
}
function_definition_to_string :: proc(fun_def: FunctionDefinition, indent: string) -> string {
	builder: strings.Builder
	b := &builder
	write(b, "(")
	for arg, i in fun_def.args {
		if i != 0 {
			write(b, ", ")
		}
		write(b, arg.name.name, ": ", expression_to_string(arg.type))
	}

	if return_ty, ok := fun_def.return_type.(^Expression); ok {
		write(b, ") -> ", expression_to_string(return_ty^))
	} else {
		write(b, ") -> None")
	}
	switch len(fun_def.body) {
	case 0:
		write(b, " { }")
	case 1:
		write(b, " { ", statement_to_string(fun_def.body[0]), " }")
	case:
		write(b, " {\n")
		inner_indent := more_indent(indent)
		for s in fun_def.body {
			write(b, inner_indent, statement_to_string(s), "\n")
		}
		write(b, indent, "}")
	}
	return strings.to_string(builder)
}
assignment_kind_to_string :: proc(kind: AssignmentKind) -> string {
	switch kind {
	case .Assign:
		return "="
	case .AddAssign:
		return "+="
	case .SubAssign:
		return "-="
	case .MulAssign:
		return "*="
	case .DivAssign:
		return "/="
	}
	unreachable()
}
assignment_kind_to_long_string :: proc(kind: AssignmentKind) -> string {
	switch kind {
	case .Assign:
		return "ASSIGN"
	case .AddAssign:
		return "ADD_ASSIGN"
	case .SubAssign:
		return "SUB_ASSIGN"
	case .MulAssign:
		return "MUL_ASSIGN"
	case .DivAssign:
		return "DIV_ASSIGN"
	}
	unreachable()
}

assignment_place_as_expression :: proc(place: AssignmentPlace) -> Expression {
	switch place in place {
	case AccessOp:
		return Expression{kind = place}
	case Ident:
		return Expression{kind = place}
	case IndexOp:
		return Expression{kind = place}
	}
	unreachable()
}

more_indent :: proc(indent: string = "") -> string {
	return tprint(indent, "    ")
}


tokenize_test :: proc() {
	source := `
	Article :: {
		authors: [string],
		date: Date = {year: 2023, month: 7, day: 28},
	}
	MyU :: int | string | Article
	u: MyU = 3
	articles: [Article]
	switch u {
		case int:
			u = "Hello" // error because u is a refererence to an int already
		case string:
			u.len().print()
		case Article:
			articles.push(u)
	}

	`


	tokens, err := tokenize(source)
	if err, is_err := err.(string); is_err {
		print("Error: ", err)
		print_tokens_as_code(tokens[:])
	} else {
		print("Source:")
		print(source)
		print("Tokenized:")
		print_tokens_as_code(tokens[:])
		print_tokens(tokens[:])
	}

}

random_tokens :: proc(num: int = 10) -> (tokens: [dynamic]Token) {
	for i in 0 ..< num {
		append(&tokens, random_token())
	}
	return tokens
}

add_random_tokens_into_code :: proc(source: string, num_random_tokens: int) -> [dynamic]Token {
	tokens, err := tokenize(source)
	assert(err == nil)
	for _ in 0 ..< num_random_tokens {
		inject_at(&tokens, rand.int_max(len(tokens)), random_token())
	}
	return tokens
}

import "core:math"
import "core:math/rand"
random_token :: proc() -> (tok: Token) {
	LIT_TOKENS := [?]TokenType {
		.LitBool, // true or false
		.LitInt, // e.g. 383
		.LitFloat, // e.g. 3.40
		.LitString, // "Hello"
		.LitChar, // 'Hello'
		.LitNone, //
	}
	OTHER_TOKENS := [?]TokenType {
		.LeftBrace,
		.RightBrace,
		.LeftBracket,
		.RightBracket,
		.LeftParen,
		.RightParen,
		.Dot, // .
		.DotDot, // ..
		.DotDotDot, //  ...
		.Pipe, // |
		.Add, // +
		.AddAssign, // +=
		.Sub, // -
		.SubAssign, // -=
		.Mul, // *
		.MulAssign, // *= 
		.Div, // /
		.DivAssign, // 
		.Colon, // :
		.ColonAssign, // :=
		.ColonColon, // ::
		.Assign, // = 
		.Not, // !
		.NotEqual, // !=
		.Greater, // >
		.Less, // <
		.GreaterEqual, // >=
		.LessEqual, // <=
		.Equal, // ==
		.And, // &&, and
		.Or, // ||, or
		.Arrow, // ->
		.Is, // is
		.In, // in
		.If, // if
		.For, // for
		.Else, // else
		.Enum, // enum
		.Break, // break
		.Return, // return
	}

	r := rand.float32()
	if r > 0.7 {
		tok.ty = .Ident
		IDENT_NAMES := [?]string {
			"Foo",
			"Bar",
			"Baz",
			"a",
			"x",
			"b",
			"fizz",
			"crux",
			"abc",
			"c",
			"d",
			"xxx",
		}
		tok.meta.string = rand.choice(IDENT_NAMES[:])
	} else if r > 0.4 {
		tok.ty = rand.choice(LIT_TOKENS[:])
		#partial switch tok.ty {
		case .LitBool:
			// true or false
			tok.meta.bool = rand.choice([]bool{true, false})
		case .LitInt:
			// e.g. 383
			tok.meta.int = rand.int63() % 100
		case .LitFloat:
			// e.g. 3.40
			tok.meta.float = math.round_f64(rand.float64() * 1000.0) / 100.0
		case .LitString:
			// "Hello"
			strings := [?]string{"Hello", "World", "What", "Odin", "Love", "Is"}
			tok.meta.string = rand.choice(strings[:])
		case .LitChar:
			// 'Hello'
			chars := [?]rune{'j', 'k', 'p', 'g', 'h', 'q', 'l'}
			tok.meta.char = rand.choice(chars[:])
		case .LitNone: //

		}
	} else {
		tok.ty = rand.choice(OTHER_TOKENS[:])
	}
	return tok
}


SOMECODE :: `
Person :: {
    name: string
    age: int
}
p := Person{name: "Tom", age: 45}
people : [Person] = [{"Hans", 23}, {"Claus", 12}] 

PI :: 3.14
Vec2 :: {x: float, y: float}
cross :: (a: Vec2, b: Vec2) -> float {return a.x*b.y - a.y*b.x}

Circle :: {radius: float}
Rectangle :: {a: float, b: float}
Polygon :: {outline : [Vec2]}

// union of different types
Shape :: Circle | Rectangle | Polygon

area :: (shape: Shape) -> float {
    switch shape {
        case Circle: return shape.radius.squared() * PI 
        case Rectangle: return shape.a * shape.b
        case Polygon: return shape.outline.area() // see function below
    }
}

area :: (outline: [Vec2]) -> float {
    n := outline.len
    if n < 3 { return 0 }
    sum := 0
    for i in n {
        sum += outline[i].cross(outline[(i + 1) % n])
    }
    return sum
}

shapes := [Circle{3}, Rect{4,5}, Polygon{ouline: [{1,2}, {5,4}, {0,8}, {-2,4}]}]

// print all shapes that are a circle or have a large area:
shapes.filter(_.area() > 10 || _ is Circle).print()

`
