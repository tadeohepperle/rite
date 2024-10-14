package rite
import "core:strings"

/*
	


*/

main :: proc() {
	source := `
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
	`
	tokens_arr, err := tokenize(source)
	tokens := tokens_arr[:]
	if err, ok := err.(string); ok {
		print(err)
		return
	}
	print_tokens_as_code(tokens)
	print_tokens(tokens)
	mod, ok := parse(tokens)
	print("parse err: ", ok)
	print(module_to_string(mod))
}

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
		assign := assignment_kind_to_string(s.kind)
		value := expression_to_string(s.value)
		return tprint(indent, place, " ", assign, " ", value)
	case Declaration:
		name := s.ident.name
		ty := ""
		value := ""
		if ty_expr, ok := s.ty.(Expression); ok {
			ty = expression_to_string(ty_expr)
		}
		if value_expr, ok := s.value.(Expression); ok {
			value = expression_to_string(value_expr)
		}
		switch s.kind {
		case .ConstExplicit:
			return tprint(indent, name, " : ", ty, " : ", value)
		case .RuntimeExplicit:
			return tprint(indent, name, " : ", ty, " = ", value)
		case .RuntimeExplicitDefault:
			return tprint(indent, name, " : ", ty)
		case .ConstInferred:
			return tprint(indent, name, " :: ", value)
		case .RuntimeInferred:
			return tprint(indent, name, " := ", value)
		}
	case Expression:
		return tprint(indent, expression_to_string(s))
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
		condition := ""
		if cond, ok := s.condition.(Expression); ok {
			condition = tprint(" ", expression_to_string(cond))
		}
		write(b, indent, "for", condition, " {\n")
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

expression_to_string :: proc(expr: Expression) -> string {
	switch ex in expr {
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
	case MathOperation:
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
	case NegateExpression:
		return tprint("MINUS(", expression_to_string(ex), ")")
	case NotExpression:
		return tprint("NOT(", expression_to_string(ex), ")")
	case FunctionCall:
		function := expression_to_string(ex.function^)
		builder := strings.builder_make(context.temp_allocator)
		b := &builder
		write(b, "CALL(", function)
		for arg, i in ex.args {
			write(&builder, ", ", expression_to_string(arg))
		}
		write(b, ")")
		return strings.to_string(builder)
	case IndexOperation:
		place := expression_to_string(ex.place^)
		index := expression_to_string(ex.index^)
		tprint("INDEX(", place, ", ", index, ")")
	case Ident:
		return ex.name
	case IdentPath:
		assert(len(ex) >= 1)
		builder := strings.builder_make(context.temp_allocator)
		for v, i in ex {
			if i != 0 {
				write(&builder, ".")
			}
			write(&builder, v.name)
		}
		return strings.to_string(builder)
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
		todo()
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
	return "UnhandledExpression"
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

assignment_place_as_expression :: proc(place: AssignmentPlace) -> Expression {
	switch place in place {
	case IdentPath:
		return place
	case Ident:
		return place
	case IndexOperation:
		return place
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
