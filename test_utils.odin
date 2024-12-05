package rite
import "core:log"
import "core:math"
import "core:math/rand"
import "core:strings"
import "core:testing"

// /////////////////////////////////////////////////////////////////////////////
// SECTION: Displaying the AST
// /////////////////////////////////////////////////////////////////////////////

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
		place := expression_to_string(s.place)
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
	case IfStatement:
		builder := strings.builder_make(context.temp_allocator)
		b := &builder
		write(b, indent, "if ", expression_to_string(s.condition), " {\n")
		inner_indent := more_indent(indent)
		for inner_stmnt in s.body {
			write(b, statement_to_string(inner_stmnt, inner_indent), "\n")
		}
		write(b, indent, "}")
		switch else_block in s.else_block {
		case ElseBlock:
			write(b, " else {\n")
			for inner_stmnt in else_block.body {
				write(b, statement_to_string(inner_stmnt, inner_indent), "\n")
			}
			write(b, indent, "}")
		case ^IfStatement:
			write(b, " else ")
			write(b, statement_to_string(else_block^, indent)[4:])
		}
		return strings.to_string(builder)
	case ForStatement:
		builder := strings.builder_make(context.temp_allocator)
		b := &builder
		kind_str := ""
		switch kind in s.kind {
		case ConditionalLoop:
			kind_str = tprint(" ", expression_to_string(kind.condition))
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

comparison_kind_to_string :: proc(cmp_kind: CompareOpKind) -> string {
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
primitive_value_to_string :: proc(prim: PrimitiveValue) -> string {
	switch prim.type {
	case .None:
		return "None"
	case .Bool:
		if prim.data.bool {
			return "true"
		} else {
			return "false"
		}
	case .Int:
		return tprint(prim.data.int)
	case .Float:
		return tprint(prim.data.float)
	case .String:
		return prim.data.string
	case .Char:
		return tprint(prim.data.char)
	case .Type:
		return primitive_type_to_string(prim.data.primitive_type)
	}
	unreachable()
}
expression_to_string :: proc(expr: Expression, indent: string = "") -> string {
	switch ex in expr.kind {
	case InvalidExpression:
		return tprint(
			"InvalidExpression(",
			ex.msg,
			", got `",
			ex.tokens,
			tokens_as_code(ex.tokens_slice),
			"`)",
		)
	case LogicalOp:
		return two_arg_expr_to_string("OR" if ex.kind == .Or else "AND", ex.first^, ex.second^)
	case CompareOp:
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
		return tprint("MINUS(", expression_to_string(ex.inner^), ")")
	case NotExpression:
		return tprint("NOT(", expression_to_string(ex.inner^), ")")
	case CallOp:
		function := expression_to_string(ex.function^)
		builder := strings.builder_make(context.temp_allocator)
		b := &builder
		write(b, "CALL(", function)
		if len(ex.args) > 0 {
			write(b, " WITH ARGS ")
		}
		for arg, i in ex.args {
			if i != 0 {
				write(b, ", ")
			}
			write(b, expression_to_string(arg))
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
	case Primitive:
		return primitive_value_to_string(ex.value)
	case StructLiteral:
		builder := strings.builder_make(context.temp_allocator)

		b := &builder
		if name, ok := ex.name_or_brace_token_idx.(^Expression); ok {
			write(b, expression_to_string(name^))
		}

		write(b, "{ ")
		switch fields in ex.fields {
		case []Expression:
			for value, i in fields {
				if i != 0 {
					write(b, ", ")
				}
				write(b, expression_to_string(value))
			}
		case []NamedField:
			for field, i in fields {
				if i != 0 {
					write(b, ", ")
				}
				write(b, field.name.name, ": ")
				write(b, expression_to_string(field.value))
			}
		}
		write(b, "}" if ex.fields == nil else " }")
		return strings.to_string(builder)
	case ArrayLiteral:
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
	case MapLiteral:
		builder := strings.builder_make(context.temp_allocator)
		b := &builder
		write(b, "[")
		for e, i in ex.entries {
			if i != 0 {
				write(b, ", ")
			}
			write(b, expression_to_string(e.key), ": ", expression_to_string(e.value))
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
	case UnionDecl:
		todo()
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
		write(b, arg.name.name, ": ", expression_to_string(arg.ty))
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

more_indent :: proc(indent: string = "") -> string {
	return tprint(indent, "    ")
}

// /////////////////////////////////////////////////////////////////////////////
// SECTION: Display token streams
// /////////////////////////////////////////////////////////////////////////////


// /////////////////////////////////////////////////////////////////////////////
// SECTION: Fuzzing
// /////////////////////////////////////////////////////////////////////////////


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
random_token :: proc() -> (tok: Token) {
	LIT_TOKENS := [?]TokenType {
		.LitBool, // true or false
		.LitInt, // e.g. 383
		.LitFloat, // e.g. 3.40
		.LitString, // "Hello"
		.LitChar, // 'Hello'
		.PrimitiveType,
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
		tok.data.string = rand.choice(IDENT_NAMES[:])
	} else if r > 0.4 {
		tok.ty = rand.choice(LIT_TOKENS[:])
		#partial switch tok.ty {
		case .LitBool:
			// true or false
			tok.data.bool = rand.choice([]bool{true, false})
		case .LitInt:
			// e.g. 383
			tok.data.int = rand.int63() % 100
		case .LitFloat:
			// e.g. 3.40
			tok.data.float = math.round_f64(rand.float64() * 1000.0) / 100.0
		case .LitString:
			// "Hello"
			strings := [?]string{"Hello", "World", "What", "Odin", "Love", "Is"}
			tok.data.string = rand.choice(strings[:])
		case .LitChar:
			// 'Hello'
			chars := [?]rune{'j', 'k', 'p', 'g', 'h', 'q', 'l'}
			tok.data.char = rand.choice(chars[:])
		case .PrimitiveType:
			tok.data.primitive_type = rand.choice_enum(PrimitiveType)
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


// /////////////////////////////////////////////////////////////////////////////
// SECTION: Parsing
// /////////////////////////////////////////////////////////////////////////////

expression_from_string :: proc(s: string) -> Expression {
	tokens, err := tokenize(s)
	assert(err == nil)
	print(tokens)
	return parse_expression(tokens[:])
}

module_from_string :: proc(s: string) -> (Module, []Token) {
	tokens, err := tokenize(s)
	assert(err == nil)
	return parse(tokens[:]), tokens[:]
}

statement_slice_eq :: proc(a: []Statement, b: []Statement) -> bool {
	if len(a) != len(b) {
		return false
	}
	for a_val, i in a {
		b_val := b[i]
		statement_eq(a_val, b_val) or_return
	}
	return true
}
if_statement_eq :: proc(a: IfStatement, b: IfStatement) -> bool {
	expression_eq(a.condition, b.condition) or_return
	statement_slice_eq(a.body, b.body) or_return
	switch el_a in a.else_block {
	case nil:
		if b.else_block != nil {
			return false
		}
	case ElseBlock:
		el_b := b.else_block.(ElseBlock) or_return
		return statement_slice_eq(el_a.body, el_b.body)
	case ^IfStatement:
		el_b := b.else_block.(^IfStatement) or_return
		return if_statement_eq(el_a^, el_b^)
	}
	return true
}
statement_eq :: proc(a_stmt: Statement, b_stmt: Statement) -> bool {
	switch a in a_stmt {
	case Assignment:
		b := b_stmt.(Assignment) or_return
		if a.kind != b.kind {
			return false
		}
		expression_eq(a.place, b.place) or_return
		expression_eq(a.value, b.value) or_return
	case Declaration:
		b := b_stmt.(Declaration) or_return
		if a.kind != b.kind {
			return false
		}
		maybe_expression_eq(a.ty, b.ty) or_return
		maybe_expression_eq(a.value, b.value) or_return
	case Expression:
		b := b_stmt.(Expression) or_return
		return expression_eq(a, b)
	case IfStatement:
		b := b_stmt.(IfStatement) or_return
		return if_statement_eq(a, b)
	case ForStatement:
		b := b_stmt.(ForStatement) or_return
		switch a_kind in a.kind {
		case nil:
			if b.kind != nil {
				return false
			}
		case ConditionalLoop:
			b_kind := b.kind.(ConditionalLoop) or_return
			expression_eq(a_kind.condition, b_kind.condition) or_return
		case IteratorLoop:
			b_kind := b.kind.(IteratorLoop) or_return
			expression_eq(a_kind.iterator, b_kind.iterator) or_return
			if a_kind.variable.name != b_kind.variable.name {
				return false
			}
		}
		statement_slice_eq(a.body, b.body) or_return
	case BreakStatement:
		b := b_stmt.(BreakStatement) or_return
	case ReturnStatement:
		b := b_stmt.(ReturnStatement) or_return
		maybe_expression_eq(a.value, b.value) or_return
	}
	return true
}
expression_slice_eq :: proc(a: []Expression, b: []Expression) -> bool {
	if len(a) != len(b) {
		return false
	}
	for a_val, i in a {
		b_val := b[i]
		expression_eq(a_val, b_val) or_return
	}
	return true
}
named_field_slice_eq :: proc(a: []NamedField, b: []NamedField) -> bool {
	if len(a) != len(b) {
		return false
	}
	for a_field, i in a {
		b_field := b[i]
		expression_eq(a_field.value, b_field.value) or_return
		if a_field.name.name != b_field.name.name {
			return false
		}
	}
	return true
}
function_arg_slice_eq :: proc(a: []FunctionArg, b: []FunctionArg) -> bool {
	if len(a) != len(b) {
		return false
	}
	for a_field, i in a {
		b_field := b[i]
		expression_eq(a_field.ty, b_field.ty) or_return
		if a_field.name.name != b_field.name.name {
			return false
		}
	}
	return true
}

maybe_expression_ptr_eq :: proc(a_ex: Maybe(^Expression), b_ex: Maybe(^Expression)) -> bool {
	a, a_is_expr := a_ex.(^Expression)
	b, b_is_expr := b_ex.(^Expression)
	if a_is_expr != b_is_expr {
		return false
	}
	if a_is_expr {
		return expression_eq(a^, b^)
	}
	return true
}
maybe_expression_eq :: proc(a_ex: Maybe(Expression), b_ex: Maybe(Expression)) -> bool {
	a, a_is_expr := a_ex.(Expression)
	b, b_is_expr := b_ex.(Expression)
	if a_is_expr != b_is_expr {
		return false
	}
	if a_is_expr {
		return expression_eq(a, b)
	}
	return true
}
primitive_value_eq :: proc(a: PrimitiveValue, b: PrimitiveValue) -> bool {
	if a.type != b.type {
		return false
	}
	switch a.type {
	case .None:
		return true
	case .Bool:
		return a.data.bool == b.data.bool
	case .Int:
		return a.data.int == b.data.int
	case .Float:
		return a.data.float == b.data.float
	case .String:
		return a.data.string == b.data.string
	case .Char:
		return a.data.char == b.data.char
	case .Type:
		return a.data.primitive_type == b.data.primitive_type
	}
	unreachable()
}
expression_eq :: proc(a_ex: Expression, b_ex: Expression) -> bool {
	switch a in a_ex.kind {
	case InvalidExpression:
		b := b_ex.kind.(InvalidExpression) or_return
		return b.msg == a.msg
	case LogicalOp:
		b := b_ex.kind.(LogicalOp) or_return
		if a.kind != b.kind {
			return false
		}
		return expression_eq(a.first^, b.first^) && expression_eq(a.second^, b.second^)
	case CompareOp:
		b := b_ex.kind.(CompareOp) or_return
	case MathOp:
		b := b_ex.kind.(MathOp) or_return
		return(
			a.kind == b.kind &&
			expression_eq(a.first^, b.first^) &&
			expression_eq(a.second^, b.second^) \
		)
	case NegateExpression:
		b := b_ex.kind.(NegateExpression) or_return
		return expression_eq(a.inner^, b.inner^)
	case NotExpression:
		b := b_ex.kind.(NotExpression) or_return
		return expression_eq(a.inner^, b.inner^)
	case CallOp:
		b := b_ex.kind.(CallOp) or_return
		expression_eq(a.function^, b.function^) or_return
		expression_slice_eq(a.args, b.args) or_return
	case IndexOp:
		b := b_ex.kind.(IndexOp) or_return
		return expression_eq(a.place^, b.place^) && expression_eq(a.index^, b.index^)
	case AccessOp:
		b := b_ex.kind.(AccessOp) or_return
		return expression_eq(a.parent^, b.parent^) && a.ident.name == b.ident.name
	case Ident:
		b := b_ex.kind.(Ident) or_return
		return a.name == b.name
	case Primitive:
		b := b_ex.kind.(Primitive) or_return
		return primitive_value_eq(a.value, b.value)
	case StructLiteral:
		b := b_ex.kind.(StructLiteral) or_return
		switch a_fields in a.fields {
		case nil:
			if b.fields != nil {
				return false
			}
		case []NamedField:
			b_fields := b.fields.([]NamedField) or_return
			named_field_slice_eq(a_fields, b_fields) or_return
		case []Expression:
			b_fields := b.fields.([]Expression) or_return
			expression_slice_eq(a_fields, b_fields) or_return
		}
		if a_name, ok := a.name_or_brace_token_idx.(^Expression); ok {
			b_name := b.name_or_brace_token_idx.(^Expression) or_return
			expression_eq(a_name^, b_name^) or_return
		}
	case ArrayLiteral:
		b := b_ex.kind.(ArrayLiteral) or_return
		expression_slice_eq(a.values, b.values) or_return
	case MapLiteral:
		b := b_ex.kind.(MapLiteral) or_return
		if len(a.entries) != len(b.entries) {
			return false
		}
		for a_pair, i in a.entries {
			b_pair := b.entries[i]
			expression_eq(a_pair.key, b_pair.key) or_return
			expression_eq(a_pair.value, b_pair.value) or_return
		}
	case FunctionSignature:
		b := b_ex.kind.(FunctionSignature) or_return
		expression_slice_eq(a.arg_types, b.arg_types) or_return
		expression_eq(a.return_type^, b.return_type^) or_return
	case FunctionDefinition:
		b := b_ex.kind.(FunctionDefinition) or_return
		function_arg_slice_eq(a.args, b.args) or_return
		maybe_expression_ptr_eq(a.return_type, b.return_type) or_return
		statement_slice_eq(a.body, b.body) or_return
	case EnumDecl:
		b := b_ex.kind.(EnumDecl) or_return
		if len(a.variants) != len(b.variants) {
			return false
		}
		for a_var, i in a.variants {
			b_var := b.variants[i]
			if a_var.name != b_var.name {
				return false
			}
		}
	case UnionDecl:
		b := b_ex.kind.(UnionDecl) or_return
		todo()
	}

	return true
}

test_expect_tokens_equal :: proc(t: ^testing.T, got: []Token, expected: []Token) {
	for el in got {
		assert(el.ty != .Eof)
	}
	for el in expected {
		assert(el.ty != .Eof)
	}
	min_len := min(len(got), len(expected))
	for i in 0 ..< min_len {
		g := got[i]
		e := expected[i]
		testing.expectf(
			t,
			token_eq(g, e),
			"expected token %d to be %v, got %v",
			i,
			tokens_to_string({g}),
			tokens_to_string({e}),
		)
	}
	if min_len < len(got) {
		log.errorf(
			"expected only %d tokens, got additional %v",
			min_len,
			tokens_to_string(got[min_len:]),
		)
		testing.fail(t)
	} else if min_len < len(expected) {
		log.errorf(
			"expected %d tokens, but got only %d, missing %v",
			len(expected),
			min_len,
			tokens_to_string(expected[min_len:]),
		)
		testing.fail(t)
	}

}

token_eq :: proc(a: Token, b: Token) -> bool {
	if a.ty != b.ty {
		return false
	}
	#partial switch a.ty {
	case .Ident:
		return a.data.string == b.data.string
	case .LitInt:
		return a.data.int == b.data.int
	case .LitFloat:
		return a.data.float == b.data.float
	case .LitBool:
		return a.data.bool == b.data.bool
	case .LitString:
		return a.data.string == b.data.string
	case .LitChar:
		return a.data.char == b.data.char
	}
	return true
}
