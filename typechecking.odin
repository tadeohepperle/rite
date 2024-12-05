package rite

float :: f64
/*

we need to annotate the AST with type ids.
Also we need to store what type names relate to which 


For each constant we tr


foo :: (i: int) -> Type {
    if i == 3 {
        return int
    }
    return string
}

A :: {
    f1: foo(3)

}

*/

declaration_outer_dependencies :: proc(decl: Declaration) -> map[string]Empty {
	deps: map[string]Empty
	if ty, ok := decl.ty.(Expression); ok {
		_collect_expr(&deps, ty)
	}
	if value, ok := decl.value.(Expression); ok {
		_collect_expr(&deps, value)
	}
	return deps

	_collect_expr :: proc(deps: ^map[string]Empty, expr: Expression) {
		switch ex in expr.kind {
		case InvalidExpression:
		// nothing to do
		case LogicalOr:
			_collect_expr(deps, ex.first^)
			_collect_expr(deps, ex.second^)
		case LogicalAnd:
			_collect_expr(deps, ex.first^)
			_collect_expr(deps, ex.second^)
		case Comparison:
			_collect_expr(deps, ex.first^)
			for other in ex.others {
				_collect_expr(deps, other.expr)
			}
		case MathOp:
			_collect_expr(deps, ex.first^)
			_collect_expr(deps, ex.second^)
		case NegateExpression:
			_collect_expr(deps, ex.inner^)
		case NotExpression:
			_collect_expr(deps, ex.inner^)
		case CallOp:
			_collect_expr(deps, ex.function^)
			for arg in ex.args {
				_collect_expr(deps, arg)
			}
		case IndexOp:
			_collect_expr(deps, ex.place^)
			_collect_expr(deps, ex.index^)
		case AccessOp:
			_collect_expr(deps, ex.parent^)
			deps[ex.ident.name] = Empty{}
		case Ident:
			deps[ex.name] = Empty{}
		case Primitive:
		case StructLiteral:
			if name_expr, ok := ex.name_or_brace_token_idx.(^Expression); ok {
				_collect_expr(deps, name_expr^)
			}
			switch fields in ex.fields {
			case []Expression:
				for f in fields {
					_collect_expr(deps, f)
				}
			case []NamedField:
				for f in fields {
					_collect_expr(deps, f.value)
				}
			}
		case ArrayLiteral:
			for val in ex.values {
				_collect_expr(deps, val)
			}
		case MapLiteral:
			for kv in ex.entries {
				_collect_expr(deps, kv.key)
				_collect_expr(deps, kv.value)
			}
		case FunctionSignature:
			_collect_expr(deps, ex.return_type^)
			for arg_ty in ex.arg_types {
				_collect_expr(deps, arg_ty)
			}
		case FunctionDefinition:
			if return_type, ok := ex.return_type.(^Expression); ok {
				_collect_expr(deps, return_type^)
			}
			for arg in ex.args {
				_collect_expr(deps, arg.ty)
			}
		// no need to analyze body
		case EnumDecl:
		// nothing to do
		case UnionDecl:
			for var in ex.variants {
				_collect_expr(deps, var)
			}

		}
	}
}

toposort_constants_by_outer_dependencies :: proc(
	constants: ^ScopeConstants,
) -> (
	sorted: [dynamic]string,
	cycled: [dynamic]string,
) {
	// Note: limitation with this approach is that constants with the same name are not allowed to form cycles.
	// so this is not okay:   
	// foo :: (i: int ) -> Type { if i < 10 { return int } else { return None } }
	// foo :: () -> Type { return { i: foo(3), j: string } }
	// MyStruct :: foo() // should be { i: int, j: string }

	OverloadGroup :: struct {
		constants:      [dynamic]^ScopeConstant,
		dependents:     map[string]Empty,
		n_dependencies: int,
	}
	overload_group :: proc() -> OverloadGroup {
		return OverloadGroup {
			constants = tmp_arr(^ScopeConstant),
			dependents = tmp_map(string, Empty),
			n_dependencies = 0,
		}
	}

	overload_groups: map[string]OverloadGroup = tmp_map(string, OverloadGroup)

	// fill the overload_groups map (I wish we could avoid too many hash calculations and use hm.get_or_insert() like in rust):
	for name, constants in constants {
		if name not_in overload_groups {
			overload_groups[name] = overload_group()
		}
		group := &overload_groups[name]
		for &const in constants {
			append(&group.constants, &const)
			for dep_name in const.outer_dependencies {
				// add the dependency:
				if dep_name not_in overload_groups {
					overload_groups[dep_name] = overload_group()
				}
				dep_group: ^OverloadGroup = &overload_groups[dep_name]
				if name not_in dep_group.dependents {
					group.n_dependencies += 1
					dep_group.dependents[name] = Empty{}
				}
			}
		}
	}

	info("Overload groups:")
	for name, group in overload_groups {
		info("    ", name, group.n_dependencies, group.dependents)
	}

	// find start elements (with non-nil const) and without any dependency and add them to the sorted list:
	sorted = tmp_arr(string)
	cycled = tmp_arr(string)
	for name, group in overload_groups {
		if len(group.constants) == 0 {
			// name not found (e.g. invalid type/constant)
			// remove this dependency from all others, to not block resolvement.
			// this will result in errors later when a type or other const value is actually looked up and not found.
			for dep_name in group.dependents {
				dep_group := &overload_groups[dep_name]
				dep_group.n_dependencies -= 1
			}
		} else if group.n_dependencies == 0 {
			append(&sorted, name)
		}
	}

	// add other elements to sorted by shaving off the number of their unresolved dependencies,
	// by looking at each element that is already sorted
	for i := 0; i < len(sorted); i += 1 {
		group_name := sorted[i]
		group := &overload_groups[group_name]
		for dep_name in group.dependents {
			dep_group := &overload_groups[dep_name]
			dep_group.n_dependencies -= 1
			if dep_group.n_dependencies == 0 {
				append(&sorted, dep_name)
			}
		}
	}

	// all groups that have any dependencies left must be part of a cycle:
	for name, group in overload_groups {
		if group.n_dependencies != 0 {
			append(&cycled, name)
		}
	}

	return sorted, cycled
}

collect_constants :: proc(statements: []Statement) -> (constants: ScopeConstants) {
	for &stmt in statements {
		decl := (&stmt.(Declaration)) or_continue
		if decl.kind == .ConstExplicit || decl.kind == .ConstInferred {
			const := ScopeConstant {
				decl               = decl,
				value              = {},
				outer_dependencies = declaration_outer_dependencies(decl^),
			}
			name := decl.ident.name
			if name in constants {
				append(&constants[name], const)
			} else {
				constants[name] = [dynamic]ScopeConstant{const}
			}
		}
	}
	return constants
}


Errors :: struct {
	source:     string,
	all_tokens: []Token,
	errors:     [dynamic]Error,
}
errors_print :: proc(this: Errors) {
	if len(this.errors) == 0 {
		print("No errors, all good.")
		return
	}
	print("Found", len(this.errors), "Errors:")
	for e, i in this.errors {
		tokens := this.all_tokens[e.range.start_idx:e.range.end_idx]
		print(i, ":   ", e.msg, "   ", tokens_as_code(tokens))
	}
}
errors_create :: proc(source: string, all_tokens: []Token) -> Errors {
	return Errors{source = source, all_tokens = all_tokens, errors = {}}
}
// errors_add :: proc(this: ^Errors, range: TokenRange, msg_parts: ..any) {
// 	append(&this.errors, Error{tprint(..msg_parts), range, .Unknown})
// }
error_expr :: proc(this: ^Errors, expr: ^Expression, msg: string) {
	range := expression_token_range(expr^)
	msg := tprint("Invalid expression `", expression_to_string(expr^), "`: ", msg) // future: rn stupid and inefficient, ik
	append(&this.errors, Error{msg, range, .Unknown})
}
error_for_stmt :: proc(this: ^Errors, stmt: Statement, msg: string) {
	range := statement_token_range(stmt)
	msg := tprint("Invalid statement `", statement_to_string(stmt), "`: ", msg) // future: rn stupid and inefficient, ik
	append(&this.errors, Error{msg, range, .Unknown})
}
Error :: struct {
	msg:   string,
	range: TokenRange,
	kind:  ErrorKind,
}
// represents stage of the 
ErrorKind :: enum {
	Unknown,
	Parsing,
	TypeCheck,
}


TypeCheckCtx :: struct {
	scope:  ^Scope,
	types:  ^Types,
	errors: ^Errors,
}

typecheck :: proc(mod: ^Module, errors: ^Errors) {
	using ctx := TypeCheckCtx {
		types  = new_clone(builtin_types()),
		scope  = new_clone(Scope{}),
		errors = errors,
	}
	mod.scope = ctx.scope

	// collect constants and sort them in the order in which they can be evaluated:
	scope.constants = collect_constants(mod.statements)
	for name, consts in scope.constants {
		for const in consts {
			info(name, "  ", statement_tokens_as_string(const.decl^, errors.all_tokens))
			for dep in const.outer_dependencies {
				info("  -", dep)
			}
		}
	}

	sorted, cycled := toposort_constants_by_outer_dependencies(&scope.constants)
	info("cycled", cycled)
	info("sorted", sorted)

	// if there are cyclic dependencies, add errors for all of them
	for cycled_name in cycled {
		for const in scope.constants[cycled_name] {
			error_for_stmt(errors, const.decl^, "cyclic dependencies involving")
		}
	}

	// assign types to all declarations and compute const values
	for name in sorted {
		for &const in scope.constants[name] {
			typecheck_const_declaration(&ctx, &const)
		}
	}

	// add types to all constants:

	/*

	collect all the constant declarations in the module and sort them by the names used within their definitions
	





	*/


	// constants: Constants = collect_const_declarations(mod.statements) or_return
}


// type_check_expression 

typecheck_const_declaration :: proc(using ctx: ^TypeCheckCtx, const: ^ScopeConstant) {
	decl_kind := const.decl.kind
	if decl_kind == .ConstExplicit {
		ty_expr := &const.decl.ty.(Expression) or_else panic("ConstExplicit has ty expr")
		typecheck_expression(ctx, ty_expr, TYPE_TYPE)
		ty_value := eval_const_expr(ctx, ty_expr^)
		if ty_value.type == nil {
			// unknown value, some error occurred maybe looking up some named in the const expr. 
			// we don't check earlier that all names are valid
			error_expr(ctx.errors, ty_expr, "type of constant could not be resolved")
			return
		}
		if ty_value.type.tag != .Type {
			// this happens e.g. if someone tries: FOO : 32 : {3,3}
			// here the ty_value.type would be .Int because the type of the 32 is int, which is not a type
			error_expr(
				ctx.errors,
				ty_expr,
				tprint("expression in TypeDecl place is not type, but ", ty_value),
			)
			return
		}
		declaration_type := ty_value.data.type
		// const.value.type = {declaration_type, ???}

		value_expr := &const.decl.value.(Expression) or_else panic("ConstExplicit has value expr")


	} else if decl_kind == .ConstInferred {
		value_expr := &const.decl.value.(Expression) or_else panic("ConstInferred has value expr")
		typecheck_expression(ctx, value_expr, nil)

	} else {
		panic("Not a const declaration")
	}
}

// Note: for now we just execute simple expressions, functions are not supported, as they would require an entire runtime, stack + heap system
// if successful, value.type should be expr.type
eval_const_expr :: proc(ctx: ^TypeCheckCtx, expr: Expression) -> (value: AnyValue) {
	assert(expr.type != nil)
	// switch ex in expr.kind {
	// case InvalidExpression:
	// 	return UNKNOWN_VALUE
	// case LogicalOr:
	// case LogicalAnd:
	// case Comparison:
	// case MathOp:
	// case NegateExpression:
	// case NotExpression:
	// case CallOp:
	// case IndexOp:
	// case AccessOp:
	// case Ident:
	// case Primitive:
	// case StructLiteral:
	// case ArrayLiteral:
	// case MapLiteral:
	// case FunctionSignature:
	// case FunctionDefinition:
	// case EnumDecl:
	// case UnionDecl:

	// }

	todo()
	// assert(value.type == expr.type)
	// return UNKNOWN_VALUE
}

Scope :: struct {
	parent:    ^Scope,
	constants: map[string][dynamic]ScopeConstant,
	variables: map[string]ScopeVariable, // variables will be stack allocated in the order they appear in in code
}
ScopeConstants :: map[string][dynamic]ScopeConstant
ScopeVariable :: struct {
	ty:   Type,
	decl: ^Declaration,
}
ScopeConstant :: struct {
	decl:               ^Declaration,
	value:              AnyValue,
	outer_dependencies: map[string]Empty, // idents found in the type def or value of the constant, NOT counting names that are referred to from the inside of a function in their own scope.
}
AnyValue :: struct {
	type: Type,
	data: AnyData, // points at
}
AnyData :: struct #raw_union {
	int:    int,
	float:  float,
	string: string,
	bool:   bool,
	char:   rune,
	ptr:    rawptr, // for the bytes of structs
	type:   Type, // for values that are types
	slice:  []Empty, // type punned
	// todo: maps,
	// todo: FnInstructions
}
any_value_to_string :: proc(val: AnyValue) -> string {
	if val.type == nil {
		return "UNKNOWN TYPE VALUE"
	}
	switch val.type.tag {
	case .None:
		return "None"
	case .Int:
		return tprint("Int(", val.data.int, ")")
	case .Float:
		return tprint("Float(", val.data.float, ")")
	case .Bool:
		return tprint("Bool(", val.data.bool, ")")
	case .String:
		return tprint("String(", val.data.string, ")")
	case .Char:
		return tprint("Char(", val.data.char, ")")
	case .Struct:
		return tprint("Some Struct Value") // todo
	case .Union:
		return tprint("Some Union Value") // todo
	case .Enum:
		return tprint("Some Enum Value") // todo
	case .Type:
		return tprint("Type(", val.data.type.structure, ")") // todo
	case .Function:
		return tprint("Some Function Value")
	case .Array:
		return tprint("Some Array")
	case .Map:
		return tprint("Some Map")
	}
	unreachable()
}

// can be produced as an error if e.g. a name could not be resolved
UNKNOWN_VALUE := AnyValue {
	type = nil,
	data = {},
}

FunctionValue :: struct {
	arg_types_hash: ArgTypesHash, // not used yet
	expr:           ^Expression,
}

ArgTypesHash :: distinct u64
/// the TypeHash is a hash that follows directly from the structure of a type (which may include a global name, distinguishing e.g. different zero sized types)
TypeHash :: distinct u64

Type :: ^TypeInfo
Types :: struct {
	// Note: builtin types live in static memory, only user defined types are registered in here.
	// BucketArray such that memory stays pinned.
	registered:    BucketArray(TypeInfo),
	hash_to_types: map[TypeHash]^TypeInfo,
}
TypeInfo :: struct {
	hash:            TypeHash, // calculated from structure
	using structure: TypeStructure,
}
TypeStructure :: struct {
	tag:  TypeTag,
	meta: TypeMeta,
}
TypeTag :: enum {
	None,
	Int,
	Float,
	Bool,
	String,
	Char,
	Struct,
	Union,
	Enum,
	Type,
	Function,
	Array,
	Map,
}
TypeMeta :: struct #raw_union {
	enum_ty:     EnumType,
	struct_ty:   StructType,
	union_ty:    UnionType,
	function_ty: FunctionType,
	array_ty:    ArrayType,
	map_ty:      MapType,
}
ArrayType :: struct {
	item_type: Type,
}
MapType :: struct {
	key_type:   Type,
	value_type: Type,
}
FunctionType :: struct {
	arg_types:   []Type,
	return_type: Type,
}
EnumType :: struct {
	variants: []Type,
}
EnumVariantInfo :: struct {
	ty:   Type, // should be a zero sized struct.
	name: string,
}
StructType :: struct {
	name:           Maybe(string),
	unnamed_fields: []UnnamedFieldInfo,
	named_fields:   []NamedFieldInfo,
	size:           int,
	is_numeric:     bool,
}
UnnamedFieldInfo :: struct {
	ty:     Type,
	offset: int,
}
NamedFieldInfo :: struct {
	name:   string,
	ty:     Type,
	offset: int,
}
UnionType :: struct {
	variants: []Type,
}


types_get :: proc(types: ^Types, structure: TypeStructure) -> Type {
	hash := type_hash_from_structure(structure)
	ty, has_ty := types.hash_to_types[hash]
	if has_ty {
		return ty
	} else {
		type := bucket_array_insert(&types.registered, TypeInfo{hash, structure})
		types.hash_to_types[hash] = type
		return type
	}
}
types_get_array_type :: proc(types: ^Types, item_type: Type) -> Type {
	arr_ty_structure := TypeStructure {
		tag = .Array,
		meta = {array_ty = ArrayType{item_type = item_type}},
	}
	return types_get(types, arr_ty_structure)
}


// calculates a typeid as a hash of the types structure
type_hash_from_structure :: proc(structure: TypeStructure) -> TypeHash {
	switch structure.tag {
	case .None:
		return NONE_TYPE_ID
	case .Bool:
		return BOOL_TYPE_ID
	case .Int:
		return INT_TYPE_ID
	case .Float:
		return FLOAT_TYPE_ID
	case .String:
		return STRING_TYPE_ID
	case .Char:
		return CHAR_TYPE_ID
	case .Array:
		array_ty := structure.meta.array_ty
		h: Hasher = hasher_init()
		hasher_add(&h, "#Array") //  todo: I think this is inefficient.
		hasher_add(&h, array_ty.item_type.hash)
		return TypeHash(hasher_finish(h))
	case .Map:
		map_ty := structure.meta.map_ty
		h: Hasher = hasher_init()
		hasher_add(&h, "#Map")
		hasher_add(&h, map_ty.key_type.hash)
		hasher_add(&h, map_ty.value_type.hash)
		return TypeHash(hasher_finish(h))
	case .Type:
		return TYPE_TYPE_ID
	case .Struct:
		struct_ty := structure.meta.struct_ty
		h: Hasher = hasher_init()
		hasher_add(&h, "#Struct")
		if struct_name, ok := struct_ty.name.(string); ok {
			hasher_add(&h, struct_name)
		}
		for uf in struct_ty.unnamed_fields {
			hasher_add(&h, uf.ty.hash)
		}
		for nf in struct_ty.named_fields {
			hasher_add(&h, nf.name)
			hasher_add(&h, nf.ty.hash)
		}
		return TypeHash(hasher_finish(h))
	case .Union:
		// todo: this is bullshit right now! needs to be order independent though!
		hash: TypeHash = 0
		union_ty := structure.meta.union_ty
		for variant in union_ty.variants {
			hash ~= variant.hash
		}
		return hash
	case .Enum:
		// todo: this is bullshit right now!
		hash: TypeHash = 0
		enum_ty := structure.meta.enum_ty
		for variant in enum_ty.variants {
			hash ~= variant.hash
		}
		return hash
	case .Function:
		fn_ty := structure.meta.function_ty
		h: Hasher = hasher_init()
		for arg_ty in fn_ty.arg_types {
			hasher_add(&h, arg_ty.hash)
		}
		hasher_add(&h, fn_ty.return_type.hash)
		return TypeHash(hasher_finish(h))
	}
	unreachable()
}

function_type_arg_types_hash :: proc(fn_ty: FunctionType) -> ArgTypesHash {
	h: Hasher = hasher_init()
	for arg_ty in fn_ty.arg_types {
		hasher_add(&h, arg_ty.hash)
	}
	return ArgTypesHash(hasher_finish(h))
}


typecheck_expression :: proc(
	using ctx: ^TypeCheckCtx,
	expr: ^Expression,
	wanted_type: Maybe(Type),
) {
	switch &ex in expr.kind {
	case InvalidExpression:
	// just don't assign any type, leave type as nil ptr
	case LogicalOr:
		typecheck_expression(ctx, ex.first, BOOL_TYPE)
		typecheck_expression(ctx, ex.second, BOOL_TYPE)
		expr.type = BOOL_TYPE
	case LogicalAnd:
		typecheck_expression(ctx, ex.first, BOOL_TYPE)
		typecheck_expression(ctx, ex.second, BOOL_TYPE)
		expr.type = BOOL_TYPE
	case Comparison:
		// for now it is probably fine to expect that the left and right side of the comparison are the same type
		typecheck_expression(ctx, ex.first, nil)
		comparand_type := ex.first.type
		assert(comparand_type != nil)
		for &other in ex.others {
			typecheck_expression(ctx, &other.expr, comparand_type)
		}
		expr.type = BOOL_TYPE
	case MathOp:
		expr.tag = .Value // math ops should not be possible between types e.g. `A :: int + string`
		typecheck_expression(ctx, ex.first, nil)
		typecheck_expression(ctx, ex.second, nil)
		if ex.first.type != nil && ex.second.type != nil {
			res_type := math_op_result_type(ex.first.type, ex.second.type)
			expr.type = res_type // could be nil if no math op possible between the types, that is ok.
		}
	case NegateExpression:
		expr.tag = .Value // no `-` negation on types, so this expr is always a value
		typecheck_expression(ctx, ex.inner, nil)
		expr.type = ex.inner.type
	// question: should we check if the inner type is even numeric here?
	// Without check the error occurs later, when it shows that the inner type is not numeric??
	// if ex.inner.type != nil {
	// 	    if numeric_type_tag(ex.inner.type) != .None {  expr.type = ex.inner.type  } 
	// }
	case NotExpression:
		expr.tag = .Value // no `!` logic on types, so this expr is always a value
		typecheck_expression(ctx, ex.inner, BOOL_TYPE)
		expr.type = BOOL_TYPE
	case CallOp:
	case IndexOp:
	case AccessOp:
	case Ident:
	case Primitive:
		// this logic assigns types and the correct tag (Value, Type, TypeAndValue) to all:
		// - literals, e.g.: 12842, 12.999, "Hello", 'A', true, false
		assert(ex.value.type != .None) // forbidden, parser should put type = .Type, data = {primitive_type = .None} instead.
		if ex.value.type == .Type {
			prim := ex.value.data.primitive_type
			expr.type = primitive_type_to_type(prim)
			if prim == .None {
				expr.tag = .TypeAndValue
			} else {
				expr.tag = .Type
			}
		} else {
			// if float wanted, but int literal given, that is okay, change the int literal into a float literal:
			// e.g. in `LEN: float : 2` the 2 becomes `2.0`
			if wanted, ok := wanted_type.(Type);
			   ok && wanted == FLOAT_TYPE && ex.value.type == .Int {
				val_as_float := f64(ex.value.data.int)
				ex.value = PrimitiveValue{.Float, {float = val_as_float}}
			}
			expr.type = primitive_type_to_type(ex.value.type)
			expr.tag = .Value
		}
	case StructLiteral:
		todo()
	case ArrayLiteral:
		typecheck_array_literal(ctx, expr, ex, wanted_type)
		return
	case MapLiteral:
		todo()
	case FunctionSignature:
		function_ty: FunctionType

		arg_types: []Type = make([]Type, len(ex.arg_types))
		any_arg_invalid: bool
		for &arg, i in ex.arg_types {
			typecheck_expression(ctx, &arg, TYPE_TYPE)
			if arg.type == nil {
				any_arg_invalid = true
			} else {
				arg_types[i] = arg.type
			}
			// if arg.
		}
		typecheck_expression(ctx, ex.return_type, TYPE_TYPE)
		if ex.return_type.type == nil || any_arg_invalid {
			delete(arg_types)
			return // cannot figure out function type
		}

		function_ty_structure := TypeStructure {
			tag = .Function,
			meta = {
				function_ty = FunctionType {
					arg_types = arg_types,
					return_type = ex.return_type.type,
				},
			},
		}
		expr.type = types_get(ctx.types, function_ty_structure)
		expr.tag = .Type // because this is just a signature, no actual value
	case FunctionDefinition:
		function_ty: FunctionType

		// check the arg types:
		arg_types: []Type = make([]Type, len(ex.args))
		any_arg_invalid: bool
		for &arg, i in ex.args {
			typecheck_expression(ctx, &arg.ty, TYPE_TYPE)
			if arg.ty.type == nil {
				any_arg_invalid = true
			} else {
				arg_types[i] = arg.ty.type
			}
		}
		// if there is a return type, check it, otherwise, act like NONE_TYPE would be return type
		return_ty: Type = nil
		if return_ty_expr, ok := ex.return_type.(^Expression); ok {
			typecheck_expression(ctx, return_ty_expr, TYPE_TYPE)
			return_ty = return_ty_expr.type
		} else {
			return_ty = NONE_TYPE
		}
		if return_ty == nil || any_arg_invalid {
			delete(arg_types)
			return // cannot figure out function type
		}
		function_ty_structure := TypeStructure {
			tag = .Function,
			meta = {function_ty = FunctionType{arg_types = arg_types, return_type = return_ty}},
		}
		// do not typecheck fn body yet, as that is a new scope.
		expr.type = types_get(ctx.types, function_ty_structure)
		expr.tag = .Value // because this is the actual instructions not a type
	case EnumDecl:
		expr.type = TYPE_TYPE
	case UnionDecl:
		union_ty: Type
		// todo
		expr.type = TYPE_TYPE
	}

	// todo! where to raise errors like this???
	// add some error because if some type was expected and there is now a mismatch:
	// if wanted, ok := wanted_type.(Type); ok {
	// 	if expr.type != wanted {
	// 		range := expression_token_range(expr^)
	// 		errors_add(
	// 			ctx.errors,
	// 			range,
	// 			"for expression ",
	// 			expression_to_string(expr^),
	// 			" type ",
	// 			type_to_string(wanted),
	// 			" was expected, but type ",
	// 			type_to_string(expr.type),
	// 			" was assigned",
	// 		)
	// 	}
	// }
}

typecheck_array_literal :: proc(
	ctx: ^TypeCheckCtx,
	expr: ^Expression,
	arr: ArrayLiteral, // aliases expr
	wanted_type: Maybe(Type),
) {
	n_values := len(arr.values)
	if wanted, ok := wanted_type.(Type); ok {
		// a specific array was wanted, e.g. `A : [int] : []` or `A : [int] = [2,2,4,3,5]`
		if wanted.tag == .Array {
			expr.tag = .Value
			wanted_item_ty := wanted.meta.array_ty.item_type
			expr.type = types_get_array_type(ctx.types, wanted_item_ty)
			for &val in arr.values {
				typecheck_expression(ctx, &val, wanted_item_ty)
			}
		} else if wanted.tag == .Type { 	// e.g. [int]
			expr.tag = .Type
			if n_values != 1 {
				error_expr(ctx.errors, expr, "Type wanted, got multi element array")
				return
			}
			// the one item should be a type itself:
			item := &arr.values[0]
			typecheck_expression(ctx, item, TYPE_TYPE)
			if item.type == nil {
				return
			}
			if !can_be_type(item.tag) {
				error_expr(ctx.errors, expr, "Value in single element array is not a type")
				return
			}
			// if the inner is indeed a valid type now, try register the corresponding array type:
			arr_ty_structure := TypeStructure {
				tag = .Array,
				meta = {array_ty = ArrayType{item_type = item.type}},
			}
			expr.type = types_get(ctx.types, arr_ty_structure)
			return
		} else {
			error_expr(
				ctx.errors,
				expr,
				tprint("Array literal is not wanted type ", type_to_string(wanted)),
			)
			return
		}
	} else {
		// array literal and nothing specific is wanted
		if n_values == 0 {
			error_expr(ctx.errors, expr, "type of 0-element array literal not known")
			return
		}
		// first element dictates the type
		first_val := &arr.values[0]
		typecheck_expression(ctx, first_val, nil) // type not restricted, could be whatever	
		// if first_val is nil, this checks the other values without any expectations,
		// otherwise expects that all other values are the same type
		for &el in arr.values[1:] {
			typecheck_expression(ctx, first_val, first_val.type)
		}
		if first_val.type == nil {
			return
		}
		assert(first_val.tag != .Unknown)
		expr.type = types_get_array_type(ctx.types, first_val.type)
		if n_values == 1 && can_be_type(first_val.tag) {
			expr.tag = .Type
		} else {
			expr.tag = .Value
		}
	}
}
// returns nil if there is no possible math op (+,-,*,/) between the two types
// if a math op is possible the result type is probably either a or b
math_op_result_type :: proc(a: Type, b: Type) -> (res_ty: Type) {
	// math ops should be possible between a and b if:  (can also change order of a and b)
	// a and b are scalars (int or float)
	// a is scalar, b is a numeric struct (structs where all fields are int, float or another numeric struct)
	// a and b are the same numeric struct // e.g. Vec3 :: {x: float, y: float, z: float} 
	// a is scalar, b is sequence of scalars or numeric structs
	// a is numeric struct, b is sequence of this numeric struct.

	// right now only support two scalars:
	a_num := numeric_type_tag(a)
	b_num := numeric_type_tag(b)

	if a_num == .Scalar && b_num == .Scalar {
		// two scalars (int or float), if one of them is float, the result of the op is also float, e.g. 3.0 * 2 = 6.0
		if a.tag == .Float || b.tag == .Float {
			return FLOAT_TYPE
		} else {
			return INT_TYPE
		}
	}
	return nil
	// todo: future, more complicated logic here, support math ops with structs, arrays, etc!!
}

NumericTypeTag :: enum {
	None,
	Scalar,
	NumericStruct,
	ArrayOfScalars,
	ArrayOfNumericStructs,
	// future: ArrayOfNumericArrays, // currently not supported, e.g. [[3,3,4], [3,129292992]] * 7
	// also unions where all variants are numeric could be multiplied by scalars, but this is
	// not supported yet.
}
numeric_type_tag :: proc(s: TypeStructure) -> NumericTypeTag {
	#partial switch s.tag {
	case .Int, .Float:
		return .Scalar
	case .Struct:
		if s.meta.struct_ty.is_numeric {
			return .NumericStruct
		}
	case .Array:
		item_numeric_kind := numeric_type_tag(s.meta.array_ty.item_type)
		if item_numeric_kind == .Scalar {
			return .ArrayOfScalars
		} else if item_numeric_kind == .NumericStruct {
			return .ArrayOfNumericStructs
		}
	}
	return .None
}


type_to_string :: proc(ty: Type) -> string {
	if ty == nil {
		return "UNKNOWN TYPE VALUE"
	}
	return tprint(ty.tag) // todo!
}


// /////////////////////////////////////////////////////////////////////////////
// SECTION: Builtin Types
// /////////////////////////////////////////////////////////////////////////////
NONE_TYPE_ID :: 0
NONE_TYPE_INFO := TypeInfo {
	hash = NONE_TYPE_ID,
	tag  = .None,
}
NONE_TYPE := &NONE_TYPE_INFO // should somehow elide to TYPE_TYPE because None can be type or value!

BOOL_TYPE_ID :: 1
BOOL_TYPE_INFO := TypeInfo {
	hash = NONE_TYPE_ID,
	tag  = .Bool,
}
BOOL_TYPE := &BOOL_TYPE_INFO

INT_TYPE_ID :: 2
INT_TYPE_INFO := TypeInfo {
	hash = INT_TYPE_ID,
	tag  = .Int,
}
INT_TYPE := &INT_TYPE_INFO

FLOAT_TYPE_ID :: 3
FLOAT_TYPE_INFO := TypeInfo {
	hash = FLOAT_TYPE_ID,
	tag  = .Float,
}
FLOAT_TYPE := &FLOAT_TYPE_INFO


STRING_TYPE_ID :: 4
STRING_TYPE_INFO := TypeInfo {
	hash = STRING_TYPE_ID,
	tag  = .String,
}
STRING_TYPE := &STRING_TYPE_INFO

CHAR_TYPE_ID :: 5
CHAR_TYPE_INFO := TypeInfo {
	hash = CHAR_TYPE_ID,
	tag  = .Char,
}
CHAR_TYPE := &CHAR_TYPE_INFO

// this type is the type of Foo in `Foo :: { hello: true }` or Foo :: int
TYPE_TYPE_ID :: 6
TYPE_TYPE_INFO := TypeInfo {
	hash = TYPE_TYPE_ID,
	tag  = .Type,
}
TYPE_TYPE := &TYPE_TYPE_INFO


// __TYPE_ARRAY_TYPE_STRUCTURE := TypeStructure {
// 	tag = .Array,
// 	meta = {array_ty = ArrayType{item_type = TYPE_TYPE}},
// }
// TYPE_ARRAY_TYPE_ID :: 6
// TYPE_ARRAY_TYPE_INFO := TypeInfo {
// 	hash      = type_hash_from_structure(__TYPE_ARRAY_TYPE_STRUCTURE),
// 	structure = __TYPE_ARRAY_TYPE_STRUCTURE,
// }
// TYPE_ARRAY_TYPE := &TYPE_ARRAY_TYPE_INFO

primitive_type_to_type :: proc(prim: PrimitiveType) -> Type {
	switch prim {
	case .None:
		return NONE_TYPE
	case .Bool:
		return BOOL_TYPE
	case .Int:
		return INT_TYPE
	case .Float:
		return FLOAT_TYPE
	case .String:
		return STRING_TYPE
	case .Char:
		return CHAR_TYPE
	case .Type:
		return TYPE_TYPE
	}
	unreachable()
}

builtin_types :: proc() -> (types: Types) {
	// Note: these builtin types do not use the bucket array for backing memory
	types.hash_to_types[NONE_TYPE_ID] = NONE_TYPE
	types.hash_to_types[BOOL_TYPE_ID] = BOOL_TYPE
	types.hash_to_types[INT_TYPE_ID] = INT_TYPE
	types.hash_to_types[FLOAT_TYPE_ID] = FLOAT_TYPE
	types.hash_to_types[STRING_TYPE_ID] = STRING_TYPE
	types.hash_to_types[CHAR_TYPE_ID] = CHAR_TYPE
	// types.hash_to_types[TYPE_ARRAY_TYPE_ID] = TYPE_ARRAY_TYPE // important to 
	// types.id_to_type_info[UNKNOWN_TYPE_ID] = UNKNOWN_TYPE
	return types
}
