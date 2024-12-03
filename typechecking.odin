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
		case LitBool:
		case LitInt:
		case LitFloat:
		case LitString:
		case LitChar:
		case LitPrimitiveType:
		case LitStruct:
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
		case LitArray:
			for val in ex.values {
				_collect_expr(deps, val)
			}
		case LitMap:
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
		case LitEnumType:
		// nothing to do
		case LitUnionType:
			for var in ex.variants {
				_collect_expr(deps, var)
			}

		}
	}
}

topo_sort_constants_by_outer_dependencies :: proc(
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
					overload_groups[name] = overload_group()
				}
				dep_group: ^OverloadGroup = &overload_groups[name]
				if name not_in dep_group.dependents {
					group.n_dependencies += 1
					dep_group.dependents[name] = Empty{}
				}
			}
		}
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
				ty                 = nil,
				decl               = decl,
				value              = nil,
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

type_check :: proc(mod: ^Module) -> Err {
	types := new_clone(base_types())
	scope := new_clone(Scope{})
	scope.constants = collect_constants(mod.statements)
	sorted, cycled := topo_sort_constants_by_outer_dependencies(&scope.constants)
	/*

	collect all the constant declarations in the module and sort them by the names used within their definitions
	





	*/


	// constants: Constants = collect_const_declarations(mod.statements) or_return
	return nil
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
	ty:                 Type,
	decl:               ^Declaration,
	value:              ConstantValue,
	outer_dependencies: map[string]Empty, // idents found in the type def or value of the constant, NOT counting names that are referred to from the inside of a function in their own scope.
}
ConstantValue :: union {
	// nil means not evaluated yet
	int,
	float,
	string,
	bool,
	[]FunctionValue, // multiple, because we have function overloading
}

FunctionValue :: struct {
	arg_types_hash: ArgTypesHash,
	expr:           ^Expression,
}

ArgTypesHash :: distinct u64
/// the TypeId is a hash that follows directly from the structure of a type (which may include a global name, distinguishing e.g. different zero sized types)
TypeId :: distinct u64
UNKNOWN_TYPE: TypeId : TypeId(max(u64))
Type :: ^TypeInfo
Types :: struct {
	registered:      BucketArray(TypeInfo),
	id_to_type_info: map[TypeId]^TypeInfo,
}
TypeInfo :: struct {
	id:              TypeId,
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
}
TypeMeta :: struct #raw_union {
	enum_ty:     EnumType,
	struct_ty:   StructType,
	union_ty:    UnionType,
	type_ty:     Type,
	function_ty: FunctionType,
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


// calculates a typeid as a hash of the types structure
type_id_from_structure :: proc(structure: TypeStructure) -> TypeId {
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
	case .Type:
		return TYPE_TYPE_ID
	case .Struct:
		struct_ty := structure.meta.struct_ty
		h: Hasher = hasher_init()
		if struct_name, ok := struct_ty.name.(string); ok {
			hasher_add_string(&h, struct_name)
		}
		for uf in struct_ty.unnamed_fields {
			hasher_add_sized(&h, uf.ty.id)
		}
		for nf in struct_ty.named_fields {
			hasher_add_string(&h, nf.name)
			hasher_add_sized(&h, nf.ty.id)
		}
		return TypeId(hasher_finish(h))
	case .Union:
		hash: TypeId = 0
		union_ty := structure.meta.union_ty
		for variant in union_ty.variants {
			hash ~= variant.id
		}
		return hash
	case .Enum:
		hash: TypeId = 0
		enum_ty := structure.meta.enum_ty
		for variant in enum_ty.variants {
			hash ~= variant.id
		}
		return hash
	case .Function:
		fn_ty := structure.meta.function_ty
		h: Hasher = hasher_init()
		for arg_ty in fn_ty.arg_types {
			hasher_add_sized(&h, arg_ty)
		}
		hasher_add_sized(&h, fn_ty.return_type)
		return TypeId(hasher_finish(h))
	}
	unreachable()
}

function_type_arg_types_hash :: proc(fn_ty: FunctionType) -> ArgTypesHash {
	h: Hasher = hasher_init()
	for arg_ty in fn_ty.arg_types {
		hasher_add_sized(&h, arg_ty)
	}
	return ArgTypesHash(hasher_finish(h))
}

base_types :: proc() -> (types: Types) {
	types.id_to_type_info[NONE_TYPE_ID] = NONE_TYPE
	types.id_to_type_info[BOOL_TYPE_ID] = BOOL_TYPE
	types.id_to_type_info[INT_TYPE_ID] = INT_TYPE
	types.id_to_type_info[FLOAT_TYPE_ID] = FLOAT_TYPE
	types.id_to_type_info[STRING_TYPE_ID] = STRING_TYPE
	types.id_to_type_info[CHAR_TYPE_ID] = CHAR_TYPE
	return types
}

NONE_TYPE_ID :: 0
NONE_TYPE_INFO := TypeInfo {
	id  = NONE_TYPE_ID,
	tag = .None,
}
NONE_TYPE := &NONE_TYPE_INFO

BOOL_TYPE_ID :: 1
BOOL_TYPE_INFO := TypeInfo {
	id  = NONE_TYPE_ID,
	tag = .Bool,
}
BOOL_TYPE := &BOOL_TYPE_INFO

INT_TYPE_ID :: 2
INT_TYPE_INFO := TypeInfo {
	id  = INT_TYPE_ID,
	tag = .Int,
}
INT_TYPE := &INT_TYPE_INFO

FLOAT_TYPE_ID :: 3
FLOAT_TYPE_INFO := TypeInfo {
	id  = FLOAT_TYPE_ID,
	tag = .Float,
}
FLOAT_TYPE := &FLOAT_TYPE_INFO


STRING_TYPE_ID :: 4
STRING_TYPE_INFO := TypeInfo {
	id  = STRING_TYPE_ID,
	tag = .String,
}
STRING_TYPE := &STRING_TYPE_INFO

CHAR_TYPE_ID :: 5
CHAR_TYPE_INFO := TypeInfo {
	id  = CHAR_TYPE_ID,
	tag = .Char,
}
CHAR_TYPE := &CHAR_TYPE_INFO

// this type is the type of Foo in `Foo :: { hello: true }` or Foo :: int
TYPE_TYPE_ID :: 6
TYPE_TYPE_INFO := TypeInfo {
	id  = TYPE_TYPE_ID,
	tag = .Type,
}
TYPE_TYPE := &TYPE_TYPE_INFO

// Constants :: struct {
// 	names:  map[string]^Declaration,
// 	order:  [dynamic]^Declaration,
// 	cycles: [dynamic]^Declaration,
// }

// collect_const_declarations :: proc(statements: []Statement) -> (constants: Constants, err: Err) {
// 	ConstWithDeps :: struct {
// 		decl:         ^Declaration,
// 		dependants:   [dynamic]string,
// 		dependencies: int,
// 	}
// 	const_deps: map[string]ConstWithDeps
// 	// collect the constants, making sure no name is declared twice
// 	for &s in statements {
// 		if decl, ok := &s.(Declaration); ok {
// 			name := decl.ident.name
// 			if name in constants.names {
// 				return {}, tprint("Constant ", name, " declared twice!")
// 			}
// 			const_deps[name] = ConstWithDeps {
// 				decl = decl,
// 			}
// 		}
// 	}
// 	print("constants:")
// 	for name, c in const_deps {
// 		print(name)
// 		// print(name, c.decl)
// 	}
// 	return constants, nil
// }


add_type_to_expression :: proc(types: ^Types, expr: ^Expression, expected_type: Maybe(Type)) {
	if expected, ok := expected_type.(Type); ok {
		add_type_to_expression_expected(types, expr, expected)
	} else {
		add_type_to_expression_unrestricted(types, expr)
	}
}

add_type_to_expression_unrestricted :: proc(types: ^Types, expr: ^Expression) {
	switch &ex in expr.kind {
	case InvalidExpression:
		todo()
	case LogicalOr:
		add_type_to_expression_expected(types, ex.first, BOOL_TYPE)
		add_type_to_expression_expected(types, ex.second, BOOL_TYPE)
		expr.type = BOOL_TYPE
	case LogicalAnd:
		add_type_to_expression_expected(types, ex.first, BOOL_TYPE)
		add_type_to_expression_expected(types, ex.second, BOOL_TYPE)
		expr.type = BOOL_TYPE
	case Comparison:
		// for now it is probably fine to expect that the left and right side of the comparison are the same type
		add_type_to_expression_unrestricted(types, ex.first)
		comparand_type := ex.first.type
		assert(comparand_type != nil)
		for &other in ex.others {
			add_type_to_expression_expected(types, &other.expr, comparand_type)
		}
		expr.type = BOOL_TYPE
	case MathOp:
		// look up add fn maybe???
		todo()
	case NegateExpression:
		add_type_to_expression_unrestricted(types, ex.inner)
	case NotExpression:
		add_type_to_expression_expected(types, ex.inner, BOOL_TYPE)
		assert(expr.type == BOOL_TYPE)
	case CallOp:
	case IndexOp:
	case AccessOp:
	case Ident:
	// lookup ident in scopes
	case LitBool:
		expr.type = BOOL_TYPE
	case LitInt:
		expr.type = INT_TYPE
	case LitFloat:
		expr.type = FLOAT_TYPE
	case LitString:
		expr.type = STRING_TYPE
	case LitChar:
		expr.type = CHAR_TYPE
	case LitStruct:
		todo()
	case LitArray:
	case LitMap:
	case FunctionSignature:
	case FunctionDefinition:
	case LitEnumType:
	case LitUnionType:
		union_ty: Type
		// todo
		expr.type = TYPE_TYPE
	case LitPrimitiveType:
		expr.type = TYPE_TYPE // ??? maybe TYPE_TYPE_TYPE if it is Type?? No!!!!
	}
}

add_type_to_expression_expected :: proc(types: ^Types, expr: ^Expression, expected_type: Type) {
	todo()
}
