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
type_check :: proc(mod: ^Module) -> Err {
	constants: Constants = collect_const_declarations(mod.statements) or_return
	return nil
}


Namespace :: struct {
	constants: map[string][dynamic]NamespaceConstant,
}
NamespaceConstant :: struct {
	ty:    Type,
	value: ConstantValue,
}
ConstantValue :: union {
	int,
	float,
	string,
	bool,
}


Type :: ^TypeInfo
Types :: struct {
	registered: BucketArray(TypeInfo),
	ids:        map[TypeId]^TypeInfo,
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
}
TypeMeta :: struct #raw_union {
	enum_ty:   EnumType,
	struct_ty: StructType,
	union_ty:  UnionType,
	type_ty:   Type,
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
TypeId :: distinct u64

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
			hasher_add_fixed_size(&h, uf.ty.id)
		}
		for nf in struct_ty.named_fields {
			hasher_add_string(&h, nf.name)
			hasher_add_fixed_size(&h, nf.ty.id)
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
	}
	unreachable()
}

base_types :: proc() -> (types: Types) {
	types.ids[NONE_TYPE_ID] = NONE_TYPE
	types.ids[BOOL_TYPE_ID] = BOOL_TYPE
	types.ids[INT_TYPE_ID] = INT_TYPE
	types.ids[FLOAT_TYPE_ID] = FLOAT_TYPE
	types.ids[STRING_TYPE_ID] = STRING_TYPE
	types.ids[CHAR_TYPE_ID] = CHAR_TYPE
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

none_type :: proc(types: ^Types) -> Type {
	todo()
}

bool_type :: proc(types: ^Types) -> Type {
	todo()
}

int_type :: proc(types: ^Types) -> Type {
	todo()
}


float_type :: proc(types: ^Types) -> Type {
	todo()
}

Constants :: struct {
	names:  map[string]^Declaration,
	order:  [dynamic]^Declaration,
	cycles: [dynamic]^Declaration,
}

collect_const_declarations :: proc(statements: []Statement) -> (constants: Constants, err: Err) {
	ConstWithDeps :: struct {
		decl:         ^Declaration,
		dependants:   [dynamic]string,
		dependencies: int,
	}
	const_deps: map[string]ConstWithDeps
	// collect the constants, making sure no name is declared twice
	for &s in statements {
		if decl, ok := &s.(Declaration); ok {
			name := decl.ident.name
			if name in constants.names {
				return {}, tprint("Constant ", name, " declared twice!")
			}
			const_deps[name] = ConstWithDeps {
				decl = decl,
			}
		}
	}
	print("constants:")
	for name, c in const_deps {
		print(name)
		// print(name, c.decl)
	}
	return constants, nil
}


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

	case NegateExpression:
		add_type_to_expression_unrestricted(types, cast(^Expression)ex)
	case NotExpression:
		add_type_to_expression_expected(types, cast(^Expression)ex, BOOL_TYPE)
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
	case LitArray:
	case FunctionSignature:
	case FunctionDefinition:
	case EnumDecl:
	case LitUnionDecl:
		union_ty: Type
		// todo
		expr.type = TYPE_TYPE
	case LitNone:
		expr.type = TYPE_TYPE
	}
}

add_type_to_expression_expected :: proc(types: ^Types, expr: ^Expression, expected_type: Type) {
	todo()
}
