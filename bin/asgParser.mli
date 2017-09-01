type type_expression =
	| IntType
	| StringType
	| VoidType
	| Fun of type_expression * type_expression
	| NewType of string
and typedef = Types of typedef_node
and typedef_node = {
	id: string;
	constructors: constructor list;
} and constructor = {
	cons_id: string;
	type_of_id: type_expression;
}

type pattern =
	| IntegerPattern of int
	| StringPattern of string
	| ()
	| VarPattern of string
	| FunPattern of pattern_node
and pattern_node = {
	id: string;
	params: pattern list;
}

type expression =
	| IntegerExpression of int
	| StringExpression of string
	| Fun of expression_node
	| Add of expression * expression
	| Sub of expression * expression
	| Mul of expression * expression
	| Div of expression * expression
	| Ccat of expression * expression
and expression_node = {
	id: string;
	params: expression list;
}

type equation = Equation of equation_node
and equation_node = {
	id: string;
	patterns: assignment_node;
} and assignment_node = {
	params: pattern list;
	val_assignment: expression;
}

type program = Program of program_node
and program_node = {
	typedefs: typedef list;
	valdefs: equation list;
	main: expression;
}

val parse: Asglexer.token list -> program option