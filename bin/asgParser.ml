(* open Asglexer *)

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

let rec parse_typedef typedefs =
	match typedefs with
	| (Id "type" :: Id _ :: Equal :: _) ->
		(
			match expr [] [] typedefs with
			| None -> None
			| Some (typedef,tail) ->
				Some (typedef,tail)
		)
	| _ -> None
and expr defined ret typedefs =
	match typedefs with
	| Id "type" :: Id id :: Equal :: tail ->
		if not (List.mem id defined) then
			(
				match expr1 (id :: defined) [] tail with
				| None -> None
				| Some (constructors,l) -> expr (id :: defined) (Types ({id=id; constructors=constructors}) :: ret) l
			)
		else None
	| _ -> Some (ret, typedefs)
and expr1 defined constructors tail =
	match tail with
	| Case :: Id a :: Of :: t ->
		(
			match expr2 defined t with
			| None -> None
			| Some (r, l) -> expr1 defined ({cons_id=a; type_of_id=r;} :: constructors) l
		)
	| _ -> Some (List.rev constructors, tail)
and expr2 defined tail = (*returns Some (type_expression, l)*)
	match tail with
	| (Int | String | Void | Id _ ) as b :: l ->
		(
			match expr5 defined b l with
			| Some (r, l) -> Some (r, l)
			| None ->
				(
					match b with
					| Int -> Some (IntType, l)
					| String -> Some (StringType, l)
					| Void -> Some (VoidType, l)
					| Id id -> if not (List.mem id defined) then None else Some (NewType (id), l)
					| _ -> None
				)
		)
	| (LeftParenthese :: l) ->
		(
			match expr3 defined l with
			| None -> None
			| Some (r, l) ->
				(
					match l with
					| RightParenthese :: l -> Some (r, l)
					| _ -> None
				)
		)
	| _ -> None
and expr5 defined left tail =
	match tail with
	| Into :: l -> expr3 defined tail
	| _ -> None
and expr3 defined tail =
	let call a l = expr4 defined a l
	in
	match tail with
	| Into :: Int :: l -> call IntType l
	| Into :: String :: l -> call StringType l
	| Into :: Void :: l -> call VoidType l
	| Into :: Id id :: l -> if not (List.mem id defined) then None else call (NewType (id)) l
	| _ -> None
and expr4 defined left tail =
	match tail with
	| Into :: l ->
		(
			match expr2 defined l with
			| None -> None
			| Some (r, l) -> Some (Fun (left, r), l)
		)
	| RightParenthese :: l -> Some (left, l)
	| _ -> Some (left, tail);;

(*  *)

let rec parse_pattern patterns =
	match patterns with
	| (LiteralInteger _ | LiteralString _ | Var _ | Id _ | LeftParenthese) :: l->
		(
			match expr patterns with
			| None -> None
			| Some (pattern,l) -> Some (pattern,l)
		)
	| _ -> None
and expr patterns =
	match patterns with
	| (LiteralInteger _ | LiteralString _ | Var _ | Id _ | LeftParenthese) :: l->
		(
			match expr1 [] patterns with
			| None -> None
			| Some (pattern,l) -> Some (pattern,l)
		)
	| _ -> None
and expr1 plist patterns =
	match patterns with
	| Id "main" :: _ -> Some (List.rev plist, patterns)
	| (LeftParenthese :: l) ->
		(
			match expr l with
			| None -> None
			| Some(r, l) ->
				(
					match l with
					| RightParenthese :: l -> expr1 (List.hd r :: plist) l
					| _ -> None
				)
		)
	| (LiteralInteger a :: l) -> expr1 (IntegerPattern (a) :: plist) l
	| (LiteralString a :: l) -> expr1 (StringPattern (a) :: plist) l
	| (Var a :: l) -> expr1 (VarPattern (a) :: plist) l
	| (Id func :: l) ->
		(
			match expr2 func l with
			| None -> None
			| Some (pattern,l) -> expr1 (FunPattern ({ id=func; params=pattern }) :: plist) l
		)
	| _ -> Some (List.rev plist, patterns)
and expr2 id patterns =
	match expr1 [] patterns with
	| None -> None
	| Some (plist, l) -> Some (plist, l)

(*  *)

let rec parse_expression expressions =
	match expressions with
	| (LiteralInteger _ | LiteralString _ | LeftParenthese) :: _ ->
		(
			match expr expressions with
			| None -> None
			| Some (r,l) ->
				(
					match l with
					| ( EOF | Id _ ) :: tail -> Some (r, l)
					| _ -> None
				)
		)
	| _ -> None
and expr expressions =
	match expressions with
	| (LiteralInteger _ | LiteralString _ | LeftParenthese) :: _ ->
		(
			match expr1 expressions with
			| None -> None
			| Some (r,l) ->
				match concat l r with
				| None -> None
				| Some (r,l) -> Some (r,l)
		)
	| _ -> None
and concat l r =
	match l with
	| Concat :: l ->
		(
			match expr l with
			| None -> None
			| Some (r', l) -> Some ((Ccat (r, r')), l)
		)
	| (EOF | RightParenthese | LiteralInteger _ | LiteralString _ | Id _) :: _ -> Some (r,l)
	| _ -> None
and expr1 expressions =
	match expressions with
	| (LiteralInteger _ | LiteralString _ | LeftParenthese) :: _ ->
		(
			match expr2 expressions with
			| None -> None
			| Some (r,l) ->
				match add_or_sub l r with
				| None -> None
				| Some (r,l) -> Some (r,l)
		)
	| _ -> None
and add_or_sub l r =
	match l with
	| Plus :: l ->
		(
			match expr1 l with
			| None -> None
			| Some (r', l) -> Some ((Add (r, r')), l)
		)
	| Minus :: l ->
		(
			match expr1 l with
			| None -> None
			| Some (r', l) -> Some ((Sub (r, r')), l)
		)
	| (EOF | RightParenthese | LiteralInteger _ | LiteralString _ | Id _ | Concat) :: _ -> Some (r,l)
	| _ -> None
and expr2 l =
	match l with
	| (LiteralInteger _ | LiteralString _ | LeftParenthese) :: _ ->
		(
			match expr3 l with
			| None -> None
			| Some (r,l) -> mul_or_div l r
		)
	| _ -> None
and mul_or_div l r =
	match l with
	| Multiply :: l ->
		(
			match expr2 l with
			| None -> None
			| Some (r', l) -> Some ((Mul (r, r')), l)
		)
	| Divide :: l ->
		(
			match expr2 l with
			| None -> None
			| Some (r', l) -> Some ((Div (r, r')), l)
		)
	| (EOF | RightParenthese | LiteralInteger _ | LiteralString _ | Id _ | Concat | Plus | Minus) :: _ -> Some (r,l)
	| _ -> None
and expr3 l =
	match l with
	| LiteralInteger i :: l -> Some (IntegerExpression i, l)
	| LiteralString i :: l -> Some (StringExpression i, l)
	| LeftParenthese :: l ->
		(
			match expr l with
			| None -> None
			| Some (r,l) ->
				match l with
				| RightParenthese :: l -> Some (r,l)
				| _ -> None
		)
	| _ -> None

(*  *)

let rec parse_valdef valdefs =
	match valdefs with
	| (Id _ :: tail ) ->
		(
			match expr [] valdefs with
			| None -> None
			| Some (valdef, tail) -> Some (valdef, tail)
		)
	| _ -> None
and expr ret valdefs =
	match valdefs with
	| (Id "main") :: _ -> Some (ret, valdefs)
	| (Id id :: tail ) ->
		(
			match parse_pattern tail with
			| None -> 
				(
					match tail with
					| Equal :: l ->
						(
							match expr1 tail with
							| None -> None
							| Some (expression, tail) -> expr (Equation ({id=id; patterns={params=[()]; val_assignment=expression}}) :: ret) tail
						)
					| _ -> None
				)
			| Some (patt, tail) ->
				match expr1 tail with
				| None -> None
				| Some (expression, tail) -> expr (Equation ({id=id; patterns={params=patt; val_assignment=expression}}) :: ret) tail
		)
	| _ -> Some (ret, valdefs)
and expr1 tail =
	match tail with
	| Equal :: l -> parse_expression l
	| l -> parse_expression l

(*  *)

let rec exist_val keyword equations =
	match equations with
	| Equation { id=a; patterns=b } :: tail -> if a=keyword then true else exist_val keyword tail
	| [] -> false

let rec exist_type keyword typedefs =
	match typedefs with
	| Types { id=a; constructors=b } :: tail -> 
		let rec check key l =
			match l with
			| { cons_id=a; type_of_id=b } :: l -> if a=key then true else check key l
			| [] -> false
		in
		if check keyword b then true else exist_type keyword tail
	| [] -> false

(* let num_of_arguments keyword typedefs = *)

let parse l =
	match parse_typedef l with
	| None -> None
	| Some (typedef_list, l) ->
		match parse_valdef l with
		| None -> None
		| Some (valdef_list, l) ->
				match l with
				| Id "main" :: l -> 
					(
						match parse_expression l with
						| None -> None
						| Some (expressions, l) -> Some (Program ({typedefs=typedef_list; valdefs=valdef_list; main=expressions}))
					)
				| _ -> None