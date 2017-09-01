open Asgparser

let rec itp expression =
	match expression with
	| IntegerExpression i -> i
	(* | Fun ( id=a; params=b ) -> interpret_function a b *)
	| Add (l,r) -> (itp l) + (itp r)
	| Sub (l,r) -> (itp l) - (itp r)
	| Mul (l,r) -> (itp l) * (itp r)
	| Div (l,r) -> (itp l) / (itp r)

let interpret prog =
	match prog with
	| Program { typedefs=a; valdefs=b; main=c } -> string_of_int (itp c)

