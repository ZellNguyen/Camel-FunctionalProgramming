{
	type token =
	| LeftParenthese
	| RightParenthese
	| LiteralInteger of int
	| LiteralString of string
	| Var of string
	| Int
	| String
	| Void
	| Id of string
	| Into
	| Equal
	| Plus
	| Minus
	| Multiply
	| Divide
	| Of
	| Concat
	| Case
	| EOF
}

rule lexer = parse '(' { LeftParenthese }
	| ')' { RightParenthese }
	| ['0'-'9']+ as i { LiteralInteger (int_of_string i) }
	| '"'([^'"']+ as s)'"' { LiteralString s }
	| '?'(['A'-'z']['A'-'z' '0'-'9' '_']*[''']* as s) { Var s }
	| ("int") { Int }
	| ("string") { String }
	| ("void") { Void }
	| ("->") { Into }
	| '=' { Equal }
	| '+' { Plus }
	| '-' { Minus }
	| '*' { Multiply }
	| '/' { Divide }
	| ':' { Of }
	| '^' { Concat }
	| '|' { Case }
	| ['A'-'z']['A'-'z' '0'-'9' '_']*[''']* as s { Id s }
	| eof { EOF }
	| [' ' '\t' '\n' '\r'] { lexer lexbuf }