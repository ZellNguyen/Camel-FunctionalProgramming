open Asglexer
open Asgparser
open Asginterpreter

let rec collect_from buffer =
	match lexer buffer with
	| EOF -> [EOF]
	| token -> token :: collect_from buffer

let rec collect_from buffer =
	match lexer buffer with
	| EndOfFile -> [EndOfFile]
	| token -> token :: collect_from buffer

let eval str =
	let buffer = Lexing.from_string str in
	let tokens = collect_from buffer in
	match parse tokens with
	| None -> "parse error"
	| Some e -> interpret e |> string_of_int

let _ = Array.iteri (
	fun i expr -> if i<>0 then Printf.printf "%s\n" (eval expr) else ()
) Sys.argv