(* 1. Take in input of file from argument. *)
(* 2. Run scanner on input for expr *)
(* 3. Run parser on expr for parse tree *)
(* 4. Compile parse tree into java (for now just print out) *)

type action = Ast 
 
let read_file filename = 
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; []
with End_of_file ->
  close_in chan;
  List.rev !lines ;;

let _ =
  let action = if Array.length Sys.argv > 2 then
      List.assoc Sys.argv.(1) [ ("-a", Ast)]
    else  Ast in
  let src_file = if Array.length Sys.argv > 2 then
  	  Sys.argv.(2)
    else Sys.argv.(1) in
  let lexbuf = Lexing.from_channel (open_in src_file) in
  let program = Parser.program Scanner.token lexbuf in
  match action with
    Ast -> let listing = Ast.string_of_program program
           in print_string listing