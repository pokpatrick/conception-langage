open Ast;;
open Semantic;;
open Memory;;

let parse_prog ic =
  let buffer = Lexing.from_channel ic in
    try
      Parser.prog Lexer.token buffer 
    with
      | Parsing.Parse_error -> 
	  (Printf.fprintf stderr "Syntaxe Error position %d\n" (buffer.Lexing.lex_curr_pos);
	   raise Parsing.Parse_error)
;;

let initialize_turtle angle x y =
  set_memory initial_memory (Memory("angle")) angle;
  set_memory initial_memory (Memory("x")) x;
  set_memory initial_memory (Memory("y")) y
;; 

let initialize() = 
  Graphics.open_graph " 640x480";
  initialize_turtle 0.0 (float_of_int (Graphics.size_x() / 2)) (float_of_int (Graphics.size_y() / 2));
  Graphics.moveto (int_of_float (get_memory initial_memory (Memory("x")))) (int_of_float (get_memory initial_memory (Memory("y"))))
;; 

let main file_name =
  let ic = open_in file_name in
    initialize();
    prog_semantic (parse_prog ic)
;;

if Array.length(Sys.argv) < 2 then
  output_string stderr "./logo <testfile>"
else
  ignore (main (Sys.argv.(1)));
flush stdout;
read_line()
;;



