
{ 
  open Parser
  let line=ref 1

}

let ws = (['\t' ' ']*)
let eol = '\n'
let lparen = '('
let rparen = ')'
let lbracket = '['
let rbracket = ']'
let semicol = ";"
let r_move = "MOVE"
let r_turn = "TURN"
let r_set = "SET"
let r_call = "CALL"
let r_if = "IF"
let r_then = "THEN"
let r_else = "ELSE"
let r_exit = "EXIT"
let r_while = "WHILE"
let r_loop = "LOOP"
let r_break = "BREAK"
let r_try = "TRY"
let r_with = "WITH"
let r_raise = "RAISE"
let r_var = "VAR"
let r_fun = "FUN"
let r_funrec = "FUNREC"
let r_proc = "PROC"
let r_procrec = "PROCREC"
let r_true = "TRUE"
let r_false = "FALSE"
let r_not = "NOT"
let r_and = "AND"
let r_or = "OR"
let r_exit = "EXIT"
let op_plus = '+'
let op_div = '/'
let op_mult = "*"
let op_minus = "-"
let equal = '='
let inf = '<'
let sup = '>'
let ident = (['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9'])*
let digit = ['0'-'9']
let num = (digit*)
let cmd_help = "HELP"
let cmd_quit = "QUIT"

rule token = parse
| ws {token lexbuf}
| eol {incr line; token lexbuf}
| eof {EOF}
| lparen {LPAREN}
| rparen {RPAREN}
| lbracket {LBRACKET}
| rbracket {RBRACKET}
| semicol {SEMICOL}
| r_move {MOVE}
| r_turn {TURN}
| r_set {SET}
| r_call {CALL}
| r_if {IF}
| r_then {THEN}
| r_else {ELSE}
| r_exit {EXIT}
| r_while {WHILE}
| r_loop {LOOP}
| r_break {BREAK}
| r_try {TRY}
| r_with {WITH}
| r_raise {RAISE}
| r_var {VAR}
| r_fun {FUN}
| r_funrec {FUNREC}
| r_proc {PROC}
| r_procrec {PROCREC}
| r_true {TRUE}
| r_false {FALSE}
| r_not {NOT}
| r_and {AND}
| r_or {OR}
| r_exit {EXIT}
| op_plus {PLUS} 
| op_div {DIV}
| op_minus {MINUS}
| op_mult {MULT}
| equal {EQUAL}
| inf {INF}
| sup {SUP}
|(['a'-'z''A'-'Z'])(['a'-'z''A'-'Z''0'-'9'])* 
			{IDENT(Lexing.lexeme lexbuf)}
| digit as n {NUM(int_of_string (Char.escaped n))}
| num as n {NUM(int_of_string n)}
| cmd_help {HELP}
| cmd_quit {QUIT}
| _ { failwith((Lexing.lexeme lexbuf) ^ ": mistake at line " ^ string_of_int !line)}
