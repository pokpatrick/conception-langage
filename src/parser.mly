/* header */
%{
  open Ast
%}
%token LBRACKET RBRACKET
%token LPAREN RPAREN
%token SEMICOL
%token MOVE TURN SET CALL
%token IF THEN ELSE
%token WHILE LOOP
%token TRY WITH
%token EXIT BREAK RAISE
%token VAR IDENT
%token FUN FUNREC PROC PROCREC
%token TRUE FALSE
%token END EOF
%token HELP QUIT
%token <int> NUM
%token <string> IDENT

/* operators */
%token NOT AND OR
%token PLUS MINUS DIV MULT
%token INF SUP EQUAL

/* priority */
%left NOT AND OR
%left PLUS MINUS 
%left DIV MULT
%left INF SUP EQUAL

  /* types */
%start prog
%type <Ast.prog> prog
%%

  /* grammar */
prog:
| LBRACKET RBRACKET {ASTProg []}
| LBRACKET cmds RBRACKET {ASTProg $2}
;

cmds:
| cmd {[$1]}
| cmd SEMICOL cmds {$1::$3}
| dec SEMICOL cmds {$1::$3}
;

cmd:
| MOVE expr {ASTMove($2)}
| TURN expr {ASTTurn($2)}
| SET IDENT expr {ASTSet($2, $3)}
| CALL IDENT exprs {ASTCall($2, $3)};
| IF expr THEN prog ELSE prog {ASTIf($2, cmds_of_prog $4, cmds_of_prog $6)}
| EXIT {ASTExit}
| WHILE LPAREN expr RPAREN prog {ASTWhile($3, cmds_of_prog $5)}
| LOOP LPAREN expr RPAREN prog {ASTLoop($3, cmds_of_prog $5)}
| TRY prog WITH IDENT prog {ASTTry(cmds_of_prog $2, $4, cmds_of_prog $5)}
| RAISE IDENT {ASTRaise($2)}
;

dec:
| PROC IDENT idents EQUAL prog {ASTProc($2,$3, cmds_of_prog $5)}
| PROCREC IDENT idents EQUAL prog {ASTProcRec($2,$3, (cmds_of_prog $5))}
| FUN IDENT idents EQUAL expr {ASTFun($2, $3, $5)}
| FUNREC IDENT idents EQUAL expr {ASTFunRec($2, $3, $5)}
| VAR IDENT {ASTVar($2)}
;

expr:
| NUM {ASTNum($1)}
| IDENT {ASTIdent($1)}
| TRUE {ASTBool(true)}
| FALSE {ASTBool(false)};
| NOT expr {ASTNot($2)}
| expr AND expr {ASTAnd($1, $3)}
| expr OR expr {ASTOr($1, $3)}
| expr EQUAL expr {ASTEqual($1, $3)}
| expr INF expr {ASTInf($1, $3)}
| expr SUP expr {ASTSup($1, $3)}
| expr PLUS expr {ASTPlus($1, $3)}
| expr MINUS expr {ASTMinus($1, $3)} 
| expr MULT expr {ASTMult($1, $3)}
| expr DIV expr {ASTDiv($1, $3)}
| LPAREN expr RPAREN {ASTPar($2)}   
;

exprs:
| expr {[$1]}
| expr exprs {$1::$2}
;

idents:
| IDENT {[$1]}
| IDENT idents {$1::$2}
;

%%
(* end of grammar *)
