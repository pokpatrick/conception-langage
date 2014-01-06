type expr =
  | ASTNum of int
  | ASTIdent of string
  | ASTBool of bool
  | ASTNot of expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTEqual of expr * expr
  | ASTInf of expr * expr
  | ASTSup of expr * expr
  | ASTPlus of expr * expr
  | ASTMinus of expr * expr
  | ASTMult of expr * expr
  | ASTDiv of expr * expr
  | ASTPar of expr
;;

type expr_type =
  | ExprNum of float
  | ExprBool of bool
;;

type cmds =
  | ASTMove of expr
  | ASTTurn of expr
  | ASTSet of string * expr
  | ASTCall of string * expr list
  | ASTIf of expr * cmds list * cmds list
  | ASTExit
  | ASTWhile of expr * cmds list
  | ASTLoop of expr * cmds list
  | ASTTry of cmds list * string * cmds list
  | ASTRaise of string
  | ASTProc of string * string list * cmds list
  | ASTProcRec of string * string list * cmds list
  | ASTFun of string * string list * expr
  | ASTFunRec of string * string list * expr
  | ASTVar of string
;;

type prog =
  | ASTProg of cmds list
;;

let cmds_of_prog prog =
  match prog with
    | ASTProg(cmds) -> cmds
;;

