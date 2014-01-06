open Memory;;
open Continuation;;

type value =
  | Num of float
  | Bool of bool
  | FFun of (string list * Ast.expr)
  | FProc of (string list * Ast.cmds list)
  | Adr of string
  | Cont of (memory -> memory)
;;

type environnement = (string, value) Hashtbl.t;;

let initial_environnement : environnement = Hashtbl.create 1;;

let rec out_environnement (environnement : environnement) (id : string) =
  Hashtbl.find environnement id
;;

let in_environnement (environnement : environnement) (id : string) (value : value) =
  Hashtbl.add environnement id value
;;

let extend_environnement (environnement : environnement) (arguments : string list) (values : float list) =
  List.fold_left2
    (fun environnement -> fun argument -> fun value ->
       (in_environnement environnement argument (Num value);
	environnement))
    environnement arguments values
;;
