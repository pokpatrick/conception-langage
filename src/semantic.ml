open Ast;;
open Memory;;
open Continuation;;
open Environnement;;

let move_turtle_to memory =
  Graphics.lineto
    (int_of_float (get_memory initial_memory (Memory("x"))))
    (int_of_float (get_memory initial_memory (Memory("y"))));
;; 

let rec expr_semantic (expr : expr) (environnement : environnement) (memory : memory) (continuation : continuation) =
  match expr with
    | ASTNum(n) -> ExprNum(float_of_int n)
    | ASTIdent(v) ->	
	(match (out_environnement environnement v) with
	   | Num(f) -> ExprNum(f)
	   | Adr(a) -> ExprNum(get_memory memory (Address(a)))
	   | _ -> failwith "Error expr_semantic ident")
    | ASTBool(b) -> ExprBool(b)
    | ASTNot(e) -> 
	(match (expr_semantic e environnement memory continuation) with
	   | ExprBool(b) -> ExprBool(not b)
	   | _ -> failwith "Error expr_semantic not")
    | ASTAnd(e1, e2) ->
	(match ((expr_semantic e1 environnement memory continuation),
		(expr_semantic e2 environnement memory continuation)) with
	   | (ExprBool(b1), ExprBool(b2)) -> ExprBool(b1 && b2)
	   | _ -> failwith "Error expr_semantic and")
    | ASTOr(e1, e2) ->
	(match ((expr_semantic e1 environnement memory continuation),
		(expr_semantic e2 environnement memory continuation)) with
	   | (ExprBool(b1), ExprBool(b2)) -> ExprBool(b1 || b2)
	   | _ -> failwith "Error expr_semantic or")
    | ASTEqual(e1, e2) ->
	(match ((expr_semantic e1 environnement memory continuation),
		(expr_semantic e2 environnement memory continuation)) with
	   | (ExprNum(e1), ExprNum(e2)) -> ExprBool(e1 = e2)
	   | (ExprBool(b1), ExprBool(b2)) -> ExprBool(b1 = b2)
	   | _ -> failwith "Error expr_semantic equal")
    | ASTInf(e1, e2) -> 
	(match ((expr_semantic e1 environnement memory continuation),
		(expr_semantic e2 environnement memory continuation)) with
	   | (ExprNum(e1), ExprNum(e2)) -> ExprBool(e1 < e2)
	   | _ -> failwith "Error expr_semantic inf")
    | ASTSup(e1, e2) ->
	(match ((expr_semantic e1 environnement memory continuation),
		(expr_semantic e2 environnement memory continuation)) with
	   | (ExprNum(e1), ExprNum(e2)) -> ExprBool(e1 > e2)
	   | _ -> failwith "Error expr_semantic sup")
    | ASTPlus(e1, e2) ->
	(match ((expr_semantic e1 environnement memory continuation),
		(expr_semantic e2 environnement memory continuation)) with
	   | (ExprNum(e1), ExprNum(e2)) -> ExprNum(e1 +. e2)
	   | _ -> failwith "Error expr_semantic plus")
    | ASTMinus(e1, e2) ->
	(match ((expr_semantic e1 environnement memory continuation),
		(expr_semantic e2 environnement memory continuation)) with
	   | (ExprNum(e1), ExprNum(e2)) -> ExprNum(e1 -. e2)
	   | _ -> failwith "Error expr_semantic minus")
    | ASTMult(e1, e2) ->
	(match ((expr_semantic e1 environnement memory continuation),
		(expr_semantic e2 environnement memory continuation)) with
	   | (ExprNum(e1), ExprNum(e2)) -> ExprNum(e1 *. e2)
	   | _ -> failwith "Error expr_semantic mult")
    | ASTDiv(e1, e2) ->
	(match ((expr_semantic e1 environnement memory continuation),
		(expr_semantic e2 environnement memory continuation)) with
	   | (ExprNum(e1), ExprNum(0.0)) -> failwith "Division by 0"
	   | (ExprNum(e1), ExprNum(e2)) -> ExprNum(e1 /. e2)
	   | _ -> failwith "Error expr_semantic div")
    | ASTPar(e) ->
	(match (expr_semantic e environnement memory continuation) with
	   | ExprNum(e) -> ExprNum(e)
	   | ExprBool(b) -> ExprBool(b))
;;

let float_of_expr expr_type = 
  match expr_type with
    | ExprNum(f) -> f
    | _ -> failwith "float_of_expr inconpatible type"
;;

let rec floatlist_of_expr rlist environnement mu continuation =
  match rlist with
    | [] -> []
    | head::tail ->
	(float_of_expr(expr_semantic head environnement mu continuation))::floatlist_of_expr tail environnement mu continuation
;;

let rec cmds_semantic cmds (environnement : environnement) (memory : memory) (continuation : continuation) =
  match cmds with
    | [] -> continuation memory
    | head::tail -> 
	cmd_semantic head environnement memory continuation;
	cmds_semantic tail environnement memory continuation
and cmd_semantic cmd (environnement :environnement) (memory : memory) (continuation : continuation) =
  match cmd with
    | ASTMove(e) ->
	let angle = get_memory memory (Memory("angle"))
	and distance = float_of_expr (expr_semantic e environnement memory continuation)
	in 
	  set_memory memory (Memory("x")) ((get_memory memory (Memory("x")) +. distance *. (sin angle)));
	  set_memory memory (Memory("y")) ((get_memory memory (Memory("y")) +. distance *. (cos angle)));
	  move_turtle_to memory;
	  continuation memory
    | ASTTurn(e) ->
	let angle = get_memory memory (Memory("angle"))
	and pi = atan 1.0 *. 4.0
	and distance = float_of_expr (expr_semantic e environnement memory continuation)
	in
	  set_memory memory (Memory("angle")) (angle +. (distance *. pi /. 180.) -. (pi *. 2.0));		 
	  continuation memory
    | ASTSet(v, e) ->
	(match (out_environnement environnement v) with
	   | Adr(s) ->
	       set_memory memory (Address(s)) (float_of_expr(expr_semantic e environnement memory continuation));
	       continuation memory
	   | _ -> failwith "Error accessing environnement")
    | ASTCall(name, exprs) -> 
	(match (out_environnement environnement name) with
	   | FProc(args, p) ->
	       let envprime =
		 extend_environnement environnement args (floatlist_of_expr exprs environnement memory continuation) in
		 cmds_semantic p envprime memory continuation
	   | _ -> failwith "Error cmd_semantic call")
    | ASTIf(e, p1, p2) ->
	(match (expr_semantic e environnement memory continuation) with
	   | ExprBool(true) -> cmds_semantic p1 environnement memory continuation
	   | ExprBool(false) -> cmds_semantic p2 environnement memory continuation
	   | _ -> failwith "Error cmd_semantic if")
    | ASTExit -> continuation memory
    | ASTWhile(e, p) ->
	let rec k m =
	  match (expr_semantic e environnement memory continuation) with
	    | ExprBool(true) -> cmds_semantic p environnement memory k
	    | ExprBool(false) -> continuation m
	    | _ -> failwith "Error cmd_semantic while"
	in
	  (k memory)
    | ASTLoop(e, p) ->
	if (float_of_expr (expr_semantic e environnement memory continuation) = 0.0) then
	  continuation memory
	else
	  (set_memory memory (Address("loop")) (float_of_expr(expr_semantic e environnement memory continuation) -. 1.0);
	   cmds_semantic p environnement memory continuation)
    | ASTTry(p1, s, p2) ->
	let k m = cmds_semantic p2 environnement memory continuation in
	  in_environnement environnement s (Cont k);
	  cmds_semantic p1 environnement memory continuation
    | ASTRaise(s) -> 
	(match (out_environnement environnement s) with
	   | Cont k -> continuation memory
	   | _ -> failwith "Error cmd_semantic raise")
    | ASTProc(pname, args, p) -> 
	in_environnement environnement pname (FProc(args, p));
	continuation memory
    | ASTProcRec(p, xs, ss) -> continuation memory
    | ASTFun(fname, args, exprs) ->
	in_environnement environnement fname (FFun(args, exprs));
	continuation memory
    | ASTFunRec(s, args, e) -> continuation memory
    | ASTVar(v) ->
	in_environnement environnement v (Adr v);
	new_memory memory (Address(v));
	continuation memory
;;

let rec prog_semantic prog =
  match prog with
    | ASTProg(p) -> cmds_semantic p initial_environnement initial_memory continuation
;;

