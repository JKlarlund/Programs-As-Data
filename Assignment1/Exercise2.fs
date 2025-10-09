module Assignment1.Exercise2
open Ex2CodeFromBook

(*
Exercise 2.1
*)
let rec createNewEnvironment l env : (string * int) list =
  match l with
  | [] -> env
  | (s, xpr)::xs ->
    let value = eval xpr env
    createNewEnvironment xs ((s, value)::env) //What if it isn't a variable?
and eval e (env : (string * int) list) : int =
  match e with
  | CstI i            -> i 
  | Var x             -> lookup env x 
  | Let(l, ebody) -> 
    let newEnv = createNewEnvironment l env
    eval ebody newEnv
  | Prim("+", e1, e2) -> eval e1 env + eval e2 env
  | Prim("*", e1, e2) -> eval e1 env * eval e2 env
  | Prim("-", e1, e2) -> eval e1 env - eval e2 env
  | Prim _            -> failwith "unknown primitive";;
  
(*
Exercise 2.2
*)


let rec freevars2 e : string list =
    match e with
    | CstI i -> []
    | Var x  -> [x]
    | Let(l, ebody) ->
          let freeVarsList = getDeclaredFreeVars l []
          let xs = List.fold (fun s x -> (fst x)::s) [] l
          union (freeVarsList, minus (freevars ebody, xs)) //Assuming that theres no lets in here. Don't assume that-
    | Prim(ope, e1, e2) -> union (freevars2 e1, freevars2 e2) //You can add two let expressions together. Don't know if that makes sense.
and getDeclaredFreeVars l res =
  match l with
  | [] -> res
  | x::xs ->
    match x with
    | s, xpr -> getDeclaredFreeVars xs ((addVar xpr []) @ res)
    
and addVar (e: expr) res =
  match e with
  | Var x -> x::res
  | Prim(ope, e1, e2) -> addVar e1 res @ addVar e2 res
  | _ -> []
  
(*
Exercise 2.3
*)
let rec tcomp (e : expr) (cenv : string list) : texpr =
    match e with
    | CstI i -> TCstI i
    | Var x  -> TVar (getindex cenv x)
    | Let(l, ebody) ->
      handleLet l cenv ebody
    | Prim(ope, e1, e2) -> TPrim(ope, tcomp e1 cenv, tcomp e2 cenv);
and handleLet l nextEnv ebody =
  match l with
  | x::xs ->
    match x with
    | (s, xpr) ->
      TLet(tcomp xpr nextEnv, handleLet xs (s::nextEnv) ebody)
  | [] -> tcomp ebody nextEnv