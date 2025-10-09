module Assignment1.Program2

//From the book
type expr = 
  | CstI of int
  | Var of string
  | Let of (string * expr) list * expr
  | Prim of string * expr * expr;;
  
let rec lookup env x =
  match env with 
  | []        -> failwith (x + " not found")
  | (y, v)::r -> if x=y then v else lookup r x
  
let rec mem x vs = 
    match vs with
    | []      -> false
    | v :: vr -> x=v || mem x vr;;
    
let rec union (xs, ys) = 
    match xs with 
    | []    -> ys
    | x::xr -> if mem x ys then union(xr, ys)
               else x :: union(xr, ys);;
               
let rec freevars e : string list =
    match e with
    | CstI i -> []
    | Var x  -> [x]
    | Prim(ope, e1, e2) -> union (freevars e1, freevars e2);;

(* minus xs ys  is the set of all elements in xs but not in ys *)

let rec minus (xs, ys) = 
    match xs with 
    | []    -> []
    | x::xr -> if mem x ys then minus(xr, ys)
               else x :: minus (xr, ys);;
    
//

let rec aux l env : (string * int) list =
  match l with
  | [] -> env
  | (s, xpr)::xs ->
    let value = eval xpr env
    aux xs ((s, value)::env) //What if it isn't a variable?
and eval e (env : (string * int) list) : int =
  match e with
  | CstI i            -> i 
  | Var x             -> lookup env x 
  | Let(l, ebody) -> 
    let newEnv = aux l env
    eval ebody newEnv
  | Prim("+", e1, e2) -> eval e1 env + eval e2 env
  | Prim("*", e1, e2) -> eval e1 env * eval e2 env
  | Prim("-", e1, e2) -> eval e1 env - eval e2 env
  | Prim _            -> failwith "unknown primitive";;
//
let res =
  let firstTuple = ("x", Prim("+", CstI(5), CstI(7)))
  let secondTuple = ("y", Prim("*", Var "x", CstI(2)))
  let ebody = Prim("+", Var "x", Var "y")
  let list = [firstTuple; secondTuple]
  let l = Let(list, ebody)
  let result = eval l []
  //let result = eval (Let("x", Prim("+", CstI(5), CstI(7))::("x2", Prim("*", Var "x", CstI(2)))::[])), Prim("+", Var "x", Var "x2"))) []
  result
  
let rec freevars2 e : string list =
    match e with
    | CstI i -> []
    | Var x  -> [x]
    | Let(l, ebody) ->
          let freeVarsList = aux2 l []
          let xs = List.fold (fun s x -> (fst x)::s) [] l
          union (freeVarsList, minus (freevars ebody, xs)) //Assuming that theres no lets in here. Don't assume that-
    | Prim(ope, e1, e2) -> union (freevars2 e1, freevars2 e2) //You can add two let expressions together. Don't know if that makes sense.
and aux2 l res =
  match l with
  | [] -> res
  | x::xs ->
    match x with
    | s, xpr -> aux2 xs ((helper xpr []) @ res)
    
and helper (e: expr) res =
  match e with
  | Var x -> x::res
  | Prim(ope, e1, e2) -> helper e1 res @ helper e2 res
  | _ -> []
  
type texpr =                            (* target expressions *)
  | TCstI of int
  | TVar of int                         (* index into runtime environment *)
  | TLet of texpr * texpr               (* erhs and ebody                 *)
  | TPrim of string * texpr * texpr;;
//From the book
let rec getindex vs x = 
  match vs with 
  | []    -> failwith "Variable not found"
  | y::yr -> if x=y then 0 else 1 + getindex yr x;;

(* Compiling from expr to texpr *)

let rec tcomp (e : expr) (cenv : string list) : texpr =
    match e with
    | CstI i -> TCstI i
    | Var x  -> TVar (getindex cenv x)
    | Let(l, ebody) ->
      helper2 l cenv ebody
    | Prim(ope, e1, e2) -> TPrim(ope, tcomp e1 cenv, tcomp e2 cenv);
and helper2 l nextEnv ebody =
  match l with
  | x::xs ->
    match x with
    | (s, xpr) ->
      TLet(tcomp xpr nextEnv, helper2 xs (s::nextEnv) ebody)
  | [] -> tcomp ebody nextEnv
    
//let x = x + 6 in x + 3
//Let(x, x+6, x+3)
//cenv1: [x]
//TLet(tcomp (x+6) [], tcomp (x+3) [x])
//tcomp (x+6) - TPrim(+, index(x) doesnt exist!, 6)
//tcomp (x+3) - TPrim(+, index(x) exists!, 3)

//let x1 = 2 let x2 = 3 in x1 + x2


(* Evaluation of target expressions with variable indexes.  The
   run-time environment renv is a list of variable values (ints).  *)

let rec teval (e : texpr) (renv : int list) : int =
    match e with
    | TCstI i -> i
    | TVar n  -> List.nth renv n
    | TLet(erhs, ebody) -> 
      let xval = teval erhs renv
      let renv1 = xval :: renv 
      teval ebody renv1 
    | TPrim("+", e1, e2) -> teval e1 renv + teval e2 renv
    | TPrim("*", e1, e2) -> teval e1 renv * teval e2 renv
    | TPrim("-", e1, e2) -> teval e1 renv - teval e2 renv
    | TPrim _            -> failwith "unknown primitive";;
    
let res3 =
  let firstTuple = ("x", Prim("+", CstI(5), CstI(7)))
  let secondTuple = ("y", Prim("*", Var "x", CstI(2)))
  let ebody = Prim("+", Var "x", Var "y")
  let list = [firstTuple; secondTuple]
  let l = Let(list, ebody)
  let result = tcomp l []
  teval result []
  