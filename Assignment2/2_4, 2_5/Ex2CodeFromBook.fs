module Assignment2.Ex2CodeFromBook

type expr = 
  | CstI of int
  | Var of string
  | Let of string * expr * expr
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
let e1 = Let("z", CstI 17, Prim("+", Var "z", Var "z"));;

let rec minus (xs, ys) = 
    match xs with 
    | []    -> []
    | x::xr -> if mem x ys then minus(xr, ys)
               else x :: minus (xr, ys);;
               

type texpr =                            (* target expressions *)
  | TCstI of int
  | TVar of int                         (* index into runtime environment *)
  | TLet of texpr * texpr               (* erhs and ebody                 *)
  | TPrim of string * texpr * texpr;;

let rec getindex vs x = 
  match vs with 
  | []    -> failwith "Variable not found"
  | y::yr -> if x=y then 0 else 1 + getindex yr x;;
  
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
    
(* Storing intermediate results and variable bindings in the same stack *)

type sinstr =
  | SCstI of int                        (* push integer           *)
  | SVar of int                         (* push variable from env *)
  | SAdd                                (* pop args, push sum     *)
  | SSub                                (* pop args, push diff.   *)
  | SMul                                (* pop args, push product *)
  | SPop                                (* pop value/unbind var   *)
  | SSwap;;                             (* exchange top and next  *)
  
let rec seval (inss : sinstr list) (stack : int list) =
    match (inss, stack) with
    | ([], v :: _) -> v
    | ([], [])     -> failwith "seval: no result on stack"
    | (SCstI i :: insr,          stk) -> seval insr (i :: stk) 
    | (SVar i  :: insr,          stk) -> seval insr (List.nth stk i :: stk) 
    | (SAdd    :: insr, i2::i1::stkr) -> seval insr (i1+i2 :: stkr)
    | (SSub    :: insr, i2::i1::stkr) -> seval insr (i1-i2 :: stkr)
    | (SMul    :: insr, i2::i1::stkr) -> seval insr (i1*i2 :: stkr)
    | (SPop    :: insr,    _ :: stkr) -> seval insr stkr
    | (SSwap   :: insr, i2::i1::stkr) -> seval insr (i1::i2::stkr)
    | _ -> failwith "seval: too few operands on stack";;
    
type stackvalue =
  | Value                               (* A computed value *)
  | Bound of string;;                   (* A bound variable *)

(* Compilation to a list of instructions for a unified-stack machine *)

let rec scomp (e : expr) (cenv : stackvalue list) : sinstr list =
    match e with
    | CstI i -> [SCstI i]
    | Var x  -> [SVar (getindex cenv (Bound x))]
    | Let(x, erhs, ebody) -> 
          scomp erhs cenv @ scomp ebody (Bound x :: cenv) @ [SSwap; SPop]
    | Prim("+", e1, e2) -> 
          scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SAdd] 
    | Prim("-", e1, e2) -> 
          scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SSub] 
    | Prim("*", e1, e2) -> 
          scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SMul] 
    | Prim _ -> failwith "scomp: unknown operator";;

let intsToFile (inss : int list) (fname : string) = 
    let text = String.concat " " (List.map string inss)
    System.IO.File.WriteAllText(fname, text);;
