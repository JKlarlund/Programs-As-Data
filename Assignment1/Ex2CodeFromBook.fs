module Assignment1.Ex2CodeFromBook

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