module Solution

open Assignment1.Intro2

let max2 (a: int, b: int) : int =
    if a > b then a
    else b
    
let max3 (a: int, b: int, c: int) : int =
    let l = [b; c]
    let rec aux l curMax=
        match l with
        | [] -> curMax
        | ele::l ->
            if ele > curMax then aux l ele
            else aux l curMax
    aux l a
    
let isPositive (l: int list) : bool =
    List.forall (fun x -> if x>=0 then true else false) l

let isSorted (l: int list) : bool =
    List.fold (fun (acc, sol) e -> if e >= acc then (e, true) else (e, false)) (l.Head, true) l.Tail |> fun (acc, sol) -> sol

//From Appendix A
type inttree =
    | Lf
    | Br of int * inttree * inttree;;
//

let count (tree: inttree) : int =
    let rec aux tree =
        match tree with
        | Lf -> 0
        | Br(a, t1, t2) ->
            1 +  aux t1 + aux t2
    aux tree
    
let depth (tree: inttree) : int =
    let rec aux tree curDepth =
        match tree with
        | Lf -> 0
        | Br(a, t1, t2) ->
            max2 ((aux t1 curDepth+1), (aux t2 curDepth+1))
    aux tree 0
    
//From Appendix A
let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr
  
let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim _            -> failwith "unknown primitive";;
    

//This is the extended eval.

let min2 (a, b) =
    if a < b then a
    else b

let rec eval2 e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x
    | Prim("+", e1, e2) -> eval2 e1 env + eval2 e2 env
    | Prim("*", e1, e2) -> eval2 e1 env * eval2 e2 env
    | Prim("-", e1, e2) -> eval2 e1 env - eval2 e2 env
    | Prim("==", e1, e2) ->
        if eval2 e1 env = eval2 e2 env then 1
        else 0
    | Prim ("max", e1, e2) ->
        max2 (eval2 e1 env, eval2 e2 env)
    | Prim("min", e1, e2) ->
        min2 (eval2 e1 env, eval2 e2 env)
    | Prim _            -> failwith "unknown primitive";;

let rec eval3 e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x
    | If(e1, e2, e3) ->
        if eval3 e1 env <> 0 then eval3 e2 env else eval3 e3 env
    | Prim(ope, e1, e2) ->
        let i1 = e1
        let i2 = e2
        match ope with
        | "+" -> eval3 e1 env + eval3 e2 env
        | "-" -> eval3 e1 env - eval3 e2 env
        | "*" -> eval3 e1 env * eval3 e2 env
        | "==" ->
            if eval3 e1 env = eval3 e2 env then 1
            else 0
        | "max" ->
            max2 (eval3 e1 env, eval3 e2 env)
        | "min" ->
            min2 (eval3 e1 env, eval3 e2 env)
        | _ -> failwith "unknown primitive";;
//
        
type aexpr =
    | CstI of int
    | Var of string
    | Mul of aexpr * aexpr
    | Add of aexpr * aexpr
    | Sub of aexpr * aexpr
    
let exp1 = Sub(Var "v", Add(Var "w", Var "z"))

let exp2 = Add(Var "x", Add(Var "y", Add(Var "z", Var "w")))

let fmt (xpr: aexpr) : string =
    let rec aux xpr str =
        match xpr with
        | CstI i -> string i
        | Var s -> str + s
        | Mul(x, y) -> "(" + aux x "" + "*" + aux y "" + ")"
        | Add(x, y) -> "(" + aux x "" + "+" + aux y "" + ")"
        | Sub(x, y) -> "(" + aux x "" + "-" + aux y "" + ")"
    aux xpr ""
    
let helper a =
    match a with
    | Add(e1, e2) ->
        match e1, e2 with
        | CstI 0, _ ->
            e2
        | _, CstI 0 ->
            e1
        | CstI i, CstI j -> CstI (i+j)
        | _ -> Add(e1, e2)
    | Sub(e1, e2) ->
        match e1, e2 with
        | e1, e2 when e1=e2 -> CstI 0
        | _, CstI 0 -> e1
        | CstI 0, _ -> e2
        | CstI i, CstI j -> CstI (i+j)
        | _ -> Sub(e1, e2)
    | Mul(e1, e2) ->
        match e1, e2 with
        | _, CstI 0 -> CstI 0
        | CstI 0, _ -> CstI 0
        | CstI 1, _ -> e2
        | CstI i, CstI j -> CstI (i*j)
        | _, CstI 1 -> e1
        | _ -> Mul(e1, e2)
    | _ -> a

    
let rec simplify (xpr: aexpr) : aexpr =
    match xpr with
    | Add(e1, e2) ->
        match e1, e2 with
        | CstI i, CstI j ->
            CstI (i+j)
        | CstI i, _ ->
            if i = 0 then simplify e2
            else helper (Add(simplify e1, simplify e2))
        | _, CstI i ->
            if i = 0 then simplify e1
            else helper (Add(simplify e1, simplify e2))
        | e1, e2 ->
             helper (Add(simplify e1, simplify e2))
    | Sub(e1, e2) ->
        match e1, e2 with
        | CstI i, CstI j ->
            CstI (i-j)
        | CstI i, _ ->
            if i = 0 then simplify e2
            else helper (Sub(simplify e1, simplify e2))
        | _, CstI i ->
            if i=0 then simplify e1
            else helper (Sub(simplify e1, simplify e2))
        | e1, e2 ->
            if e1 = e2 then CstI 0
            else helper (Sub(simplify e1, simplify e2))
        //Note: Here, we will return 1 zero if both are 0.
    | Mul(e1, e2) ->
        match e1, e2 with
        | CstI i, CstI j ->
            CstI (i*j)
        | CstI i, _ ->
            match i with
            | 0 -> CstI 0
            | 1 -> simplify e2
            | _ -> helper (Mul(simplify e1, simplify e2))
        | _, CstI i ->
            match i with
            | 0 -> CstI 0
            | 1 -> simplify e1
            | _ -> helper (Mul(simplify e1, simplify e2))
        | e1, e2 ->
            helper (Mul(simplify e1, simplify e2))
    | _ -> xpr

//Not totally correct.
let differentiate (xpr: aexpr) (var: aexpr) : aexpr =
   
    let rec helper xpr =
        match xpr with
            | Mul(e1, e2) ->
            match e1, e2 with
                | Var x, Var y when Var x = var && Var y = var ->
                    Mul(CstI 2, Var x)
                | CstI i, Var x when Var x = var ->
                    CstI i
                | Var x, CstI i when Var x = var ->
                    CstI i
                | _, _ ->
                    Mul(e1, e2)
            | Add(e1, e2) ->
                match e1, e2 with
                | Var x, Var y when Var x = var && Var y = var ->
                    CstI 2
                | Var x, CstI i when Var x = var ->
                    CstI 1
                | CstI i, Var x when Var x = var ->
                    CstI 1
                | _, _ -> Add(e1, e2)
            | Sub(e1, e2) ->
                match e1, e2 with
                | Var x, Var y when Var x = var && Var y = var ->
                    CstI 0
                | Var x, CstI i when Var x = var ->
                    CstI 1
                | CstI i, Var x when Var x = var ->
                    CstI -1
                | _, _ -> Sub(e1, e2)
            | CstI i -> CstI 0
            | Var x when Var x = var -> CstI 1
            | _ -> xpr
                    
    
    let rec aux xpr =
        match xpr with
        | Mul(e1, e2) ->
            match e1, e2 with
            | Var x, Var y when Var x = var && Var y = var ->
                Mul(CstI 2, Var x)
            | CstI i, Var x when Var x = var ->
                CstI i
            | Var x, CstI i when Var x = var ->
                CstI i
            | _, _ ->
                helper (Mul(aux e1, aux e2))
        | Add(e1, e2) ->
            match e1, e2 with
            | Var x, Var y when Var x = var && Var y = var ->
                CstI 2
            | Var x, CstI i when Var x = var ->
                CstI 1
            | CstI i, Var x when Var x = var ->
                CstI 1
            | _, _ -> helper (Add(aux e1, aux e2))
        | Sub(e1, e2) ->
            match e1, e2 with
            | Var x, Var y when Var x = var && Var y = var ->
                CstI 0
            | Var x, CstI i when Var x = var ->
                CstI 1
            | CstI i, Var x when Var x = var ->
                CstI -1
            | _, _ -> helper (Sub(aux e1, aux e2))
        | CstI i -> CstI 0
        | Var x when Var x = var -> CstI 1
        | _ -> xpr
        
    match var with
    | Var _ -> aux (simplify xpr)
    | _ -> failwith "Can only differentiate with respect to a single variable."
    


