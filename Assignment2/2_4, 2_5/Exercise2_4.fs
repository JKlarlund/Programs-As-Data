module Exercise2_4

open Assignment2.Ex2CodeFromBook

(*
Exercise 2.4
*)
let assemble (e: sinstr list) : int list =
    let rec aux (e: sinstr list) l : int list =
        match e with
        | [] -> l
        | x::xs ->
            match x with
            | SCstI i -> aux xs (i::0::l)
            | SVar v -> aux xs (v::1::l)
            | SAdd -> aux xs (2::l)
            | SSub -> aux xs (3::l)
            | SMul -> aux xs (4::l)
            | SPop -> aux xs (5::l)
            | SSwap -> aux xs (6::l)
    List.rev (aux e [])
    
