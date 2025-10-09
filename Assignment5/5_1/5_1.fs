module Test

let merge (l1: int list) (l2: int list) : int list =
  let rec aux l1 l2 l' : int list =
    match l1, l2 with
    | [], [] -> List.rev l'
    | [], l2 -> (List.rev l') @ l2
    | l1, [] -> (List.rev l') @ l1
    | x1::l1s, x2::l2s ->
      if x1<x2 then aux l1s l2 (x1::l')
      else aux l1 l2s (x2::l')

  aux l1 l2 []
