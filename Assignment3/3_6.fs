(*
In Parse.fs, I added:
*)

let compString (str: string) =
  scomp (fromString str) []
