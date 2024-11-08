open! Core

let one_rot l =
  let rec iterate acc = function
    | [] -> []
    | [ x ] -> x :: List.rev acc
    | x :: l -> iterate (x :: acc) l
  in
  iterate [] l

let rec rotate n l = match n with 0 -> l | _ -> rotate (n - 1) (one_rot l)
