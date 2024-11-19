open Defs

type t = { suite : csuite; value : cvalue } [@@deriving show]

let make suite value = { suite; value }
let show = show
let is_trump card suite = card.suite = suite

let is_trump_by_game card chosen_game =
  match chosen_game with
  | GClubs -> card.suite = SClubs
  | GDiamonds -> card.suite = SDiamonds
  | GHearts -> card.suite = SHearts
  | GSpades -> card.suite = SSpades
  | _ -> failwith "can't be called on non color game"

let value card = card.value
let suite card = card.suite

let compare_by_value c1 c2 =
  assert (suite c1 = suite c2);
  let c1_int = cvalue_to_enum @@ value c1 in
  let c2_int = cvalue_to_enum @@ value c2 in
  if c1_int < c2_int then -1 else if c1_int = c2_int then 0 else 1
