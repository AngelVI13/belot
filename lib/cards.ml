open! Core
open! Defs
open! Card
open! Poly

let show_card_list cards =
  List.fold cards ~init:"" ~f:(fun acc el ->
      sprintf "%s\n%s" acc (Card.show el))

let find_carre_combinations cards =
  let sorted =
    List.sort_and_group cards ~compare:(fun c1 c2 ->
        let c1_int = cvalue_to_enum @@ Card.value c1 in
        let c2_int = cvalue_to_enum @@ Card.value c2 in
        if c1_int < c2_int then -1 else if c1_int > c2_int then 1 else 0)
  in
  List.filter sorted ~f:(fun same_list -> List.length same_list = 4)

let find_consecutive_combinations cards =
  (* group all cards by suit *)
  let grouped =
    List.group cards ~break:(fun c1 c2 -> Card.suite c1 <> Card.suite c2)
  in
  (* withing each suite list sort cards by value *)
  let sorted =
    List.map grouped ~f:(fun c ->
        List.sort c ~compare:(fun c1 c2 ->
            let c1_int = cvalue_to_enum @@ Card.value c1 in
            let c2_int = cvalue_to_enum @@ Card.value c2 in
            if c1_int < c2_int then -1 else if c1_int > c2_int then 1 else 0))
  in
  (* withing each sorted suite list group cards consecutively increasing values *)
  let combinations =
    List.map sorted ~f:(fun cards ->
        List.group cards ~break:(fun c1 c2 ->
            let c1_int = cvalue_to_enum @@ Card.value c1 in
            let c2_int = cvalue_to_enum @@ Card.value c2 in
            c1_int + 1 <> c2_int))
  in
  (* filter out any suite combinations of less than 3 cards *)
  let combinations =
    List.map combinations ~f:(fun combos ->
        List.filter combos ~f:(fun c -> List.length c >= 3))
  in
  (* return a single list of all combinations *)
  List.fold combinations ~init:[] ~f:(fun acc el -> acc @ el)

let%expect_test "find_carre_combinations:no carre" =
  let cards =
    [
      Card.make SClubs Jack;
      Card.make SSpades Jack;
      Card.make SHearts Jack;
      Card.make SDiamonds Ace;
      Card.make SHearts Ten;
    ]
  in
  let combinations = find_carre_combinations cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_card_list el));
  [%expect {| |}]

let%expect_test "find_carre_combinations:carre of jacks" =
  let cards =
    [
      Card.make SClubs Jack;
      Card.make SSpades Jack;
      Card.make SHearts Jack;
      Card.make SDiamonds Jack;
      Card.make SHearts Ten;
    ]
  in
  let combinations = find_carre_combinations cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_card_list el));
  [%expect
    {|
    { Card.suite = Defs.SClubs; value = Defs.Jack }
    { Card.suite = Defs.SSpades; value = Defs.Jack }
    { Card.suite = Defs.SHearts; value = Defs.Jack }
    { Card.suite = Defs.SDiamonds; value = Defs.Jack }
    |}]

let%expect_test "find_consecutive_combinations:one combo" =
  let cards =
    [
      Card.make SClubs Jack;
      Card.make SDiamonds Queen;
      Card.make SHearts Nine;
      Card.make SHearts Ten;
      Card.make SHearts Jack;
      Card.make SHearts Queen;
      Card.make SHearts Ace;
      Card.make SSpades Jack;
    ]
  in
  let combinations = find_consecutive_combinations cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_card_list el));
  [%expect
    {|
    { Card.suite = Defs.SHearts; value = Defs.Nine }
    { Card.suite = Defs.SHearts; value = Defs.Ten }
    { Card.suite = Defs.SHearts; value = Defs.Jack }
    { Card.suite = Defs.SHearts; value = Defs.Queen }
    |}]

let%expect_test "find_consecutive_combinations:2 combos from different colors" =
  let cards =
    [
      Card.make SClubs Jack;
      Card.make SClubs Queen;
      Card.make SClubs King;
      Card.make SDiamonds Nine;
      Card.make SHearts Ten;
      Card.make SHearts Jack;
      Card.make SHearts Queen;
      Card.make SHearts Ace;
    ]
  in
  let combinations = find_consecutive_combinations cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_card_list el));
  [%expect
    {|
    { Card.suite = Defs.SClubs; value = Defs.Jack }
    { Card.suite = Defs.SClubs; value = Defs.Queen }
    { Card.suite = Defs.SClubs; value = Defs.King }

    { Card.suite = Defs.SHearts; value = Defs.Ten }
    { Card.suite = Defs.SHearts; value = Defs.Jack }
    { Card.suite = Defs.SHearts; value = Defs.Queen }
    |}]

let%expect_test "find_consecutive_combinations:2 combos from same color" =
  let cards =
    [
      Card.make SClubs Jack;
      Card.make SClubs King;
      Card.make SHearts Seven;
      Card.make SHearts Eight;
      Card.make SHearts Nine;
      Card.make SHearts Queen;
      Card.make SHearts King;
      Card.make SHearts Ace;
    ]
  in
  let combinations = find_consecutive_combinations cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_card_list el));
  [%expect
    {|
    { Card.suite = Defs.SHearts; value = Defs.Seven }
    { Card.suite = Defs.SHearts; value = Defs.Eight }
    { Card.suite = Defs.SHearts; value = Defs.Nine }

    { Card.suite = Defs.SHearts; value = Defs.Queen }
    { Card.suite = Defs.SHearts; value = Defs.King }
    { Card.suite = Defs.SHearts; value = Defs.Ace }
    |}]

let%expect_test "find_consecutive_combinations:no combos" =
  let cards =
    [
      Card.make SClubs Jack;
      Card.make SClubs King;
      Card.make SHearts Seven;
      Card.make SHearts Eight;
      Card.make SHearts Ten;
      Card.make SSpades Queen;
      Card.make SHearts King;
      Card.make SHearts Ace;
    ]
  in
  let combinations = find_consecutive_combinations cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_card_list el));
  [%expect {| |}]
