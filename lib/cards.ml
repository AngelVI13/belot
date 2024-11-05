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
  let carres =
    List.filter sorted ~f:(fun same_list -> List.length same_list = 4)
  in
  List.map carres ~f:(fun carre -> Carre (Card.value @@ List.nth_exn carre 0))

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
  (* split long combinations to parts -
     6 consecutive -> 1 Quinte and drop lowest card
     8 consecutive - > 1 Quinte and 1 Tierce
  *)
  (* TODO: finish this *)
  (*let combinations =*)
  (*  List.map combinations ~f:(fun combos ->*)
  (*      List.filter combos ~f:(fun c -> List.length c >= 3))*)
  (*in*)
  (* flatten to a single list of all combinations *)
  let combinations =
    List.fold combinations ~init:[] ~f:(fun acc el -> acc @ el)
  in
  (* transform list of combinations to combination types *)
  List.map combinations ~f:(fun combo ->
      let combo_len = List.length combo in
      let start_value = Card.value @@ List.nth_exn combo 0 in
      match combo_len with
      | 3 -> Tierce start_value
      | 4 -> Quarte start_value
      | 5 -> Quinte start_value
      | _ -> failwith @@ sprintf "unexpected combination length: %d" combo_len)

let find_best_combination cards =
  let carre = find_carre_combinations cards in
  let num_carre = List.length carre in
  let consec = find_consecutive_combinations cards in
  match num_carre with
  | 2 -> carre (* no other combo is possible (no cards left)*)
  | 0 -> consec (* no carre -> only consecutive combos possible *)
  | 1 ->
      failwith "not implemented"
      (* TODO: check if a card appears in both carre and combo and return only the highest of them both *)
  | _ -> failwith (sprintf "allowed carre combos [0, 2] but got %d" num_carre)

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
         sprintf "%s\n%s" acc (show_ccombination el));
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
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {| (Defs.Carre Defs.Jack) |}]

let%expect_test "find_carre_combinations:2 carre" =
  let cards =
    [
      Card.make SClubs Jack;
      Card.make SSpades Jack;
      Card.make SHearts Jack;
      Card.make SDiamonds Jack;
      Card.make SHearts Ten;
      Card.make SSpades Ten;
      Card.make SClubs Ten;
      Card.make SDiamonds Ten;
    ]
  in
  let combinations = find_carre_combinations cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {|
    (Defs.Carre Defs.Ten)
    (Defs.Carre Defs.Jack)
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
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {| (Defs.Quarte Defs.Nine) |}]

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
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {|
    (Defs.Tierce Defs.Jack)
    (Defs.Tierce Defs.Ten)
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
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {|
    (Defs.Tierce Defs.Seven)
    (Defs.Tierce Defs.Queen)
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
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {| |}]

let%expect_test "find_consecutive_combinations:8 consecutive from same color" =
  let cards =
    [
      Card.make SHearts Seven;
      Card.make SHearts Eight;
      Card.make SHearts Nine;
      Card.make SHearts Ten;
      Card.make SHearts Jack;
      Card.make SHearts Queen;
      Card.make SHearts King;
      Card.make SHearts Ace;
    ]
  in
  let combinations = find_consecutive_combinations cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {|
    (Defs.Tierce Defs.Seven)
    (Defs.Tierce Defs.Queen)
    |}]
