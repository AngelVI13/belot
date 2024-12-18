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
    List.filter sorted ~f:(fun same_list ->
        List.length same_list = 4
        (* do not carres cant be from Seven & Eight *)
        && cvalue_to_enum @@ Card.value (List.nth_exn same_list 0)
           > cvalue_to_enum Eight)
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
  let combinations =
    List.map combinations ~f:(fun combos ->
        List.fold combos ~init:[] ~f:(fun acc combo ->
            let c_len = List.length combo in
            let updated_combos =
              match c_len with
              | 8 ->
                  (* max cards in a hand is 8 -> split to tierce and quinte *)
                  let tierce, quinte = List.split_n combo 3 in
                  [ tierce; quinte ]
              | 6 | 7 ->
                  (* drop lowest X cards in order to form the highest quinte combo *)
                  [ List.drop combo (c_len - 5) ]
              | _ -> [ combo ]
            in
            acc @ updated_combos))
  in

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

(* NOTE:
   Detailed examplanation of the rules:
   https://computergames.alle.bg/%D0%B8%D0%B3%D1%80%D0%B8-%D1%81-%D0%BA%D0%B0%D1%80%D1%82%D0%B8/%D0%B1%D0%B5%D0%BB%D0%BE%D1%82/
   The combinations specifics are revealed at end of the game so
   the player doesn't know what the oponents combinations are
*)
let find_best_combination cards =
  let carre = find_carre_combinations cards in
  let num_carre = List.length carre in
  let consec = find_consecutive_combinations cards in
  match num_carre with
  | 2 -> carre (* no other combo is possible (no cards left)*)
  | 0 -> consec (* no carre -> only consecutive combos possible *)
  | 1 ->
      let carre_card =
        match List.nth_exn carre 0 with
        | Carre value -> cvalue_to_enum value
        | _ -> failwith "unexpected type"
      in
      (* remove any consecutive combinations that contain the carre card *)
      let consec =
        List.filter consec ~f:(fun combo ->
            let start_card, num_cards =
              match combo with
              | Tierce start_card -> (cvalue_to_enum start_card, 3)
              | Quarte start_card -> (cvalue_to_enum start_card, 4)
              (* Quarte is maximum combo that can be affected by a carre (4 + 4 cards) *)
              | _ -> failwith "unexpected combination type"
            in
            let end_card = start_card + num_cards in
            carre_card < start_card || carre_card > end_card)
      in
      carre @ consec
  | _ -> failwith (sprintf "allowed carre combos [0, 2] but got %d" num_carre)

let by_suite cards suite = List.filter cards ~f:(fun c -> Card.suite c = suite)

let descending_by_suite cards suite =
  let cards_target_suite = by_suite cards suite in
  (* sort in descending order (highest value first) *)
  let cards_by_power =
    List.rev @@ List.sort cards_target_suite ~compare:Card.compare_by_value
  in
  cards_by_power

let bigger_by_suite cards suite value =
  let rec bigger_by_suite_aux cards suite value bigger =
    match cards with
    | card :: cards ->
        if Card.value card > value then
          bigger_by_suite_aux cards suite value (card :: bigger)
        else bigger
    | [] -> bigger
  in

  let descending = descending_by_suite cards suite in
  bigger_by_suite_aux descending suite value []

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

let%expect_test "find_carre_combinations:carre of sevens & eights" =
  let cards =
    [
      Card.make SClubs Seven;
      Card.make SSpades Seven;
      Card.make SHearts Seven;
      Card.make SDiamonds Seven;
      Card.make SClubs Eight;
      Card.make SSpades Eight;
      Card.make SHearts Eight;
      Card.make SDiamonds Eight;
    ]
  in
  let combinations = find_carre_combinations cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {| |}]

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
    (Defs.Quinte Defs.Ten)
    |}]

let%expect_test "find_best_combination:2 carre" =
  let cards =
    [
      Card.make SClubs Queen;
      Card.make SSpades Queen;
      Card.make SHearts Queen;
      Card.make SDiamonds Queen;
      Card.make SHearts Nine;
      Card.make SSpades Nine;
      Card.make SClubs Nine;
      Card.make SDiamonds Nine;
    ]
  in
  let combinations = find_best_combination cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {|
    (Defs.Carre Defs.Nine)
    (Defs.Carre Defs.Queen)
    |}]

let%expect_test "find_best_combination:1 carre & no combos" =
  let cards =
    [
      Card.make SClubs Queen;
      Card.make SSpades Queen;
      Card.make SHearts Queen;
      Card.make SDiamonds Queen;
      Card.make SHearts Nine;
      Card.make SSpades Ten;
      Card.make SClubs King;
      Card.make SDiamonds Ace;
    ]
  in
  let combinations = find_best_combination cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {|
    (Defs.Carre Defs.Queen)
    |}]

let%expect_test "find_best_combination:1 carre & 1 unrelated combo" =
  let cards =
    [
      Card.make SClubs Queen;
      Card.make SSpades Queen;
      Card.make SHearts Queen;
      Card.make SDiamonds Queen;
      Card.make SHearts Seven;
      Card.make SHearts Eight;
      Card.make SHearts Nine;
      Card.make SHearts Ten;
    ]
  in
  let combinations = find_best_combination cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {|
    (Defs.Carre Defs.Queen)
    (Defs.Quarte Defs.Seven)
    |}]

let%expect_test "find_best_combination:1 carre & 1 affected combo" =
  let cards =
    [
      Card.make SClubs Queen;
      Card.make SSpades Queen;
      Card.make SDiamonds Queen;
      Card.make SHearts Queen;
      Card.make SHearts King;
      Card.make SHearts Ace;
      Card.make SHearts Jack;
      Card.make SDiamonds Ace;
    ]
  in
  let combinations = find_best_combination cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {| (Defs.Carre Defs.Queen) |}]

let%expect_test "find_best_combination:no carre & 2 combo" =
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
  let combinations = find_best_combination cards in
  printf "%s"
  @@ List.fold combinations ~init:"" ~f:(fun acc el ->
         sprintf "%s\n%s" acc (show_ccombination el));
  [%expect {|
    (Defs.Tierce Defs.Seven)
    (Defs.Quinte Defs.Ten)
    |}]
