open! Core

let show_card_list cards =
  List.fold cards ~init:"" ~f:(fun acc el ->
      sprintf "%s\n%s" acc (Card.show el))

