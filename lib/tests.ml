open! Core

let%expect_test "addition_game" =
  printf "%d" (1 + 2);
  [%expect {| 3 |}]
