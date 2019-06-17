(*
  215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

  What is the sum of the digits of the number 2^1000?
 *)

open Core_kernel

let solve () =
  (* Feels like cheating... *)
  Bigint.(of_int 2 ** of_int 1000 |> to_string)
  |> String.fold ~init:0 ~f:(fun accum c -> accum + (int_of_string @@ String.of_char c))
  |> string_of_int
