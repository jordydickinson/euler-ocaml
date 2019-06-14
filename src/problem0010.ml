(* 
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.
 *)

open Core_kernel

let solve () =
  Numbers.eratosthenes 2000000
  |> List.fold ~init:0 ~f:(fun sum p -> sum + p)
  |> string_of_int
  (* 142913828922 *)
