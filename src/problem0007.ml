(*
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

  What is the 10 001st prime number?
 *)

open Core_kernel

let solve () =
  Sequence.take Numbers.primes 10001
  |> Sequence.to_list_rev
  |> List.hd_exn
  |> string_of_int
  (* 104743 *)
