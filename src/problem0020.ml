(*
  n! means n × (n − 1) × ... × 3 × 2 × 1

  For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
  and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

  Find the sum of the digits in the number 100!
 *)

open Core_kernel

let factorial n =
  (* This is what we in the industry call "cheating". *)
  let open Bigint in
  let rec factorial' accum n =
    if n = one then accum else
    factorial' (accum * n) (n - one)
  in
  factorial' one n

let sum_digits n =
  String.fold (Bigint.to_string n) ~init:0
    ~f:(fun accum c -> accum + (int_of_char c) - (int_of_char '0'))

let solve () =
  let open Bigint in
  let n = factorial (of_int 100) in
  string_of_int (sum_digits n)
