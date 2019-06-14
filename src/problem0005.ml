(*
  2520 is the smallest number that can be divided by each of the numbers from 1
  to 10 without any remainder.

  What is the smallest positive number that is evenly divisible by all of the
  numbers from 1 to 20?
 *)

open Core_kernel

let rec divisible_thru i j n =
  if i > j then true
  else if n % i = 0 then divisible_thru (i + 1) j n
  else false

let solve () =
  let rec loop n =
    if divisible_thru 1 20 n then n
    else loop (n + 1) in
  loop 1 |> string_of_int
  (* 232792560 *)
