(*
  2520 is the smallest number that can be divided by each of the numbers from 1
  to 10 without any remainder.

  What is the smallest positive number that is evenly divisible by all of the
  numbers from 1 to 20?
 *)

open Core_kernel

let rec gcd n m =
  if m = 0
  then n
  else gcd m (n%m)

let lcm n m =
  abs (n * m) / gcd n m

let solve () =
  List.range 1 20 ~stop:`inclusive
  |> List.fold ~init:1 ~f:lcm
  |> string_of_int
