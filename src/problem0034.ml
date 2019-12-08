(*
  145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

  Find the sum of all numbers which are equal to the sum of the factorial of
  their digits.

  Note: as 1! = 1 and 2! = 2 are not sums they are not included.
 *)

open Core_kernel

(* Establishing an upper bound:

  We're interested in values A such that, given digits (a1, ..., an),
  A = a1! + ... an!. We know that 0 <= ai <= 9 for each i = 1, ..., n. From this
  we can say that

    n <= a1! + ... + an! <= n*9!

  Since A >= 10^(n-1), our upper bound determined by the greatest n such that

    10^(n-1) < n*9!
  
  This turns out to be n = 7, which gives us an upper bound of 7*9!.
 *)

let digit_fact = Array.init 10 ~f:Numbers.factorial

let is_factsum n =
  let digits = Util.digits_of_int n in
  let factsum = List.sum (module Int) digits ~f:(fun d -> digit_fact.(d)) in
  factsum = n

let solve () =
  let upper_bound = 7 * digit_fact.(9) in
  Sequence.range 3 upper_bound ~stop:`inclusive
  |> Sequence.filter ~f:is_factsum
  |> Sequence.sum (module Int) ~f:ident
  |> string_of_int
