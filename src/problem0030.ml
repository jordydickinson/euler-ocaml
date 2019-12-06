(*
  Surprisingly there are only three numbers that can be written as the sum of
  fourth powers of their digits:

    1634 = 1^4 + 6^4 + 3^4 + 4^4
    8208 = 8^4 + 2^4 + 0^4 + 8^4
    9474 = 9^4 + 4^4 + 7^4 + 4^4

  As 1 = 1^4 is not a sum it is not included.

  The sum of these numbers is 1634 + 8208 + 9474 = 19316.

  Find the sum of all the numbers that can be written as the sum of fifth powers
  of their digits. 
 *)

open Core_kernel

let digits_of_int k =
  Sequence.unfold ~init:k
    ~f:(fun k -> if k = 0 then None else Some (k%10, k/10))

let powsum n xs =
  Sequence.sum (module Int) xs ~f:(fun x -> Int.pow x n)

let check_powsum n k =
  let digits = digits_of_int k in
  let powsum = powsum n digits in
  powsum = k

let solve () =
  let upper_bound = Int.pow 9 5 * 6 in
  (* ^ 9^5*6 has 6 digits. 9^5*7 also has 6 digits. *)
  Sequence.range 2 upper_bound (* Exclude 1 = 1^5. *)
  |> Sequence.filter ~f:(check_powsum 5)
  |> Sequence.sum (module Int) ~f:ident
  |> string_of_int
