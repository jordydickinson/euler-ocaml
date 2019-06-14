(*
  If we list all the natural numbers below 10 that are multiples of 3 or 5, we
  get 3, 5, 6 and 9. The sum of these multiples is 23.

  Find the sum of all the multiples of 3 or 5 below 1000.
 *)

open Core_kernel

let solve () =
  let rec loop n acc =
    if n >= 1000 then acc else
    if n % 3 = 0 || n % 5 = 0
    then loop (n + 1) (acc + n)
    else loop (n + 1) acc
  in
  loop 1 0 |> string_of_int
  (* 233168 *)
