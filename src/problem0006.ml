(*
  The sum of the squares of the first ten natural numbers is,

    12 + 22 + ... + 102 = 385
  
  The square of the sum of the first ten natural numbers is,

    (1 + 2 + ... + 10)2 = 552 = 3025
  
  Hence the difference between the sum of the squares of the first ten natural
  numbers and the square of the sum is 3025 − 385 = 2640.

  Find the difference between the sum of the squares of the first one hundred
  natural numbers and the square of the sum.
 *)

open Core_kernel

let sum_of_squares_upto n =
  let squares = List.init n (fun i -> Int.pow (i + 1) 2) in
  List.fold squares ~init:0 ~f:(fun acc n -> acc + n)

let square_of_sum_upto n =
  let sum =
    let rec loop acc n =
      if n < 1 then acc else
      loop (acc + n) (n - 1)
    in
    loop 0 n
  in
  Int.pow sum 2

let solve () =
  (square_of_sum_upto 100) - (sum_of_squares_upto 100) |> string_of_int
  (* 25164150 *)
