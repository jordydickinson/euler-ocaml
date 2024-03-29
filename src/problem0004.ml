(*
  A palindromic number reads the same both ways. The largest palindrome made
  from the product of two 2-digit numbers is 9009 = 91 × 99.

  Find the largest palindrome made from the product of two 3-digit numbers.
 *)

open Core_kernel

let is_palindrome str =
  let len = String.length str in
  let front = String.sub str 0 (len / 2) in
  let back = String.sub str (len / 2 + len mod 2) (len / 2) in
  [%equal: string] front (String.rev back)

let rec select2 xs =
  match xs with
  | []        -> []
  | x0 :: xs  -> (x0, x0) :: List.map xs (fun x -> (x0, x)) @ select2 xs

let solve () =
  let three_digit_nats = List.init 900 (fun i -> 999 - i) in
  let cart_prod = List.cartesian_product three_digit_nats three_digit_nats in
  let multiples = List.map cart_prod (fun (n1, n2) -> n1 * n2) in
  let palindromes = List.filter_map multiples (
      fun n ->
        let str = Int.to_string n in
        if is_palindrome str
        then Some(str)
        else None
    ) in
  let max_palindrome =
    List.max_elt (List.map palindromes Int.of_string) Int.compare
  in
  Option.value_exn max_palindrome |> string_of_int
  (* 906609 *)
