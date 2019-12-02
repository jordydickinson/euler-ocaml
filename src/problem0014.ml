(*
  The following iterative sequence is defined for the set of positive integers:

    n → n/2 (n is even)
    n → 3n + 1 (n is odd)

  Using the rule above and starting with 13, we generate the following sequence:

    13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

  It can be seen that this sequence (starting at 13 and finishing at 1) contains
  10 terms. Although it has not been proved yet (Collatz Problem), it is thought
  that all starting numbers finish at 1.

  Which starting number, under one million, produces the longest chain?

  NOTE: Once the chain starts the terms are allowed to go above one million. 
 *)

open Core_kernel

let memo_rec t f =
  let memo = Hashtbl.create t in
  let rec memo_fn x =
    Hashtbl.find_or_add memo x ~default:(fun () -> f memo_fn x)
  in
  memo_fn

let collatz_len =
  let collatz_len' recur n =
    if n = 1 then 1
    else if n % 2 = 0 then 1 + recur (n / 2)
    else 1 + recur (3*n + 1)
  in
  memo_rec (module Int) collatz_len'

let solve () =
  let (max_n, _) =
    Sequence.fold (Sequence.range 1 1_000_000) ~init:(0, 0) ~f:(
      fun (max_n, max_collatz_len_n) m ->
        let collatz_len_m = collatz_len m in
        if collatz_len_m > max_collatz_len_n
        then (m, collatz_len_m)
        else (max_n, max_collatz_len_n)
    )
  in
  string_of_int max_n
