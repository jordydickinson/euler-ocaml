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

let collatz_len =
  let rec collatz_len' accum n =
    if n = 1 then accum + 1
    else if n % 2 = 0 then collatz_len' (accum + 1) (n / 2)
    else collatz_len' (accum + 1) (3*n + 1)
  in
  Memo.general (collatz_len' 0)

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
