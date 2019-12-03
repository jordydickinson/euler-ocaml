(*
  In the United Kingdom the currency is made up of pound (£) and pence (p).
  There are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p).

  It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

  How many different ways can £2 be made using any number of coins?
 *)

open Core_kernel

let coins = [5; 10; 20; 50; 100; 200]

let count_ways pence =
  let table = Array.init (pence + 1) (fun pence -> pence/2 + 1) in
  List.iter coins ~f:(
      fun coin ->
        for p = 0 to pence do
          if p >= coin then
            table.(p) <- table.(p) + table.(p - coin)
        done
    );
  table.(pence)

let solve () = string_of_int @@
  count_ways 200
