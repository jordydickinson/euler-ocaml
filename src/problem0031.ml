(*
  In the United Kingdom the currency is made up of pound (£) and pence (p).
  There are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p).

  It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

  How many different ways can £2 be made using any number of coins?
 *)

open Core_kernel

let coins = [200; 100; 50; 20; 10; 5; 2; 1]

let count_ways pence =
  let rec count_ways' k pence coins accum =
    let coin = List.hd_exn coins in
    if coin = 2 then k (accum + pence/2 + 1) else
    let k = count_ways' k pence (List.tl_exn coins) in
    if coin <= pence
    then count_ways' k (pence - coin) coins accum
    else k accum
  in
  count_ways' ident pence coins 0

let solve () = string_of_int @@
  count_ways 200
