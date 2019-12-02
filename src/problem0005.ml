(*
  2520 is the smallest number that can be divided by each of the numbers from 1
  to 10 without any remainder.

  What is the smallest positive number that is evenly divisible by all of the
  numbers from 1 to 20?
 *)

open Core_kernel

let multiplicity n d =
  let rec multiplicity' accum n =
    if n % d = 0
    then multiplicity' (accum + 1) (n/d)
    else accum
  in
  multiplicity' 0 n

let factors n =
  let rec factors' accum primes n =
    if n = 1 then accum else
    let p = Sequence.hd_exn primes in
    if p > n then accum else
    let primes = Sequence.tl_eagerly_exn primes in
    let m = multiplicity n p in
    if m = 0 then factors' accum primes n else
    let n = n/(m*p) in
    let accum = Map.add_exn accum p m in
    factors' accum primes n
  in
  factors' (Map.empty (module Int)) Numbers.primes n

let factors_union ps1 ps2 =
  Map.merge_skewed ps1 ps2 ~combine:(fun ~key p1 p2 -> max p1 p2)

let solve () =
  Sequence.range 2 21
  |> Sequence.map ~f:factors
  |> Sequence.fold ~init:(Map.empty (module Int)) ~f:factors_union
  |> Map.fold ~init:1
      ~f:(fun ~key ~data product -> product * Int.(key ** data))
  |> string_of_int
  (* 232792560 *)
