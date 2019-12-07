(*
  A unit fraction contains 1 in the numerator. The decimal representation of the
  unit fractions with denominators 2 to 10 are given:

    1/2	= 	0.5
    1/3	= 	0.(3)
    1/4	= 	0.25
    1/5	= 	0.2
    1/6	= 	0.1(6)
    1/7	= 	0.(142857)
    1/8	= 	0.125
    1/9	= 	0.(1)
    1/10	= 	0.1
  
  Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
  seen that 1/7 has a 6-digit recurring cycle.

  Find the value of d < 1000 for which 1/d contains the longest recurring cycle
  in its decimal fraction part.
 *)

open Core_kernel

let cycle_len d =
  let rec cycle_len' i n prevns =
    let r = (n*10)%d in
    if r = 0 then 0 else
    match Map.find prevns r with
    | None -> cycle_len' (i + 1) r (Map.add_exn prevns r i)
    | Some j -> i - j
  in
  cycle_len' 0 1 (Map.empty (module Int))

let solve () =
  let maxd (d1, len1) (d2, len2) =
    if len2 > len1
    then (d2, len2)
    else (d1, len1)
  in
  Sequences.count_from 2
  |> Sequence.take_while ~f:(fun d -> d < 1000)
  |> Sequence.map ~f:(fun d -> d, cycle_len d)
  |> Sequence.fold ~init:(0, 0) ~f:maxd
  |> fst |> string_of_int
