(*
  A perfect number is a number for which the sum of its proper divisors is
  exactly equal to the number. For example, the sum of the proper divisors of 28
  would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

  A number n is called deficient if the sum of its proper divisors is less than
  n and it is called abundant if this sum exceeds n.

  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
  number that can be written as the sum of two abundant numbers is 24. By
  mathematical analysis, it can be shown that all integers greater than 28123
  can be written as the sum of two abundant numbers. However, this upper limit
  cannot be reduced any further by analysis even though it is known that the
  greatest number that cannot be expressed as the sum of two abundant numbers
  is less than this limit.

  Find the sum of all the positive integers which cannot be written as the sum
  of two abundant numbers.
 *)

open Core_kernel
open Numbers

let pdsum = pdsum_fn 28_124
let abundant n = pdsum n > n

let abundants =
  Sequence.range 12 28_124
  |> Sequence.filter ~f:abundant

let sums_tbl = Array.create 28123 false
let () =
  let set_sum n =
    if n - 1 < 28_123
    then sums_tbl.(n - 1) <- true
  in
  let rec populate prev next =
    match Sequence.hd next with
    | None -> ()
    | Some(cur) ->
      set_sum (cur + cur);
      List.iter prev (fun n -> set_sum (n + cur));
      populate (cur :: prev) (Sequence.tl_eagerly_exn next)
  in
  populate [] abundants

let solve () =
  Array.foldi sums_tbl ~init:0
    ~f:(fun i sum hellnaw -> if not hellnaw then sum + i + 1 else sum)
  |> string_of_int
