(*
  Starting with the number 1 and moving to the right in a clockwise direction a
  5 by 5 spiral is formed as follows:

    21 22 23 24 25
    20  7  8  9 10
    19  6  1  2 11
    18  5  4  3 12
    17 16 15 14 13

  It can be verified that the sum of the numbers on the diagonals is 101.

  What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
  formed in the same way?
 *)

open Core_kernel

let spiral_sum len =
  let rec spiral_sum' i sum n delta =
    if i % 4 = 0 then
      if i/2 + 1 = len then sum + n else
      let delta = delta + 2 in
      spiral_sum' (i + 1) (sum + n) (n + delta) delta
    else
      spiral_sum' (i + 1) (sum + n) (n + delta) delta
  in
  spiral_sum' 0 0 1 0

let solve () = string_of_int @@
  spiral_sum 1001
