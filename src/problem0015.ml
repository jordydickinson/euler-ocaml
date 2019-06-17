(*
  Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down,
  there are exactly 6 routes to the bottom right corner.

    [Image omitted.]

  How many such routes are there through a 20×20 grid?
 *)

open Core_kernel

let lattice_paths n m =
  Numbers.binomial (n + m) m

let solve () =
  lattice_paths 20 20
  |> string_of_int
