(*
  In the 20×20 grid below, four numbers along a diagonal line have been marked.

  08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
  49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
  81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
  52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
  22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
  24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
  32 98 81 28 64 23 67 10[26]38 40 67 59 54 70 66 18 38 64 70
  67 26 20 68 02 62 12 20 95[63]94 39 63 08 40 91 66 49 94 21
  24 55 58 05 66 73 99 26 97 17[78]78 96 83 14 88 34 89 63 72
  21 36 23 09 75 00 76 44 20 45 35[14]00 61 33 97 34 31 33 95
  78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
  16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
  86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
  19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
  04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
  88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
  04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
  20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
  20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
  01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48

  The product of these numbers is 26 × 63 × 78 × 14 = 1788696.

  What is the greatest product of four adjacent numbers in the same direction
  (up, down, left, right, or diagonally) in the 20×20 grid?
 *)

open Core_kernel

let grid =
  {|
    08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
    49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
    81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
    52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
    22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
    24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
    32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
    67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
    24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
    21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
    78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
    16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
    86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
    19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
    04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
    88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
    04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
    20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
    20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
    01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
  |}
  |> String.split_lines
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> not @@ String.is_empty s)
  |> Array.of_list_map ~f:(
    fun line ->
      String.split line ~on:' '
      |> Array.of_list_map ~f:int_of_string
  )

(** Get the spans of length `len` of the list `xs`. *)
let spans len xs =
  let rec spans' accum len xs =
    let new_span = List.take xs len in
    if List.length new_span < len
    then accum (* There weren't enough elements in the list to make a new span. *)
    else spans' (new_span :: accum) len (List.drop xs 1)
  in
  spans' [] len xs
  |> List.rev (* spans' prepends to the list, so we have to reverse it. *)

(** "Weave" through the grid to produce a list of left-to-right rows. *)
let weave_right grid =
  Array.to_list grid
  |> List.map ~f:Array.to_list

(** "Weave" through the grid to produce a list of top-to-bottom columns *)
let weave_down grid =
  let nrows = Array.length grid in
  let ncols = Array.length grid.(0) in
  List.init ncols ~f:(
    fun col_i ->
      List.init nrows ~f:(fun row_j -> grid.(row_j).(col_i))
  )

(** "Weave" through the grid to produce a list of top-left-to-bottom-right diagonals. *)
let weave_diagonal_s grid =
  let nrows = Array.length grid in
  let ncols = Array.length grid.(0) in
  let ndiags = nrows + ncols - 1 in
  List.init ndiags ~f:(
    fun diag_i ->
      (* I must admit I find myself very clever for this. I suggest you draw it
         out on a piece of paper for a few values of diag_i. *)
      let start_row = (diag_i - 1) % 2 * diag_i / 2 in
      let start_col = (diag_i % 2) * (diag_i + 1) / 2 in
      let diag_len = min (ncols - start_col) (nrows - start_row) in
      List.init diag_len (
        fun diag_i_j ->
          grid.(start_row + diag_i_j).(start_col + diag_i_j)
      )
  )

(** "Weave" through the grid to produce a list of top-right-to-bottom-left diagonals. *)
let weave_diagonal_z grid =
  let nrows = Array.length grid in
  let ncols = Array.length grid.(0) in
  let ndiags = nrows + ncols - 1 in
  List.init ndiags ~f:(
    fun diag_i ->
      (* A variation on weave_diagonal_s. *)
      let start_row = (diag_i - 1) % 2 * diag_i / 2 in
      let start_col = (ncols - 1) - diag_i % 2 * (diag_i + 1) / 2 in
      let diag_len = min (start_col + 1) (nrows - start_row) in
      List.init diag_len (
        fun diag_i_j ->
          grid.(start_row + diag_i_j).(start_col - diag_i_j)
      )
  )

let max_span weave_fn =
  weave_fn grid
  |> List.map ~f:(
    fun xs ->
      spans 4 xs
      |> List.map ~f:(List.fold ~init:1 ~f:( * ))
      |> List.fold ~init:0 ~f:max
  )
  |> List.fold ~init:0 ~f:max

let solve () =
  let max_right = max_span weave_right in
  let max_down = max_span weave_down in
  let max_diagonal_s = max_span weave_diagonal_s in
  let max_diagonal_z = max_span weave_diagonal_z in
  max max_right @@
  max max_down @@
  max max_diagonal_s max_diagonal_z
  |> string_of_int
  (* 70600674 *)
