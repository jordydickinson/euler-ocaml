(*
  If the numbers 1 to 5 are written out in words: one, two, three, four, five,
  then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

  If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
  words, how many letters would be used?

  NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
  forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
  letters. The use of "and" when writing out numbers is in compliance with
  British usage.
*)

open Core_kernel

let rec phrase_of_int n =
  (* This is going to be tedious... *)
  match n with
  | 0 -> "zero"
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | 4 -> "four"
  | 5 -> "five"
  | 6 -> "six"
  | 7 -> "seven"
  | 8 -> "eight"
  | 9 -> "nine"
  | 10 -> "ten"
  | 11 -> "eleven"
  | 12 -> "twelve"
  | 13 -> "thirteen"
  | 14 -> "fourteen"
  | 15 -> "fifteen"
  | 16 -> "sixteen"
  | 17 -> "seventeen"
  | 18 -> "eighteen"
  | 19 -> "nineteen"
  | 20 -> "twenty"
  | 30 -> "thirty"
  | 40 -> "forty"
  | 50 -> "fifty"
  | 60 -> "sixty"
  | 70 -> "seventy"
  | 80 -> "eighty"
  | 90 -> "ninety"
  | n when n > 20 && n < 100 ->
    phrase_of_int (n - n % 10) ^ phrase_of_int (n % 10)
  | n when n >= 100 && n < 1_000 ->
    phrase_of_int (n / 100) ^ "hundred"
    ^ if n % 100 <> 0 then "and" ^ phrase_of_int (n % 100) else ""
  | 1000 -> "onethousand"
  | n -> invalid_argf "bad n: %d" n ()

let solve () =
  Sequence.range 1 1001
  |> Sequence.map ~f:phrase_of_int
  |> Sequence.fold ~init:0 ~f:(fun accum s -> accum + String.length s)
  |> string_of_int
