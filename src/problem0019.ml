(*
  You are given the following information, but you may prefer to do some
  research for yourself.

  - 1 Jan 1900 was a Monday.
  - Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
  - A leap year occurs on any year evenly divisible by 4, but not on a century
    unless it is divisible by 400.

  How many Sundays fell on the first of the month during the twentieth century
  (1 Jan 1901 to 31 Dec 2000)?
 *)

open Core_kernel

let is_leapyear y =
  if y mod 100 = 0
  then y mod 400 = 0
  else y mod 4 = 0

let days_in_year y =
  7*31 + 4*30 +
  if is_leapyear y
  then 29
  else 28

let days_in_month y m =
  match m with
  | 1 -> 31
  | 2 -> if is_leapyear y then 29 else 28
  | 3 -> 31
  | 4 -> 30
  | 5 -> 31
  | 6 -> 30
  | 7 -> 31
  | 8 -> 31
  | 9 -> 30
  | 10 -> 31
  | 11 -> 30
  | 12 -> 31
  | _ -> failwith "invalid month"

let solve () = 
  let rec count_sundays accum y m day_of_week =
    if y > 2000 then accum else
    let accum = if day_of_week = 0 then accum + 1 else accum in
    let day_of_week = (day_of_week + days_in_month y m) mod 7 in
    let y = if m = 12 then y + 1 else y in
    let m = (m mod 12) + 1 in
    count_sundays accum y m day_of_week
  in
  count_sundays 0 1901 1 ((1 + days_in_year 1900) mod 7)
  |> string_of_int
