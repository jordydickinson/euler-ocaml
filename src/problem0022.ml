(*
  Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
  containing over five-thousand first names, begin by sorting it into
  alphabetical order. Then working out the alphabetical value for each name,
  multiply this value by its alphabetical position in the list to obtain a name
  score.

  For example, when the list is sorted into alphabetical order, COLIN, which is
  worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
  would obtain a score of 938 × 53 = 49714.

  What is the total of all the name scores in the file?

  [Note: names.txt is saved as data/p022_names.txt in the project root.]
 *)

open Core_kernel

let names =
  In_channel.read_all "data/p022_names.txt"
  |> String.strip
  |> String.split ~on:','
  |> List.map ~f:(String.strip ~drop:(fun c -> phys_equal c '"'))
  |> List.sort ~compare:String.compare

let char_score c = int_of_char c - int_of_char 'A' + 1
let name_score name =
  String.fold name ~init:0 ~f:(fun score c -> score + char_score c)

let solve () =
  In_channel.read_all "data/p022_names.txt"
  |> String.strip
  |> String.split ~on:','
  |> List.map ~f:(String.strip ~drop:(fun c -> phys_equal c '"'))
  |> List.sort ~compare:String.compare
  |> List.foldi ~init:0
      ~f:(fun i sum name -> sum + (i + 1) * name_score name)
  |> string_of_int
