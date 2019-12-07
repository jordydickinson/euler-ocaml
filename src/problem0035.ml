(*
  The number, 197, is called a circular prime because all rotations of the
  digits: 197, 971, and 719, are themselves prime.

  There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
  73, 79, and 97.

  How many circular primes are there below one million?
 *)

open Core_kernel

let primes = Numbers.eratosthenes 1_000_000 |> Int.Set.of_list

let rotations xs =
  let rec rotations' accum xs i =
    if i = 0 then accum else
    match xs with
    | [] -> []
    | x :: xs ->
      let xs = xs @ [x] in
      rotations' (xs :: accum) xs (i - 1)
  in
  rotations' [] xs (List.length xs)

let solve () =
  Set.filter primes ~f:(
    fun p ->
      let ps = Util.digits_of_int p in
      if List.mem ~equal:(=) ps 0 then false else
      let rotps = rotations ps in
      List.for_all rotps ~f:(
        fun rotp -> Set.mem primes @@ Util.int_of_digits rotp
      )
  )
  |> Set.length
  |> string_of_int
