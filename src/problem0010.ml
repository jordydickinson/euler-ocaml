(* 
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.
 *)

open Core_kernel

let () =
  Sequence.take_while Numbers.primes
    (fun p -> p < 2000000)
  |> Sequence.fold ~init:0 ~f:(fun acc p -> printf "%d\n%!" p; acc + p)
  |> printf "%d\n" (* 142913828922 *)
