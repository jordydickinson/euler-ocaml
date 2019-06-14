(* 
  The prime factors of 13195 are 5, 7, 13 and 29.

  What is the largest prime factor of the number 600851475143 ?
*)

open Core_kernel

let largest_prime_factor n =
  let rec loop n best primes =
    match Sequence.next primes with
    | None -> failwith "What the hell..."
    | Some(p, ps) ->
      if p > n then best
      else if n % p = 0 then
        let rec remove_factors factor n =
          if n % factor <> 0 then n else
          remove_factors factor (n / factor)
        in
        loop (remove_factors p n) p ps
      else loop n best ps
  in
  if n <= 1 then invalid_arg "n must be greater than or equal to 2" else
  loop n 1 Numbers.primes

let solve () =
  largest_prime_factor 600851475143 |> string_of_int
  (* 6857 *)
