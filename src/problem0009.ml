(*
  A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

    a^2 + b^2 = c^2

  For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
 *)

open Core_kernel

let triplets_summing_to n =
  let rec loop acc a =
    if a + (a + 1) + (a + 2) > n then acc else
    let rec loop' acc a b =
      if a + b + (b + 1) > n then acc else
      loop' ((a, b, n - a - b) :: acc) a (b + 1)
    in
    loop (acc @ (loop' [] a (a + 1))) (a + 1)
  in
  loop [] 1

let is_pythagorean (a, b, c) =
  (Int.pow a 2) + (Int.pow b 2) = (Int.pow c 2)

let () =
  triplets_summing_to 1000
  |> List.filter ~f:is_pythagorean
  |> List.iter ~f:(fun (a, b, c) -> printf "%d\n" (a * b * c))
  (* 31875000 *)
