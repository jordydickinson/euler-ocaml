open Core_kernel

let count_from n =
  Sequence.unfold ~init:n ~f:(fun i -> Some(i, i + 1))

(* This is an adaptation of the infinite sieve described in the paper:

       Melissa E. O’Neill. The Genuine Sieve of Eratosthenes. Retrieved June 16,
       2019 from https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf.

   It's based on one of the earlier implementations found in the paper on page
   6, before much thought to optimization was given. It should have time
   complexity of Θ(n log n log log n), provided Core_kernel.Map is the same data
   structure as Haskell's Data.Map, and provided it's faithful to the
   implementation found in the paper. Core_kernel.Sequence is being used to
   imitate Haskell's laziness. *)
let primes = 
  let reinsert tbl prime =
    Map.change tbl prime ~f:(
      fun factors ->
        match factors with
        | None -> Some([prime])
        | Some(factors) -> Some(prime :: factors)
    )
  in
  Sequence.unfold_with (count_from 2) ~init:(Map.empty (module Int))
    ~f:(fun tbl x ->
        match Map.find tbl x with
        | None -> Yield(x, Map.set tbl (x*x) [x])
        | Some(factors) -> Skip(List.fold factors ~init:(Map.remove tbl x) ~f:reinsert)
      )

let eratosthenes n =
  if n < 2 then invalid_arg "n must be >= 2" else
  let sieve = Array.create n true in
  sieve.(0) <- false;
  sieve.(1) <- false;
  Array.set sieve 1 false;
  for i = 2 to int_of_float @@ sqrt (float n) do
    if sieve.(i) then
      let j = ref (Int.pow i 2) in
      while !j < n do
        sieve.(!j) <- false;
        j := !j + i
      done
  done;
  Array.foldi sieve ~init:[]
    ~f:(fun i acc is_prime -> if is_prime then i :: acc else acc)
  |> List.rev
