open Core_kernel

(* NOTE: This implementation could be dramatically improved if written as an
    incremental sieve, which has a nice functional expression as well as
    addresses the memory problems with the Sieve of Eratosthenes. This
    implementation, however, is neither. The Sieve of Erathosthenes was too
    memory-inefficient for many problems here. *)
let primes = Sequence.memoize @@
  Sequence.unfold_step ~init:([], 2) ~f:(
    fun (ps, n) ->
      if List.for_all ps (fun p -> n % p <> 0)
      then Yield(n, (ps @ [n], n + 1))
      else Skip((ps, n + 1))
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
