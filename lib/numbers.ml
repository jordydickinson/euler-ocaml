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
