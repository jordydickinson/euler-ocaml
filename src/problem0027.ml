(*
  Euler discovered the remarkable quadratic formula:

    n^2 + n + 41
  
  It turns out that the formula will produce 40 primes for the consecutive
  integer values 0 <= n <= 39. However, when n = 40,

    40^2 + 40 + 41 = 40(40 + 1) + 41

  is divisible by 41, and certainly when n = 41,

    41^2 + 41 + 41

  is clearly divisible by 41.

  The incredible formula n^2 - 79n + 1601 was discovered, which produces 80
  primes for the consecutive values 0 <= n <= 79. The product of the
  coefficients, -79 and 1601, is -126479.

  Considering quadratics of the form:

    n^2 + an + b, where |a| < 1000 and |b| <= 1000

    where |n| is the modulus/absolute value of n
    e.g. |11| = 11 and |-4| = 4

  Find the product of the coefficients, a and b, for the quadratic expression
  that produces the maximum number of primes for consecutive values of n,
  starting with n = 0.
 *)

open Core_kernel

let primes =
  Numbers.eratosthenes 1_000_000
  |> Set.of_list (module Int)

let quadf a b n =
  n*(n + a) + b

let consec_primes f =
  let max_prime = Set.max_elt_exn primes in
  Sequences.count_from 0
  |> Sequence.map ~f:f
  |> Sequence.take_while
      ~f:(fun n -> assert (n < max_prime); Set.mem primes n)
  |> Sequence.length

let solve () = string_of_int @@
  let rec solve' best ab a b =
    if a = 1000 && b = 1001 then ab else
    let consec_ps = consec_primes (quadf a b) in
    let best, ab =
      if consec_ps > best
      then consec_ps, a*b
      else best, ab
    in
    let a, b =
      if b = 1001
      then a + 1, -1000
      else a, b + 1
    in
    solve' best ab a b
  in
  solve' 0 0 (-999) (-1000)
