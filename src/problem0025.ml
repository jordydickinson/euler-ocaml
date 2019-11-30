(*
  The Fibonacci sequence is defined by the recurrence relation:

    F(n) = F(n−1) + F(n−2), where F(1) = 1 and F(2) = 1.
  
  Hence the first 12 terms will be:

      F(1) = 1
      F(2) = 1
      F(3) = 2
      F(4) = 3
      F(5) = 5
      F(6) = 8
      F(7) = 13
      F(8) = 21
      F(9) = 34
      F(10) = 55
      F(11) = 89
      F(12) = 144

  The 12th term, F(12), is the first term to contain three digits.

  What is the index of the first term in the Fibonacci sequence to contain 1000
  digits?
 *)

open Core_kernel

let phi = Float.((1. + sqrt 5.) / 2.)

let digits_fib n = (+) 1 @@ int_of_float @@
  Float.(of_int n * log10 phi - log10 5. / 2.)

let solve () =
  let upper =
    let rec find_upper_bound bound delta =
      if digits_fib bound >= 1000
      then bound
      else find_upper_bound (bound + delta) (delta * 2)
    in
    find_upper_bound 0 1
  in
  let n =
    let rec search lower upper =
      if upper - lower = 1 then upper else
      let pivot = (lower + upper) / 2 in
      if digits_fib pivot >= 1000
      then search lower pivot
      else search pivot upper
    in
    search 0 upper
  in
  string_of_int n
