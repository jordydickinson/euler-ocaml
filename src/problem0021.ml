(*
  Let d(n) be defined as the sum of proper divisors of n (numbers less than n
  which divide evenly into n). If d(a) = b and d(b) = a, where a ≠ b, then a and
  b are an amicable pair and each of a and b are called amicable numbers.

  For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
  and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71
  and 142; so d(284) = 220.

  Evaluate the sum of all the amicable numbers under 10000.
 *)

open Core_kernel

let dtable = Array.create 9998 1
let () =
  for i = 2 to 9999 do
    let j = ref (i + i) in
    while !j < 10_000 do
      dtable.(!j - 2) <- dtable.(!j - 2) + i;
      j := !j + i
    done
  done

let d n =
  assert (n >= 2 && n < 10_000);
  dtable.(n - 2)

let solve () =
  let rec solve' accum i =
    if i = 10_000 then accum else
    let di = d i in
    if 2 <= di && di < 10_000 && not (phys_equal di i) && d di = i
    then solve' (accum + i) (i + 1)
    else solve' accum (i + 1)
  in
  string_of_int (solve' 0 2)
