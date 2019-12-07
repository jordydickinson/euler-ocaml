(*
  We shall say that an n-digit number is pandigital if it makes use of all the
  digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
  through 5 pandigital.

  The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
  multiplicand, multiplier, and product is 1 through 9 pandigital.

  Find the sum of all products whose multiplicand/multiplier/product identity
  can be written as a 1 through 9 pandigital.

  HINT: Some products can be obtained in more than one way so be sure to only
  include it once in your sum.
 *)

open Core_kernel

(*
  Taking the logarithm of both sides of a * b = c, we get
  log(a) + log(b) = log(c). Since the number of decimal digits in n is given
  by floor(log(n)) + 1, this tells us that the combined digits of a and b
  must be one more than the digits of c.

  We also know that this is only possible when a and b have combined digits of
  5, and c has 4 digits, because any values of a and b with fewer than 5
  combined digits cannot have more than 4 digits. Likewise, any values of a and
  b with more than 5 digits cannot have fewer than 4 digits.

  So we only need to check the products of a and b with combined digits of 5.
  Also note that with 5 digits we need only check a with 1 and 2 digits, and b
  with 3, 4, and 5 digits. This is due to commutativity of multiplication.
 *)

let int_of_digits digits =
  List.fold digits ~init:0
    ~f:(fun k digit -> k*10 + digit)

let solve () =
  let perm_filter_mapper n m digits =
    let a, bc = List.split_n digits n in
    let b, c = List.split_n bc m in
    let a, b, c = int_of_digits a, int_of_digits b, int_of_digits c in
    if a * b = c
    then Some c
    else None
  in
  let perms = Sequence.memoize @@ List_ext.permutations [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let pandigitals1 = Sequence.filter_map perms ~f:(perm_filter_mapper 1 4) in
  let pandigitals2 = Sequence.filter_map perms ~f:(perm_filter_mapper 2 3) in
  Sequence.append pandigitals1 pandigitals2
  |> Sequence.to_list_rev
  |> Set.of_list (module Int)
  |> Set.sum (module Int) ~f:ident
  |> string_of_int
