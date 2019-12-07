(*
  The fraction 49/98 is a curious fraction, as an inexperienced mathematician in
  attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is
  correct, is obtained by cancelling the 9s.

  We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

  There are exactly four non-trivial examples of this type of fraction, less
  than one in value, and containing two digits in the numerator and denominator.

  If the product of these four fractions is given in its lowest common terms,
  find the value of the denominator.
 *)

open Core_kernel

let digits_of_int k =
  Sequence.unfold ~init:k
    ~f:(fun k -> if k = 0 then None else Some (k%10, k/10))

let int_of_digits ks =
  List.fold ks ~init:0
    ~f:(fun accum k -> accum*10 + k)

let galaxy_brain_div p q =
  let ps = digits_of_int p |> Sequence.to_list_rev in
  let qs = digits_of_int q |> Sequence.to_list_rev in
  let common =
    let ps = Int.Set.of_list ps in
    let qs = Int.Set.of_list qs in
    Set.inter ps qs
  in
  if Set.is_empty common then None else
  let remove_common ps common =
    List.fold ps ~init:([], common) ~f:(
      fun (ps, common) p ->
        if Set.mem common p
        then ps, Set.remove common p
        else p :: ps, common
    )
    |> fst (* The result will be reversed, undoing the above to_list_rev. *)
  in
  let ps, qs = remove_common ps common, remove_common qs common in
  let p, q = int_of_digits ps, int_of_digits qs in
  Some Bignum.(p // q)

let solve () =
  let pqs = (* Ensures: p/q < 1; p & q have 2 digits; p, q <> 10n for all n *)
    let qs =
      Sequence.range 10 100
      |> Sequence.filter ~f:(fun n -> n % 10 <> 0)
    in
    Sequence.concat_map qs ~f:(
      fun q ->
        Sequence.range 10 q
        |> Sequence.filter_map ~f:(
            fun p -> if p % 10 = 0 then None else Some (p, q)
        )
    )
  in
  Sequence.filter_map pqs ~f:(
    fun (p, q) ->
      let pq = Bignum.(p // q) in
      match galaxy_brain_div p q with
      | None -> None
      | Some pq' when Bignum.(pq = pq') -> Some pq
      | Some _ -> None
  )
  |> Sequence.fold ~init:Bignum.one ~f:Bignum.( * )
  |> Bignum.den
  |> Bignum.to_string_hum
