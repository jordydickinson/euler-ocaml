open Core_kernel

let digits_of_int k =
  Sequence.unfold ~init:k ~f:(
    fun k -> if k = 0 then None else Some (k%10, k/10)
  ) |> Sequence.to_list_rev

let int_of_digits ks =
  List.fold ks ~init:0 ~f:(
    fun accum k -> accum*10 + k
  )
