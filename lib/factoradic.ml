open Core_kernel

type t = { digits : int array }

let get i fac =
  if i >= Array.length fac.digits
  then 0
  else fac.digits.(i)

let digits fac = fac.digits

let to_int fac =
  Array.foldi fac.digits ~init:(0, 1)
    ~f:(fun i (sum, placeval) digit ->
          let sum = sum + digit*placeval in
          let placeval = placeval * (i + 1) in
          (sum, placeval))
  |> fst

let of_int k =
  if k < 0 then failwith "Negative arguments not implemented" else
  let rec digits_of_int digits k placeval =
    if k = 0 then Array.of_list_rev digits else
    let digit = k mod placeval in
    let k = k / placeval in
    digits_of_int (digit :: digits) k (placeval + 1)
  in
  { digits = digits_of_int [] k 1 }
