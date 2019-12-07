open Core_kernel

let count_from n =
  Sequence.unfold ~init:n ~f:(fun i -> Some(i, i + 1))

let multiples n =
  Sequence.unfold ~init:n ~f:(fun m -> Some(m, n + m))
