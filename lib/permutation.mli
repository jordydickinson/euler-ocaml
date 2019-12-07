type t = { id : int array }

val id : t -> int array
val inv : t -> t
val of_factoradic : int -> Factoradic.t -> t
val of_rank : int -> int -> t
