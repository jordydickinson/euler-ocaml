open Core_kernel

let count_from n =
  Sequence.unfold ~init:n ~f:(fun i -> Some(i, i + 1))

let multiples n =
  Sequence.unfold ~init:n ~f:(fun m -> Some(m, n + m))

(* This is an adaptation of the infinite sieve described in the paper:

       Melissa E. O’Neill. The Genuine Sieve of Eratosthenes. Retrieved June 16,
       2019 from https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf.

   It's based on one of the earlier implementations found in the paper on page
   6, before much thought to optimization was given. It should have time
   complexity of Θ(n log n log log n), provided Core_kernel.Map is the same data
   structure as Haskell's Data.Map, and provided it's faithful to the
   implementation found in the paper. Core_kernel.Sequence is being used to
   imitate Haskell's laziness. *)
let primes = 
  Sequence.unfold_with (count_from 2) ~init:(Map.empty (module Int))
    ~f:(
      fun tbl x ->
        let reinsert tbl prime =
          Map.change tbl (x + prime) ~f:(
            fun factors ->
              match factors with
              | None -> Some([prime])
              | Some(factors) -> Some(factors @ [prime])
          )
        in
        match Map.find tbl x with
        | None -> Yield(x, Map.set tbl (x*x) [x])
        | Some(factors) -> Skip(List.fold factors ~init:(Map.remove tbl x) ~f:reinsert)
    )

let eratosthenes n =
  if n < 2 then invalid_arg "n must be >= 2" else
  let sieve = Array.create n true in
  sieve.(0) <- false;
  sieve.(1) <- false;
  Array.set sieve 1 false;
  for i = 2 to int_of_float @@ sqrt (float n) do
    if sieve.(i) then
      let j = ref (Int.pow i 2) in
      while !j < n do
        sieve.(!j) <- false;
        j := !j + i
      done
  done;
  Array.foldi sieve ~init:[]
    ~f:(fun i acc is_prime -> if is_prime then i :: acc else acc)
  |> List.rev

let binomial n k =
  (* We must be careful to avoid integer overflow of intermediate results, which
     is why this implementation looks so different from the n!/((n-k)!*k!) we
     all know and love. *)
  let rec binomial' accum n' k' =
    if k' - 1 = k then accum else
    binomial' (accum * n' / k') (n' - 1) (k' + 1)
  in
  binomial' 1 n 1

(** Returns a function which gives the sum of the proper divisors for any
    integer i such that 2 <= i && i < n. *)
let pdsum_fn n =
  let table = Array.create (n - 2) 1 in
  for i = 2 to n - 1 do
    let j = ref (i + i) in
    while !j <= n - 1 do
      table.(!j - 2) <- table.(!j - 2) + i;
      j := !j + i
    done
  done;
  let f i =
    assert (2 <= i);
    assert (i < n);
    table.(i - 2)
  in
  f

let factorial n =
  let rec factorial' n m =
    if n = 0 then m else
    factorial' (n - 1) (n * m)
  in
  factorial' n 1

type factoradic = { digits : int array }

let factoradic_get i fac =
  if i >= Array.length fac.digits
  then 0
  else fac.digits.(i)

let int_of_factoradic fac =
  Array.foldi fac.digits ~init:(0, 1)
    ~f:(fun i (sum, placeval) digit ->
          let sum = sum + digit*placeval in
          let placeval = placeval * (i + 1) in
          (sum, placeval))
  |> fst

let factoradic_of_int k =
  assert (k >= 0);
  let rec digits_of_int digits k placeval =
    if k = 0 then Array.of_list_rev digits else
    let digit = k mod placeval in
    let k = k / placeval in
    digits_of_int (digit :: digits) k (placeval + 1)
  in
  { digits = digits_of_int [] k 1 }

type perm = { _perm : int array }

let perm_inv prm =
  let prm = prm._perm in
  let len = Array.length prm in
  let inv = Array.create len 0 in
  for i = 0 to len - 1 do
    inv.(prm.(i)) <- i
  done;
  { _perm = inv }

let perm_of_factoradic len fac =
  let digits = fac.digits in
  let digitslen = Array.length digits in
  assert (len >= 0);
  assert (len >= digitslen);
  let prm = Array.create len 0 in
  let prm_free = Array.create len true in
  for i = len - 1 downto 0 do
    let offset = ref (factoradic_get i fac) in
    let prmi = ref 0 in
    let adjust_prmi () =
      while not prm_free.(!prmi) do
        incr prmi
      done
    in
    adjust_prmi ();
    while !offset > 0 do
      incr prmi;
      adjust_prmi ();
      decr offset
    done;
    prm.(!prmi) <- len - 1 - i;
    prm_free.(!prmi) <- false;
  done;
  { _perm = prm } |> perm_inv

let perm_of_rank len rank =
  assert (len >= 0);
  assert (rank >= 0);
  let fac = factoradic_of_int rank in
  perm_of_factoradic len fac
