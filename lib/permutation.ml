open Core_kernel

type t = { id : int array }

let id prm = prm.id

let inv prm =
  let prm = prm.id in
  let len = Array.length prm in
  let inv = Array.create len 0 in
  for i = 0 to len - 1 do
    inv.(prm.(i)) <- i
  done;
  { id = inv }

let of_factoradic len fac =
  let digits = Factoradic.digits fac in
  let digitslen = Array.length digits in
  assert (len >= 0);
  assert (len >= digitslen);
  let prm = Array.create len 0 in
  let prm_free = Array.create len true in
  for i = len - 1 downto 0 do
    let offset = ref (Factoradic.get i fac) in
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
  { id = prm } |> inv

let of_rank len rank =
  assert (len >= 0);
  assert (rank >= 0);
  let fac = Factoradic.of_int rank in
  of_factoradic len fac
