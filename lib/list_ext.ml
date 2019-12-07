open Core_kernel

let permutations xs =
  let rec permutations' xs xslen permslen =
    let seq = Sequence.of_lazy @@ lazy begin
      match xs with
      | [] -> Sequence.singleton []
      | x :: xs ->
        let perms1 =
          Sequence.map ~f:(List.cons x)
            (permutations' xs (xslen - 1) (permslen/xslen))
        in
        let perms2 = permutations' (xs @ [x]) xslen permslen in
        Sequence.append perms1 perms2
    end in
    Sequence.take seq permslen
  in
  let xslen = List.length xs in
  permutations' xs xslen (Numbers.factorial xslen)
