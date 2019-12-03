open Core_kernel
open Stdio

let usage = Sys.executable_name ^ " PROBLEM_NUMBER [...]"

let problems = Hashtbl.of_alist_exn (module Int)
    [(1, Problem0001.solve)
    ;(2, Problem0002.solve)
    ;(3, Problem0003.solve)
    ;(4, Problem0004.solve)
    ;(5, Problem0005.solve)
    ;(6, Problem0006.solve)
    ;(7, Problem0007.solve)
    ;(8, Problem0008.solve)
    ;(9, Problem0009.solve)
    ;(10, Problem0010.solve)
    ;(11, Problem0011.solve)
    ;(12, Problem0012.solve)
    ;(13, Problem0013.solve)
    ;(14, Problem0014.solve)
    ;(15, Problem0015.solve)
    ;(16, Problem0016.solve)
    ;(17, Problem0017.solve)
    ;(18, Problem0018.solve)
    ;(19, Problem0019.solve)
    ;(20, Problem0020.solve)
    ;(21, Problem0021.solve)
    ;(22, Problem0022.solve)
    ;(23, Problem0023.solve)
    ;(24, Problem0024.solve)
    ;(25, Problem0025.solve)
    ;(26, Problem0026.solve)
    ;(27, Problem0027.solve)
    ;(28, Problem0028.solve)
    ]

let () =
  let args = List.drop (Array.to_list Sys.argv) 1 in
  let problem_nums =
    if List.is_empty args then
      (
        eprintf "Note: No problems given; assuming all problems.\n%!";
        Hashtbl.keys problems |> List.sort ~compare:Int.compare
      )
    else
      List.map args
        ~f:(
          fun problem_str ->
            match int_of_string_opt problem_str with
            | None -> failwith "Problems must be specified as integers."
            | Some(n) ->
              if n < 1
              then (failwith "Problem numbers must be positive integers.")
              else n
        )
  in
  List.iter problem_nums ~f:(
    fun problem_num ->
      match Hashtbl.find problems problem_num with
      | None ->
        eprintf "Error: Problem %d has no solution.\n%!" problem_num
      | Some(soln_fn) ->
        let t = Sys.time () in
        let soln = soln_fn () in
        let t = Sys.time () -. t in
        printf "Solution to problem %d: %s\n%!" problem_num @@ soln;
        printf "  Time: %f seconds\n%!" t
  )
