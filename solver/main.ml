open String
open List

  (* Main *)

let (np: int list list) = Np_input.read_npuzzle_input ()
let n = length np
let () = Np_input.print_npuzzle np
let () = print_newline ()
(*
let () = print_npuzzle (List.map (fun (line, i) ->
  List.map (fun (e, j) ->
    pos_to_order {x=j; y=i} n
  ) (indexed line)
) (indexed np))
let () = print_newline ()
let () = print_npuzzle (List.map (fun (line, i) ->
  List.map (fun (e, j) ->
    pos_to_order (order_to_pos e n) n
  ) (indexed line)
) (indexed np))
let () = print_newline ()
let () = print_npuzzle (List.map (fun (line, i) ->
  List.map (fun (e, j) ->
    get_at_coord np (find_n np e)
  ) (indexed line)
) (indexed np))
let () = print_newline ()
*)
let () = print_string (string_of_int (Np_solver.count_permutation np))
let () = print_newline ()
let () = print_string (string_of_bool (Np_solver.is_solvable np))
let () = print_newline ()
let () = print_newline ()
(*
let () = iter (fun {x=x; y=y} ->
  print_string ((string_of_int x) ^ " " ^ (string_of_int y));
  print_newline ()
) (gen_coord (n * n))
*)
