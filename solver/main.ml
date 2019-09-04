open String
open List

  (* Main *)

let (grd: int list list) = Np_input.read_npuzzle_input ()
let n = length grd
let () = print_int n
let () = print_newline ()
let () = Np_input.print_npuzzle grd
let () = print_newline ()
let node = Np_solver.a_star_solver (Np_solver.scoring_node (Np_solver.scoring_grd_dst Np_solver.dist_manhattan) 1 false) grd

(*
let () = Np_input.print_npuzzle grd
let () = print_newline ()
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

let () = print_string (string_of_int (Np_solver.count_permutation grd))
let () = print_newline ()
let () = print_string (string_of_bool (Np_solver.is_solvable grd))
let () = print_newline ()
let () = print_string (string_of_bool (Np_solver.is_resolve grd))
let () = print_newline ()
let () = print_string (string_of_int (Np_solver.scoring_grd_manhattan grd))
let () = print_newline ()
let () = print_newline ()

let () = print_newline ()
let () = print_newline ()
*)

let _ = Option.map Np_solver.np_print_a_star_res node

(*
let () = iter (fun {x=x; y=y} ->
  print_string ((string_of_int x) ^ " " ^ (string_of_int y));
  print_newline ()
) (gen_coord (n * n))
*)
