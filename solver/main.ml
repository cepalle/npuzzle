open String
open List

type e_move = Up | Down | Right | Left

type np_node = {
  cost: int;
  hys: e_move list;
  grd: int list list;
}

(* TODO *)
let grd_move (grd: int list list) (m:e_move) : int list list = grd

let np_node_move {cost=cost; hys=hys; grd=grd} (m:e_move) : np_node = {
  cost = cost + 1;
  hys = m::hys;
  grd = grd_move grd m
}

let a_start_solver () =

;;


  (* Input *)

let read_npuzzle_input () : int list list =
  let del_comment (s: string) : string = hd (split_on_char '#' s) in

  let parse_line (s : string) : int list = filter_map (fun ss -> 
    if (String.length ss) == 0 then
      None
    else
      Some (int_of_string ss)
    ) (split_on_char ' ' s)
  in
  
  let rec read_npuzzle_input_rec (i : int) (nl : int) : int list list = 
    if i == 0 then
      []
    else
      append [
        let lnum = parse_line (read_line ()) in
          if (List.length lnum) != nl then
            failwith "Bad number of number in a line"
          else
            lnum
      ] (read_npuzzle_input_rec (i - 1) nl)
  in

  let _ = read_line () in
  let nl = read_int_opt () in
    if nl == None then
      failwith "fail to get npuzzle size"
    else
      read_npuzzle_input_rec (Option.get nl) (Option.get nl)

let print_npuzzle (np: int list list) =
  List.map (fun l ->
    let _ = List.map (fun e ->
      print_string ((string_of_int e) ^ " ")
    ) l in
      print_newline ()
  ) np

  (* Main *)

let np = print_npuzzle (read_npuzzle_input ())
