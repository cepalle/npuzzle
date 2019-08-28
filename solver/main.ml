open String
open List

type e_move = Up | Down | Right | Left

type np_node = {
  cost: int;
  hys: e_move list;
  grd: int list list;
  score: int;
}

let e_moves = Up::Down::Right::Left::[]

(* TODO *)
let grd_move (grd: int list list) (m: e_move): int list list = grd

let np_node_move {cost=cost; hys=hys; grd=grd} (m: e_move) (scoring_node: np_node -> int): np_node =
  let nwgrd = grd_move grd m in
  let nwcost = cost + 1 in {
    cost = nwcost;
    hys = m :: hys;
    grd = nwgrd;
    score = scoring_node {grd=nwgrd; cost=nwcost; hys=[]; score=0};
  }

(* TODO *)
let is_solvable (grd: int list list): bool = true

(* TODO *)
let is_resolve ({grd=grd} : np_node): bool = true

(* TODO *)
let scoring_grd_manhattan (grd: int list list): int = 0

(* TODO *)
let add_in_prio_queu (to_add: np_node list) (opened: np_node list): np_node list = []

let scoring_node (scoring_grd: int list list -> int) (w: int) (greedy: bool) ({grd=grd; cost=cost}: np_node): int =
  w * (scoring_grd grd) + if greedy then 0 else cost

let a_start_solver (scoring_node: np_node -> int) (start: np_node): np_node =
  let closed = Hashtbl.create (1024 * 1024) in
  let opened = ref ([start]: np_node list) in
  while List.exists (fun e -> true) !opened && not (is_resolve (hd !opened)) do
    let frst = hd !opened in
    let v = List.filter_map (fun m -> np_node_move first m) e_moves  in
    v
  done;
  start

;;

  (* Input *)

let read_npuzzle_input (): int list list =
  let del_comment (s: string): string = hd (split_on_char '#' s) in

  let parse_line (s: string): int list = filter_map (fun ss -> 
    if (String.length ss) == 0 then
      None
    else
      Some (int_of_string ss)
    ) (split_on_char ' ' s)
  in
  
  let rec read_npuzzle_input_rec (i: int) (nl: int): int list list = 
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
