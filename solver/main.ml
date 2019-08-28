open String
open List

type e_move = Up | Down | Right | Left

let e_moves = Up::Down::Right::Left::[]

type np_node = {
  cost: int;
  hys: e_move list;
  grd: int list list;
  score: int;
}

let np_node_get_grd ({grd=grd}: np_node) : int list list = grd

(* TODO *)
let grd_move (grd: int list list) (m: e_move): int list list option = Some grd

let np_node_move {cost=cost; hys=hys; grd=grd} (m: e_move) (scoring_node: np_node -> int): np_node option =
  let nwgrd = grd_move grd m in
  if nwgrd == None then
    None
  else
    let nwcost = cost + 1 in
    Some {
      cost = nwcost;
      hys = m :: hys;
      grd = (Option.get nwgrd);
      score = scoring_node {grd=(Option.get nwgrd); cost=nwcost; hys=[]; score=0};
    }

(* TODO *)
let is_solvable (grd: int list list): bool = true

(* TODO *)
let is_resolve (grd: int list list): bool = true

(* TODO *)
let scoring_grd_manhattan (grd: int list list): int = 0

(* TODO *)
let add_in_prio_queu (to_add: np_node list) (opened: np_node list): np_node list = []

let scoring_node (scoring_grd: int list list -> int) (w: int) (greedy: bool) ({grd=grd; cost=cost}: np_node): int =
  w * (scoring_grd grd) + if greedy then 0 else cost

(* TODO max len + nb elements tot *)
let a_start_solver (scoring_node: np_node -> int) (start: np_node): np_node =
  let closed = Hashtbl.create (1024 * 1024) in
  let opened = ref ([start]: np_node list) in
  while List.exists (fun e -> true) !opened && not (is_resolve (np_node_get_grd (hd !opened))) do
    let (frst: np_node) = hd !opened in
    let (neighbours: np_node list) = List.filter_map (fun m -> np_node_move frst m scoring_node) e_moves in
    let neighbours_not_in_closed = List.filter (fun e -> (Hashtbl.find_opt closed (np_node_get_grd frst)) == None) neighbours in
    opened := add_in_prio_queu neighbours_not_in_closed (tl !opened);
    Hashtbl.add closed (np_node_get_grd frst) true;
    neighbours
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
