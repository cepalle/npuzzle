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

type coord = {
  x: int;
  y: int;
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

let rec num_to_pos (num: int) (n: int): coord =
  if num == n * n then
    {x = (n - 1) / 2; y = (n) / 2}
  else if num > n * 4 - 4 then
    let {x=xb; y=yb} = num_to_pos (num - (n * 4 + 4)) (n - 1) in
    {x = xb + 1; y = yb + 1}
  else
    if num == 0 then {x=0; y=0}
    else if num <= n then {x = num - 1; y=0}
    else if num <= n * 2 - 1 then {x = n - 1; y = num - n}
    else if num <= n * 3 - 2 then {x = (n * 3 - 2) - num; y = n - 1}
    else {x=0; y = (n * 4 - 3) - num}

let rec pos_to_num ({x=x; y=y}: coord) (n: int): int =
  if x == 0 || y == 0 || x == n - 1 || y == n - 1 then
    if (n == 0) then
      0
    else if (n == 1) then
      match (x, y) with
        (0, 0) -> 1
        | (0, 1) -> 2
        | (1, 0) -> 0
        | (1, 1) -> 3
        | _ -> failwith "pos_to_num unreachable code"
    else
      if x == 0 then y
      else if x == n - 1 then n + y
      else if y == n - 1 then 3 * n - 2 - x
      else 4 * n - 3 - y
  else
    let num_in = pos_to_num {x = x - 1; y = y - 1} (n - 1) in
      if num_in == 0 then
        0
      else
        n * 4 - 4 + num_in

(* TODO *)
let is_solvable (grd: int list list): bool = true

(* TODO *)
let is_resolve (grd: int list list): bool =
  let c = length grd in
  let (bg, _) : bool * int =
    fold_left (fun ((bfg, i): bool * int) (line : int list) ->
      if (bfg == false) then
        (false, 0)
      else
        (
          let (bl, _) = fold_left (fun ((bfl, j): bool * int) (e: int) -> 
              if (bfl == false) then
                (false, 0)
              else
                (num_to_pos e c != {x=j; y=i}, j + 1)
            ) (true, 0) line
          in bl
        , i + 1)
    ) (true, 0) grd
  in bg

;;
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
    Hashtbl.add closed (np_node_get_grd frst) true
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
