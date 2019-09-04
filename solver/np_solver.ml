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

let get_grd ({grd=grd}: np_node): int list list = grd
let get_score ({score=score}: np_node): int = score

let indexed (l: 'a list): ('a * int) list =
  let rec indexed_rec (l: 'a list) (i: int): ('a * int) list =
    match l with
      [] -> []
      | h :: t -> (h, i) :: (indexed_rec t (i + 1))
  in
  indexed_rec l 0

let rec get_at (l: 'a list) (i: int): 'a =
  match i with
    0 -> hd l
    | _ -> get_at (tl l) (i - 1)

let get_at_coord (grd: 'a list list) ({x=x; y=y}: coord): 'a = get_at (get_at grd y) x

let find_n (grd: int list list) (n: int): coord =
  let (l, i) =
    find (fun (l, i) -> 
      List.exists (fun e -> e == n) l
    ) (indexed grd)
  in
  let (e, j) = find (fun (e, j) -> e == n) (indexed l) in
  {y=i; x=j}

let move_coord ({x=x; y=y}: coord) (m: e_move): coord =
  match m with
      Up -> {x = x; y = y + 1}
      | Down -> {x = x; y = y - 1}
      | Right -> {x = x + 1; y = y}
      | Left -> {x = x - 1; y = y}
      
let safe_move_coord ({x=x; y=y}: coord) (m: e_move) (n: int): coord option =
  let is_valide ({x=x; y=y}: coord) =
    x > 0 && y > 0 && x < n && y < n
  in
  let nwc = move_coord {x=x; y=y} m in
  if is_valide nwc then Some nwc else None

let replace_at_coord (grd: 'a list list) ({x=x; y=y}: coord) (erpl: 'a): 'a list list =
  List.map (fun (l, i) ->
    List.map (fun (e, j) ->
      if x = j && y = i then erpl
      else e
    ) (indexed l)
  ) (indexed grd)

let grd_move (grd: int list list) (m: e_move): int list list option =
  let l = length grd in
  let c0 = find_n grd 0 in
  Option.map (fun cd ->
    let num_move = get_at_coord grd cd in
    replace_at_coord (replace_at_coord grd cd 0) c0 num_move
  ) (safe_move_coord c0 m l) 

let np_node_move ({cost=cost; hys=hys; grd=grd}: np_node) (m: e_move) (scoring_node: np_node -> int): np_node option =
  Option.map (fun (nwgrd: int list list) -> {
      cost = cost + 1;
      hys = m :: hys;
      grd = nwgrd;
      score = scoring_node {grd=nwgrd; cost=cost + 1; hys=[]; score=0};
    }
  ) (grd_move grd m)

let coord0 (n: int): coord = {x = (n - 1) / 2; y = (n) / 2}

let rec order_to_pos (num: int) (n: int): coord =
  if num == 0 then
    coord0 n
  else if num > n * 4 - 4 then
    let {x=xb; y=yb} = order_to_pos (num - (n * 4 - 4)) (n - 2) in
    {x = xb + 1; y = yb + 1}
  else
    if num == 0 then {x=0; y=0}
    else if num <= n then {x = num - 1; y=0}
    else if num <= n * 2 - 1 then {x = n - 1; y = num - n}
    else if num <= n * 3 - 2 then {x = (n * 3 - 2) - num; y = n - 1}
    else {x=0; y = (n * 4 - 3) - num}

let rec pos_to_order ({x=x; y=y}: coord) (n: int): int =
  if coord0 n == {x=x; y=y} then
    0
  else if (n <= 1) then
    0
  else if (n == 2) then
    match (x, y) with
      (0, 0) -> 1
      | (0, 1) -> 0
      | (1, 0) -> 2
      | (1, 1) -> 3
      | _ -> failwith "pos_to_num invalid coord"
  else if y == 0 then x + 1
  else if x == n - 1 then n + y
  else if y == n - 1 then 3 * n - 2 - x
  else if x == 0 then 4 * n - 3 - y
  else
    let num_in = pos_to_order {x = x - 1; y = y - 1} (n - 2) in
      if num_in == 0 then
        0
      else
        n * 4 - 4 + num_in

let gen_coord (n: int): coord list =
  let rec gen_aux (x: int) (i: int): coord list =
    match i with
      0 -> []
      | _ -> {x=x; y=i - 1} :: (gen_aux x (i - 1))
  in
  let rec gen_rec (n: int) (i: int): coord list =
    match i with
      0 -> []
      | _ -> append (gen_aux (i - 1) n) (gen_rec n (i - 1))
  in
  List.filter (fun {x=x; y=y} ->
    y < x
  ) (gen_rec n n)

let count_permutation (grd: int list list): int =
  let n = length grd in
  let order_of a = pos_to_order (find_n grd a) n in
  fold_left (fun nb {x=a; y=b} ->
    let bl = (order_of a) < (order_of b) in
    (*
    let () = print_string ((string_of_int a) ^ " " ^ (string_of_int b) ^ " " ^ string_of_bool (bl)) in
    let () = print_newline () in
    let () = print_string ((string_of_int (order_of a)) ^ " " ^ (string_of_int (order_of b))) in
    let () = print_newline () in
    let () = print_newline () in
    *)
    if bl then nb + 1
    else nb
  ) 0 (gen_coord (n * n))

let is_solvable (grd: int list list): bool =
  let nbp = count_permutation grd in
  let {x=x0; y=y0} = find_n grd 0 in
  let {x=xv0; y=yv0} = coord0 (length grd) in
  let dst0 = (abs (x0 - xv0)) + (abs (y0 + yv0)) in 
  dst0 mod 2 == (nbp mod 2)

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
                (order_to_pos e c != {x=j; y=i}, j + 1)
            ) (true, 0) line
          in bl
        , i + 1)
    ) (true, 0) grd
  in bg

let scoring_grd_manhattan (grd: int list list): int =
  let n = length grd in
  let dist_man {x=x1; y=y1} {x=x2; y=y2} = (abs (x1 - x2)) + (abs (y1 - y2)) in
  fold_left ( fun s (l, i) ->
    s + fold_left (fun s (e, j) ->
    if e == 0 then s
    else s + (dist_man {x=j; y=i} (order_to_pos e n))
    ) 0 (indexed l)
  ) 0 (indexed grd)

let rec add_in_prio_queu (opened: np_node list) (to_add: np_node) : np_node list =
  match opened with
    [] -> [to_add]
    | h::t -> if get_score to_add <= get_score h then
                to_add::opened
              else
                h::(add_in_prio_queu t to_add)

let scoring_node (scoring_grd: int list list -> int) (w: int) (greedy: bool) ({grd=grd; cost=cost}: np_node): int =
  w * (scoring_grd grd) + if greedy then 0 else cost

(* TODO max len + nb elements tot  ///  int list list work in map ? *)
let a_start_solver (scoring_node: np_node -> int) (grd: int list list): np_node =
  let closed = Hashtbl.create (1024 * 1024) in
  let start = {
    cost=0;
    hys=[];
    grd=grd;
    score=scoring_node {cost=0; hys=[]; grd=grd; score=0}
  } in
  let opened = ref ([start]: np_node list) in
  while !opened == [] && not (is_resolve (get_grd (hd !opened))) do
    let (frst: np_node) = hd !opened in
    let (neighbours: np_node list) = List.filter_map (fun m -> np_node_move frst m scoring_node) e_moves in
    let neighbours_not_in_closed = List.filter (fun e -> (Hashtbl.find_opt closed (get_grd frst)) == None) neighbours in
    opened := fold_left add_in_prio_queu (tl !opened) neighbours_not_in_closed;
    Hashtbl.add closed (get_grd frst) true
  done;
  start
