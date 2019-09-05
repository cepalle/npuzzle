open List

type e_move = Up | Down | Right | Left

let e_moves = Up::Down::Right::Left::[]

let to_string (m: e_move) = match m with
  Up -> "Up"
  | Down -> "Down"
  | Right -> "Right"
  | Left -> "Left"

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

type a_star_res = {
  node: np_node;
  max_nb_opened: int;
  nb_closed: int;
}

let get_grd ({grd=grd; _}: np_node): int list list = grd
let get_score ({score=score; _}: np_node): int = score

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
    find (fun (l, _) -> 
      List.exists (fun e -> e == n) l
    ) (indexed grd)
  in
  let (_, j) = find (fun (e, _) -> e == n) (indexed l) in
  {y=i; x=j}

let move_coord ({x=x; y=y}: coord) (m: e_move): coord =
  match m with
      Up -> {x = x; y = y - 1}
      | Down -> {x = x; y = y + 1}
      | Right -> {x = x + 1; y = y}
      | Left -> {x = x - 1; y = y}
      
let safe_move_coord ({x=x; y=y}: coord) (m: e_move) (n: int): coord option =
  let is_valide ({x=x; y=y}: coord) =
    x >= 0 && y >= 0 && x < n && y < n
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

let np_node_move ({cost=cost; hys=hys; grd=grd; _}: np_node) (m: e_move) (scoring_node: np_node -> int): np_node option =
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
  let n = length grd in
  let rec check_line (y: int) (l: int list) (i: int): bool =
    match l with
      [] -> true
      | h::t -> let {x=xv; y=yv} = order_to_pos h n in
                (xv == i && yv == y) && (check_line y t (i + 1))
  in
  let rec check_grd (grd: int list list) (j: int): bool =
    match grd with
      [] -> true
      | h::t -> (check_line j h 0) && (check_grd t (j + 1))  
  in
  check_grd grd 0

let dist_hamming {x=x1; y=y1} {x=x2; y=y2} = 
  if x1 == x2 && y1 == y2 then 0 else 1

let dist_chebyshev {x=x1; y=y1} {x=x2; y=y2} = max (abs (x1 - x2)) (abs (y1 - y2))

let dist_euclidean {x=x1; y=y1} {x=x2; y=y2} = int_of_float (sqrt (
    ((float_of_int (abs (x1 - x2))) ** 2.0)
   +.
   ((float_of_int (abs (y1 - y2))) ** 2.0)
))

let dist_manhattan {x=x1; y=y1} {x=x2; y=y2} = (abs (x1 - x2)) + (abs (y1 - y2))

let scoring_grd_dst (dist: coord -> coord -> int) (grd: int list list): int =
  let n = length grd in
  fold_left ( fun s (l, i) ->
    s + fold_left (fun s (e, j) ->
    if e == 0 then s
    else s + (dist {x=j; y=i} (order_to_pos e n))
    ) 0 (indexed l)
  ) 0 (indexed grd)      

let rec add_in_prio_queu (opened: np_node list) (to_add: np_node) : np_node list =
  match opened with
    [] -> [to_add]
    | h::t -> if get_score to_add <= get_score h then
                to_add::opened
              else
                h::(add_in_prio_queu t to_add)

let scoring_node (scoring_grd: int list list -> int) (w: int) (greedy: bool) ({grd=grd; cost=cost; _}: np_node): int =
  w * (scoring_grd grd) + if greedy then 0 else cost

let np_print_a_star_res ({node={cost=cost; hys=hys; _}; max_nb_opened=mo; nb_closed=nc}: a_star_res): unit =
let () = print_string ("max open = " ^ (string_of_int mo)) in
let () = print_newline () in
let () = print_string ("closed = " ^ (string_of_int nc)) in
let () = print_newline () in
let () = print_string ("nb transition = " ^ (string_of_int cost) ^ " " ^ (string_of_int (length hys))) in
let () = print_newline () in
let () = print_newline () in
  iter (fun m ->
    let () = print_string (to_string m) in
    print_newline ()
  ) (rev hys)

let rec make_grd_key (grd: int list list): string =
  let rec make_line_key (l: int list): string =
    match l with
      [] -> ""
      | h::t -> (string_of_int h) ^ " " ^ (make_line_key t)
  in
  match grd with
    [] -> ""
    | h::t -> (make_line_key h) ^ (make_grd_key t)

let a_star_solver (scoring_node: np_node -> int) (grd: int list list): a_star_res option =
  if not (is_solvable grd) then None
  else Some ( 
    let closed = Hashtbl.create (1024 * 1024) in
    let (start: np_node) = {
      cost=0;
      hys=[];
      grd=grd;
      score=scoring_node {cost=0; hys=[]; grd=grd; score=0}
    } in
    let opened = ref ([start]: np_node list) in
    let max_opened = ref 1 in
    while !opened != [] && not (is_resolve (get_grd (hd !opened))) do
      let (frst: np_node) = hd !opened in
      let (neighbours: np_node list) = List.filter_map (fun m -> np_node_move frst m scoring_node) e_moves in
      let (neighbours_not_in_closed: np_node list) = List.filter (fun n -> (Hashtbl.find_opt closed (make_grd_key (get_grd n))) == None) neighbours in
      opened := fold_left add_in_prio_queu (tl !opened) neighbours_not_in_closed;
      max_opened := max !max_opened (length !opened);
      Hashtbl.add closed (make_grd_key (get_grd frst)) true
    done;
    if !opened == [] then
      failwith "faild to resolve"
    else {
      max_nb_opened = !max_opened;
      node = hd !opened;
      nb_closed = Hashtbl.length closed
    }
  )
