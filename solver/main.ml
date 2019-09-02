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

let find_n (grd: int list list) (n: int): coord =
  let (_, c) =
    fold_left (fun ((b, {x=cx;y=cy}): bool * coord) (line : int list) ->
      if b then
        (true, {x=cx; y=cy})
      else
        let (bl, {x=cxn; y=cyn}) = fold_left (fun ((b, {x=cxf; y=cyf}): bool * coord) (e : int) ->
          if e == n then 
            (true, {x=cxf; y=cyf})
          else
            (false, {x = cxf + 1; y = cyf})
        ) (false, {x = 0; y = cy}) line
        in
        if bl then
          (true, {x=cxn; y=cyn})
        else
          (false, {x = 0; y = cyn + 1})
      ) (false, {x=0; y=0}) grd
  in c

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

let indexed (l: 'a list): ('a * int) list =
  let rec indexed_rec (l: 'a list) (i: int): ('a * int) list =
    match l with
      [] -> []
      | h :: t -> (h, i) :: (indexed_rec t (i + 1))
  in
  indexed_rec l 0

let replace_at_coord (grd: 'a list list) ({x=x; y=y}: coord) (erpl: 'a): 'a list list =
  List.map (fun (l, i) ->
    List.map (fun (e, j) ->
      if x = j && y = i then erpl
      else e
    ) (indexed l)
  ) (indexed grd)

let rec get_at (l: 'a list) (i: int): 'a =
  match i with
    0 -> hd l
    | _ -> get_at (tl l) (i - 1)

let get_at_coord (grd: 'a list list) ({x=x; y=y}: coord): 'a = get_at (get_at grd y) x

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

let rec num_to_pos (num: int) (n: int): coord =
  if num == n * n then
    coord0 n
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
      | _ -> failwith "pos_to_num unreachable code"
  
  else if y == 0 then x + 1
  else if x == n - 1 then n + y
  else if y == n - 1 then 3 * n - 2 - x 
  else if x == 0 then 4 * n - 3 - y 
  
  else
    let num_in = pos_to_num {x = x - 1; y = y - 1} (n - 2) in
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
    y < x && y != 0 && x != 0
  ) (gen_rec n n)

let count_permutation (grd: int list list): int =
  let n = length grd in
  fold_left (fun nb {x=a; y=b} ->
    if (pos_to_num (find_n grd a) n) < (pos_to_num (find_n grd b) n) then
      nb + 1
    else
      nb
  ) 0 (gen_coord (n * n))
  
let is_solvable (grd: int list list): bool =
  let {x=x0; y=y0} = find_n grd 0 in
  let {x=xv0; y=yv0} = coord0 (length grd) in
  let nbp = count_permutation grd in
  let dst0 = (abs (x0 - xv0)) + (abs (y0 - yv0)) in
  0 == (nbp mod 2)

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

(* TODO *)
let scoring_grd_manhattan (grd: int list list): int = 0

let rec add_in_prio_queu (opened: np_node list) (to_add: np_node) : np_node list =
  match opened with
    [] -> [to_add]
    | h::t -> if get_score to_add > get_score h then
                to_add::opened
              else
                h::(add_in_prio_queu t to_add)

let scoring_node (scoring_grd: int list list -> int) (w: int) (greedy: bool) ({grd=grd; cost=cost}: np_node): int =
  w * (scoring_grd grd) + if greedy then 0 else cost

(* TODO max len + nb elements tot    ///  comp grd work in map ? *)
let a_start_solver (scoring_node: np_node -> int) (start: np_node): np_node =
  let closed = Hashtbl.create (1024 * 1024) in
  let opened = ref ([start]: np_node list) in
  while List.exists (fun e -> true) !opened && not (is_resolve (get_grd (hd !opened))) do
    let (frst: np_node) = hd !opened in
    let (neighbours: np_node list) = List.filter_map (fun m -> np_node_move frst m scoring_node) e_moves in
    let neighbours_not_in_closed = List.filter (fun e -> (Hashtbl.find_opt closed (get_grd frst)) == None) neighbours in
    opened := fold_left add_in_prio_queu (tl !opened) neighbours_not_in_closed;
    Hashtbl.add closed (get_grd frst) true
  done;
  start

  (* Input *)

let del_comment (s: string): string = hd (split_on_char '#' s)

let read_npuzzle_input (): int list list =

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

let print_npuzzle (np: int list list): unit =
  List.iter (fun l ->
    let _ = List.iter (fun e ->
      print_string ((string_of_int e) ^ " ")
    ) l in
      print_newline ()
  ) np

  (* Main *)

let (np: int list list) = read_npuzzle_input ()
let n = length np
let () = print_npuzzle np
let () = print_newline ()
let () = print_npuzzle (List.map (fun (line, i) ->
  List.map (fun (e, j) ->
    pos_to_num {x=j; y=i} n
  ) (indexed line)
) (indexed np))
let () = print_newline ()
let () = print_npuzzle (List.map (fun (line, i) ->
  List.map (fun (e, j) ->
    pos_to_num (num_to_pos e n) n
  ) (indexed line)
) (indexed np))
let () = print_newline ()
let () = print_string (string_of_bool (is_solvable np))
let () = print_newline ()
let () = print_string (string_of_int (count_permutation np))
let () = print_newline ()
let () = print_newline ()
let () = iter (fun {x=x; y=y} ->
  print_string ((string_of_int x) ^ " " ^ (string_of_int y));
  print_newline ()
) (gen_coord (n * n))

