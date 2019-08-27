open String
open List

let del_comment (s: string) : string = hd (split_on_char '#' s)
;;

let parse_line (s : string) : int list = filter_map (fun ss -> 
if (String.length ss) == 0 then
  None
else
  Some (int_of_string ss)
) (split_on_char ' ' s)
;;

let read_npuzzle_input () : int list list =
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
;;

let print_npuzzle np =
  List.map (fun l ->
    let _ = List.map (fun e ->
      print_string ((string_of_int e) ^ " ")
    ) l in
      print_newline ()
  ) np
;;

  (* Main *)

let np = read_npuzzle_input () in
  print_npuzzle np
;;
