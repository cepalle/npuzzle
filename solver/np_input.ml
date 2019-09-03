open String
open List

let read_npuzzle_input (): int list list =

  let del_comment (s: string): string = hd (split_on_char '#' s) in

  let parse_line (s: string): int list = filter_map (fun ss -> 
    if (String.length ss) == 0 then
      None
    else
      Some (int_of_string ss)
    ) (split_on_char ' ' (del_comment s))
  in
  
  let rec read_npuzzle_input_rec (i: int) (nl: int): int list list = 
    if i == 0 then
      []
    else
      (
        let lnum = parse_line (read_line ()) in
        if (List.length lnum) != nl then
          failwith "Bad number of number in a line"
        else
          lnum
      ) :: (read_npuzzle_input_rec (i - 1) nl)
  in

  let _ = read_line () in
  let nl = read_int_opt () in
    if nl == None then
      failwith "fail to get npuzzle size"
    else
      List.rev (read_npuzzle_input_rec (Option.get nl) (Option.get nl))

let print_npuzzle (np: int list list): unit =
  List.iter (fun l ->
    let _ = List.iter (fun e ->
      print_string ((string_of_int e) ^ " ")
    ) l in
      print_newline ()
  ) np
