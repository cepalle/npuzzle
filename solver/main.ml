open String
open List
open Printf

type np_option = {
  hamming: bool;
  chebyschev: bool;
  euclidean: bool;
  manhattan: bool;
  greedy: bool;
  weight: int;
}

let get_np_option (argv: string array): np_option = {
  hamming= Array.exists (fun e -> String.compare e "-dh" == 0) argv;
  chebyschev= Array.exists (fun e -> String.compare e "-dc" == 0) argv;
  euclidean= Array.exists (fun e -> String.compare e "-de" == 0) argv;
  manhattan= Array.exists (fun e -> String.compare e "-dm" == 0) argv;
  greedy= Array.exists (fun e -> String.compare e "-grd" == 0) argv;
  weight= if Array.exists (fun e -> String.compare e "-w" == 0) argv then
            Array.fold_left (fun w s ->
              if int_of_string_opt s == None  then w
              else int_of_string s
            ) 1 argv
          else 1;
}

let nop_to_scoring ({
  hamming=hm;
  chebyschev=ch;
  euclidean=eu;
  manhattan=ma;
  greedy=grd;
  weight=w;
}: np_option): Np_solver.np_node -> int = 
  Np_solver.scoring_node (Np_solver.scoring_grd_dst (
    if ma then Np_solver.dist_manhattan
    else if eu then Np_solver.dist_euclidean
    else if ch then Np_solver.dist_chebyshev
    else if hm then Np_solver.dist_hamming
    else Np_solver.dist_manhattan
  )) w grd

let nop = get_np_option Sys.argv
let scoring = nop_to_scoring nop

let (grd: int list list) = Np_input.read_npuzzle_input ()
let n = length grd

let () = print_int n
let () = print_newline ()
let () = Np_input.print_npuzzle grd
let () = print_newline ()

let node = Np_solver.a_star_solver scoring grd
let _ = Option.map Np_solver.np_print_a_star_res node
