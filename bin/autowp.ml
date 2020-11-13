open Autowp_lib.Parser
open Autowp_lib.Logic
open Autowp_lib.Goals
open Filename

let input = Sys.argv.(1)

let (pre, post, prog) = parse_file (open_in input)

let () =
  let vfile = open_out (remove_extension (basename input) ^ ".v") in
  print_endline "Precondition :";
  Printf.printf "   %s\n\n" (str_of_form pre);
  print_endline "Postcondition :";
  Printf.printf "   %s\n\n" (str_of_form post);
  print_endline "What remains to prove after wp computations :";
  List.iteri (fun i x ->
      Printf.printf "%d.]\n" i;
      Printf.printf "   %s\n" (str_of_form x);
      generate_coq vfile (Printf.sprintf "lemma_%d" i) x;

    ) (gen_goals pre prog post);
  close_out vfile;
  print_endline "[2. termination] not available yet"