open Autowp_lib.Parser
open Autowp_lib.Logic
open Autowp_lib.Notations
open Autowp_lib.Imp

let input = Sys.argv.(1)

let (pre, post, prog) = parse_file (open_in input)


let () =
  print_endline "Precondition  :";
  Printf.printf "   %s\n\n" (str_of_form pre);
  print_endline "Postcondition :";
  Printf.printf "   %s\n\n" (str_of_form post);
  print_endline "What remains to prove after wp computations :";
  print_endline "[1. partial correction]";
  Printf.printf "   %s\n\n" (pre --> (wp prog post) |> str_of_form);
  print_endline "[2. termination] not available yet"