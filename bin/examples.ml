open Autowp.Parser
open Autowp.Logic
open Autowp.Imp
open Autowp.Notations

(* function MAX *)
let prog =
  "if (a >= b) {
    r = a;
   } else {
    r = b;
   }" |> parse_prog

(* Trivial precondition *)
let pre = top

(* Correction of MAX *)
let post = "r >= a and r >= b" |> parse_spec

(* Print the proof goal *)
let () =
  pre --> wp prog post
  |> str_of_form
  |> print_endline