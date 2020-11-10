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

let prog' = "
  inv: x >= 0
  var: 10 - x
  while (x <= 10) {
    x = x + 1;
  }" |> parse_prog

(* Print the proof goal *)
let () =
  pre --> wp prog post
  |> str_of_form
  |> print_endline

let () =
  wp prog' top
  |> str_of_form
  |> print_endline