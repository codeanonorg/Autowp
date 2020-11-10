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
  var: x
  while (x > 0) {
    x = x - 1;
  }" |> parse_prog

let post' = "x = 0" |> parse_spec

(* Print the proof goal *)
let () =
  pre --> wp prog post
  |> str_of_form
  |> print_endline

let () =
  wp prog' post'
  |> str_of_form
  |> print_endline

let () = "(a = a or b = b and c = c) or d = d"
         |> parse_spec
         |> str_of_form
         |> print_endline