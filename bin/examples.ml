open Autowp.Parser
open Autowp.Logic
open Autowp.Imp
open Autowp.Notations

(* function MAX *)
let prog =
  "if (a >= b) {
    r = a - 1;
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

let () = "not (forall u : bounded(u) -> exists a, b : forall x : in(x, u) -> a <= x and b <= x)"
         |> parse_spec
         |> str_of_form
         |> print_endline

let () = "x <= 2 - x"
         |> parse_spec
         |> str_of_form
         |> print_endline