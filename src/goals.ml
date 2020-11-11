open Imp
open Logic
open Notations

let rec list_of_prog s =
  match s with
  | Aff _ | If _ | IfElse _ | While _ -> [s]
  | Seqc (s1, s2) -> (list_of_prog s1) @ (list_of_prog s2)


let next_while ls =
  let rec step acc = function
    | [] -> (List.rev acc, [])
    | While _::_ as l -> List.rev acc, l
    | x::xs -> step (x::acc) xs
  in
  step [] ls

let goals i pre post prog =
  let ls = list_of_prog prog in
  match next_while ls with
  | (p, []) ->
    [(i, pre --> wp (seqc_of_list p) post)]
  | (_, (While (inv, _, cond, body))::xs) ->
    let init = inv in
    let keep = Forall (vars body, Impl (And (cond, inv), wp body inv)) in
    let next = wp (seqc_of_list xs) post in
    [
      (i, pre --> init);
      (i + 1, pre --> keep);
      (i + 2, (pre &&& inv &&& (Not cond)) --> next);
    ]
  | (_, _) -> assert false