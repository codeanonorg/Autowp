(**
   A minimalist imperative programming language to 
   write small arithmetic algorithms.

   To simplify the computation of the Weakest Preconditions,
   we assume that expressions (resp. conditions) in IMP are equivalent
   to terms (resp. formulae) in {!Logic}.
*)

open Logic

(** Expressions of IMP are terms in {!Logic} *)
type expr = term

(** Conditions of IMP are {!formulas} in {!Logic} *)
type cond = formula

(** Invariants are {!formulas} in {!Logic} *)
type invariant = formula

(** Variants are arithmetic expressions *)
type variant = expr

(**
   IMP statements
   TODO : while loop
*)
type stmt =
  | Aff of string * expr
  | If of cond * stmt
  | IfElse of cond * stmt * stmt
  | While of invariant * variant * cond * stmt
  | Seqc of stmt * stmt


module VarSet = Set.Make(String)

let vars stmt =
  let rec step = function
    | Aff (x, _)  -> VarSet.singleton x
    | If (_, s)   -> step s
    | IfElse (_, s1, s2) -> VarSet.union (step s1) (step s2)
    | Seqc (s1, s2)      -> VarSet.union (step s1) (step s2)
    | While (_, _, _, s) -> step s
  in
  VarSet.fold (List.cons) (step stmt) []

(** Convert a list of statements into a sequence *)
let rec prog_of_list l =
  match l with
  | []  -> failwith "empty sequences are'nt allowed"
  | [x] -> x
  | x::xs -> Seqc (x, prog_of_list xs)

(** Convert a sequence into a list of statements *)
let rec list_of_prog s =
  match s with
  | Aff _ | If _ | IfElse _ | While _ -> [s]
  | Seqc (s1, s2) -> (list_of_prog s1) @ (list_of_prog s2)