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

(** Variants are {!formulas} in {!Logic} *)
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

(**
   [wp prog post] computes the weakest precondition of program [prog] with respect
   to a postcondition [post].

   TODO : Add variants and terminations
*)
let rec wp (prog : stmt) (post : formula) =
  match prog with
  | Aff (name, expr) ->
    alpha name expr post
  | Seqc (stmt1, stmt2) ->
    wp stmt1 (wp stmt2 post)
  | If (cond, stmt) ->
    let case1 = Impl (cond, wp stmt post) in
    let case2 = Impl (Not cond, post) in
    And (case1, case2)
  | IfElse (cond, stmt1, stmt2) ->
    let case1 = Impl (cond, wp stmt1 post) in
    let case2 = Impl (Not cond, wp stmt2 post) in
    And (case1, case2)
  | While (inv, _, cond, stmt) ->
    let init = inv in
    let keep = Impl (And (cond, inv), wp stmt inv) in
    let next = Impl (And (Not cond, inv), post) in
    And (init, And (keep, next))


(** Convert a list of statements into a sequence *)
let rec seqc_of_list l =
  match l with
  | []  -> failwith "empty sequences are'nt allowed"
  | [x] -> x
  | x::xs -> Seqc (x, seqc_of_list xs)