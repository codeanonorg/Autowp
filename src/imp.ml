(**
   A minimalist imperative programming language to 
   write small arithmetic algorithms.

   To simplify the computation of the Weakest Preconditions,
   we assume that expressions (resp. conditions) in IMP are equivalent
   to terms (resp. formulae) in {!Logic}.
*)

open Logic

(**
   Expressions of IMP are terms in {!Logic}
*)
type expr = term

(**
   Conditions of IMP are {!formulas} in {!Logic}
*)
type cond = formula

(**
   IMP statements
   TODO : while loop
*)
type stmt =
  | Aff of string * expr
  | If of cond * stmt
  | IfElse of cond * stmt * stmt
  | Seqc of stmt * stmt

(**
   [wp prog post] computes the weakest precondition of program [prog] with respect
   to a postcondition [post].
*)
let rec wp (s : stmt) (q : formula) =
  match s with
  | Aff (v, e) ->
    alpha v e q
  | Seqc (s1, s2) ->
    wp s1 (wp s2 q)
  | If (c, s) ->
    Impl (c, wp s q)
  | IfElse (c, s1, s2) ->
    let case1 = Impl (c, wp s1 q) in
    let case2 = Impl (Not c, wp s2 q) in
    And (case1, case2)

(** Convert a list of statements into a sequence *)
let rec seqc_of_list l =
  match l with
  | []  -> failwith "empty sequences are'nt allowed"
  | [x] -> x
  | x::xs -> Seqc (x, seqc_of_list xs)