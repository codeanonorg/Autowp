(**
   Notations to write IMP programs and their specification
   in OCaml.
*)

open Imp
open Logic

(** {2 Programs} *)

(** Affectation *)
let (<--) a b = Aff (a, b)

(** Condition (If) *)
let ifThen a b = If (a, b)

(** Condition (If else) *)
let ifElse a b c = IfElse (a, b, c)

(** Equality test  *)
let eq a b = Pred ("=", [a; b])

(** Greater than  *)
let gt a b = Pred (">", [a; b])

(** Greater or equal  *)
let ge a b = Pred (">=", [a; b])

(** Lesser or equal *)
let le a b = Pred ("<=", [a; b])

(** Lesser than *)
let lt a b = Pred ("<", [a; b])

(** Indexed sums *)
let sum i j t = Fun ("Sum", [i; j; t])

(** Sum *)
let add a b = Fun ("+", [a; b])

(** Subtraction *)
let sub a b = Fun ("-", [a; b])

(** Multiplication *)
let mul a b = Fun ("*", [a; b])

(** Division *)
let div a b = Fun ("/", [a; b])

(** {2 Specifications} *)

(** Equality test*)
let (===) a b = eq a b

(** And *)
let (&&&) a b = And (a, b)

(** Or *)
let (|||) a b = Or (a, b)

(** Implication *)
let (-->) a b = Impl (a, b)

(** Top *)
let top = Pred ("True", [])

(** Bottom *)
let bot = Pred ("False", [])
