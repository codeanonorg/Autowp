(**
   A Logic to write specifications for {!Imp} programs
*)

open Printf

(** Terms of the logic *)
type term =
  | Int of int
  | Var of string
  | Fun of string * term list

(** Formulae of the logic *)
type formula =
  | Forall of string list * formula
  | Exitsts of string list * formula
  | Pred of string * term list
  | And of formula * formula
  | Or of formula * formula
  | Impl of formula * formula
  | Not of formula

(** Substitute an id with a term in a term *)
let rec subst_term v e = function
  | Var v' -> if v = v' then e else Var v'
  | Fun (s, ts) -> Fun (s, List.map (subst_term v e) ts)
  | Int i -> Int i

(** Substitute an id with a term in a formula *)
let rec subst v e f =
  match f with
  | Forall (ts, f) -> Forall (ts, subst v e f)
  | Exitsts (ts, f) -> Exitsts (ts, subst v e f)
  | Pred (p, fs)  -> Pred (p, List.map (subst_term v e) fs)
  | Or (f1, f2)   -> Or (subst v e f1, subst v e f2)
  | And (f1, f2)  -> And (subst v e f1, subst v e f2)
  | Impl (f1, f2) -> Impl (subst v e f1, subst v e f2)
  | Not f -> Not (subst v e f)

let rec commas = function
  | [] -> ""
  | [x] -> x
  | x::xs -> x ^ ", " ^ commas xs

(** Term to string conversion *)
let rec str_of_term =
  function
  | Var v -> v
  | Fun (s, ts) -> begin
      match s, ts with
      | "+", [a; b] -> sprintf "(%s + %s)" (str_of_term a) (str_of_term b)
      | "-", [a; b] -> sprintf "(%s - %s)" (str_of_term a) (str_of_term b)
      | "*", [a; b] -> sprintf "(%s * %s)" (str_of_term a) (str_of_term b)
      | "/", [a; b] -> sprintf "(%s / %s)" (str_of_term a) (str_of_term b)
      | _, _ -> sprintf "%s(%s)" s (List.map str_of_term ts |> commas)
    end
  | Int i -> string_of_int i

let priority = function
  | Pred _    -> 7
  | Forall _  -> 1
  | Exitsts _ -> 2
  | Impl _    -> 3
  | Or _      -> 4
  | And _     -> 5
  | Not _     -> 6

(** Formula to string conversion *)
let rec str_of_form f =
  let parens subf =
    if priority subf < priority f then
      "(" ^ (str_of_form subf) ^ ")"
    else str_of_form subf
  in
  match f with
  | Forall (ts, f) ->
    sprintf "Forall %s : %s" (commas ts) (str_of_form f)
  | Exitsts (ts, f) ->
    sprintf "Exists %s : %s" (commas ts) (str_of_form f)
  | Pred (p, ts) -> begin
      match p, ts with
      | "=", [a; b]   -> sprintf "%s = %s" (str_of_term a) (str_of_term b)
      | "<=", [a; b]  -> sprintf "%s <= %s" (str_of_term a) (str_of_term b)
      | "<", [a; b]   -> sprintf "%s < %s" (str_of_term a) (str_of_term b)
      | ">=", [a; b]  -> sprintf "%s >= %s" (str_of_term a) (str_of_term b)
      | ">", [a; b]   -> sprintf "%s > %s" (str_of_term a) (str_of_term b)
      | "True", []    -> sprintf "True";
      | "False", []   -> sprintf "False";
      | _, _ -> sprintf "%s(%s)" p (List.map str_of_term ts |> commas)
    end
  | Impl ((Impl _ as a), b) ->
    sprintf "(%s) -> %s" (str_of_form a) (parens b)
  | Impl (a, b) ->
    sprintf "%s -> %s" (parens a) (parens b)
  | And (a, b) ->
    sprintf "%s and %s" (parens a) (parens b)
  | Or (a, b) ->
    sprintf "%s or %s" (parens a) (parens b)
  | Not f ->
    sprintf "not %s" (parens f)