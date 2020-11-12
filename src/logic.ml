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
  | Forall (ts, f') ->
    if List.mem v ts then f
    else Forall (ts, subst v e f')
  | Exitsts (ts, f') ->
    if List.mem v ts then f'
    else Exitsts (ts, subst v e f')
  | Pred (p, fs)  -> Pred (p, List.map (subst_term v e) fs)
  | Or (f1, f2)   -> Or (subst v e f1, subst v e f2)
  | And (f1, f2)  -> And (subst v e f1, subst v e f2)
  | Impl (f1, f2) -> Impl (subst v e f1, subst v e f2)
  | Not f -> Not (subst v e f)

let rec sep_by c = function
  | [] -> ""
  | [x] -> x
  | x::xs -> x ^ c ^ sep_by c xs

let commas = sep_by ", "

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

(** Formula to string conversion *)
let rec coq_of_form f =
  let parens subf =
    if priority subf < priority f then
      "(" ^ (coq_of_form subf) ^ ")"
    else coq_of_form subf
  in
  match f with
  | Forall (ts, f) ->
    sprintf "forall (%s : Z), %s" (sep_by " " ts) (coq_of_form f)
  | Exitsts (ts, f) ->
    sprintf "exists (%s : Z), %s" (sep_by " " ts) (coq_of_form f)
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
    sprintf "(%s) -> %s" (coq_of_form a) (parens b)
  | Impl (a, b) ->
    sprintf "%s -> %s" (parens a) (parens b)
  | And (a, b) ->
    sprintf "%s /\\ %s" (parens a) (parens b)
  | Or (a, b) ->
    sprintf "%s \\/ %s" (parens a) (parens b)
  | Not f ->
    sprintf "not (%s)" (coq_of_form f)

module VarSet = Set.Make (String)

let rec free_vars_term = function
  | Var v -> VarSet.singleton v
  | Fun (_, ts) ->
    List.map free_vars_term ts
    |> List.fold_left VarSet.union VarSet.empty
  | _ -> VarSet.empty

let rec free_vars = function
  | Forall (ts, f) | Exitsts (ts, f) ->
    VarSet.(diff (free_vars f) (of_list ts))
  | Pred (_, ts) ->
    List.map free_vars_term ts
    |> List.fold_left VarSet.union VarSet.empty
  | And (a, b) | Or (a, b) | Impl (a, b) ->
    VarSet.union (free_vars a) (free_vars b)
  | Not f -> free_vars f


let generate_coq name f =
  let oc = open_out (name ^ ".v") in
  let fv = VarSet.fold List.cons (free_vars f) [] in
  let ff = Forall (fv, f) in
  Printf.fprintf oc "(* This file is generated by AutoWp *)\n\n";
  Printf.fprintf oc "Require Import ZArith Omega.\n\n";
  Printf.fprintf oc "Open Scope Z_scope.\n";
  Printf.fprintf oc "Lemma %s :\n" name;
  Printf.fprintf oc "\t%s.\n" (coq_of_form ff);
  Printf.fprintf oc "Proof.\n";
  Printf.fprintf oc "\ttry (intros; auto; omega).\n";
  Printf.fprintf oc "Qed.\n";
  close_out oc
