(**
   A Logic to write specifications for {!Imp} programs
*)

(** Terms of the logic *)
type term =
  | Int of int
  | Var of string
  | Fun of string * term list

(** Formulae of the logic *)
type formula =
  | Pred of string * term list
  | And of formula * formula
  | Or of formula * formula
  | Impl of formula * formula
  | Not of formula

(** Substitute an id with a term in a term *)
let rec alpha_term v e = function
  | Var v' -> if v = v' then e else Var v'
  | Fun (s, ts) -> Fun (s, List.map (alpha_term v e) ts)
  | Int i -> Int i

(** Substitute an id with a term in a formula *)
let rec alpha v e f =
  match f with
  | Pred (p, ts)  -> Pred (p, List.map (alpha_term v e) ts)
  | Or (t1, t2)   -> Or (alpha v e t1, alpha v e t2)
  | And (t1, t2)  -> And (alpha v e t1, alpha v e t2)
  | Impl (t1, t2) -> Impl (alpha v e t1, alpha v e t2)
  | Not t -> Not (alpha v e t)

(** Term to string conversion *)
let rec str_of_term =
  function
  | Var v -> v
  | Fun (s, ts) -> begin
      match s, ts with
      | "+", [a; b] -> Printf.sprintf "(%s + %s)" (str_of_term a) (str_of_term b)
      | "-", [a; b] -> Printf.sprintf "(%s - %s)" (str_of_term a) (str_of_term b)
      | "*", [a; b] -> Printf.sprintf "(%s * %s)" (str_of_term a) (str_of_term b)
      | "/", [a; b] -> Printf.sprintf "(%s / %s)" (str_of_term a) (str_of_term b)
      | _, _ ->
        let args = List.fold_left (fun acc t -> acc ^ ", " ^ (str_of_term t)) "" ts
        in
        Printf.sprintf "%s(%s)" s args
    end
  | Int i -> string_of_int i

(** Formula to string conversion *)
let rec str_of_form f =
  match f with
  | Pred (p, ts) -> begin
      match p, ts with
      | "=", [a; b] -> Printf.sprintf "%s = %s" (str_of_term a) (str_of_term b)
      | "<=", [a; b] -> Printf.sprintf "%s <= %s" (str_of_term a) (str_of_term b)
      | "<", [a; b] -> Printf.sprintf "%s < %s" (str_of_term a) (str_of_term b)
      | ">=", [a; b] -> Printf.sprintf "%s >= %s" (str_of_term a) (str_of_term b)
      | ">", [a; b] -> Printf.sprintf "%s > %s" (str_of_term a) (str_of_term b)
      | "True", [] -> Printf.sprintf "True";
      | "False", [] -> Printf.sprintf "False";
      | _, _ ->
        let args = List.fold_left (fun acc t -> acc ^ (str_of_term t) ^ ", ") "" ts
        in
        Printf.sprintf "%s(%s)" p args
    end
  | Or (f1, f2) ->
    Printf.sprintf "%s \\/ %s" (str_of_form f1) (str_of_form f2)
  | And (f1, f2) ->
    Printf.sprintf "(%s /\\ %s)" (str_of_form f1) (str_of_form f2)
  | Impl (f1, f2) ->
    Printf.sprintf "(%s -> %s)" (str_of_form f1) (str_of_form f2)
  | Not f ->
    Printf.sprintf "!(%s)" (str_of_form f)