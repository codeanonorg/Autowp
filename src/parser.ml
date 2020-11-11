(**
   Functionnal parser for specifications and programs
*)

open Opal
open Logic
open Imp
open Notations

let some p = many p >>= (function
    | [] -> mzero
    | xs -> return xs)

let parse_id = some letter => implode
let parse_nat = some digit => (implode % int_of_string)

let parse_var = parse_id => (fun x -> Var x)
let parse_int = parse_nat => (fun x -> Int x)

let parens p = between (exactly '(') (exactly ')') p

let parse_add = (spaces >> exactly '+' << spaces) >> return add
let parse_sub = (spaces >> exactly '-' << spaces) >> return sub
let parse_mul = (spaces >> exactly '*' << spaces) >> return mul
let parse_div = (spaces >> exactly '/' << spaces) >> return div

let parse_conj = (token "and") >> return (fun x y -> And (x, y))
let parse_disj = (token "or") >> return (fun x y -> Or (x, y))
let parse_impl = (token "->") >> return (fun x y -> Impl (x, y))

let rec parse_expr inp =
  (spaces >> chainl1 parse_term (parse_add <|> parse_sub)) inp
and parse_term inp =
  (spaces >> chainl1 parse_factor (parse_mul <|> parse_div)) inp
and parse_factor inp =
  (spaces >> (parens parse_expr <|> parse_var <|> parse_int)) inp

let (let*) = (>>=)

let comparators = List.map token ["<="; ">="; "<"; ">"; "="]

let parse_comp =
  let* a = parse_expr in
  let* b = choice comparators in
  let* c = parse_expr in
  return (Pred (b, [a; c]))

let parse_commasep p = sep_by (spaces >> p) (token ",")

let parse_args p = parens (parse_commasep p)

let parse_pred =
  let* p = parse_id in
  let* b = parse_args parse_expr in
  return (Pred (p, b))

let parse_fun =
  let* p = parse_id in
  let* b = parse_args parse_expr in
  return (Fun (p, b))

let rec parse_form inp =
  begin
    parse_forall
    <|> parse_exists
    <|> parse_nq_form
  end inp
and parse_forall inp =
  begin
    let* _  = token "forall" << spaces in
    let* vs = parse_commasep parse_id in
    let* _  = token ":" in
    let* f  = parse_form in
    return (Forall (vs, f))
  end inp
and parse_exists inp =
  begin
    let* _  = token "exists" << spaces in
    let* vs = parse_commasep parse_id in
    let* _  = token ":" in
    let* f  = parse_form in
    return (Exitsts (vs, f))
  end inp
and parse_nq_form inp =
  (spaces >> chainr1 parse_limpl parse_impl) inp
and parse_limpl inp   =
  (spaces >> chainl1 parse_lterm parse_disj) inp
and parse_lterm inp   =
  (spaces >> chainl1 parse_lfactor parse_conj) inp
and parse_not inp     =
  (token "not" >> spaces >> parse_lfactor => (fun x -> Not x)) inp
and parse_lfactor inp =
  begin
    parse_forall
    <|> parse_exists
    <|> parse_comp
    <|> parse_not
    <|> parse_pred
    <|> parens (parse_form)
  end inp


let rec parse_seq inp = ((some parse_stmt) => seqc_of_list) inp

and parse_stmt inp =
  ((spaces >> parse_aff)
   <|> (spaces >> parse_ifElse)
   <|> (spaces >> parse_if)
   <|> (spaces >> parse_while)) inp

and parse_aff =
  let* dst = spaces >> parse_id in
  let* _   = token "=" in
  let* src = parse_expr in
  let* _ = token ";" << spaces in
  return (Aff (dst, src))

and parse_if inp =
  begin
    let* _ = token "if" in
    let* cond = spaces >> parens parse_nq_form << spaces in
    let* body = between (exactly '{') (exactly '}') parse_seq in
    return (If (cond, body))
  end inp

and parse_ifElse inp =
  begin
    let* _ = token "if" in
    let* cond = spaces >> parens parse_nq_form << spaces in
    let* body1 = between (exactly '{') (exactly '}') parse_seq in
    let* _ = token "else" in
    let* body2 = spaces >> between (exactly '{') (exactly '}') parse_seq in
    return (IfElse (cond, body1, body2))
  end inp

and parse_while inp =
  begin
    let* _    = token "inv:" in
    let* inv  = spaces >> parse_nq_form in
    let* _    = token "var:" in
    let* var  = spaces >> parse_expr in
    let* _    = token "while" << spaces in
    let* cond = spaces >> parens parse_nq_form << spaces in
    let* body = between (exactly '{') (exactly '}') parse_seq in
    return (While (inv, var, cond, body))
  end inp

let parse_spec s = 
  match (parse_form << spaces) (LazyStream.of_string s) with
  | Some (x, Nil) -> x
  | Some (_, _) ->
    failwith "parse error [spec], remaining chars"
  | None ->
    failwith "parse error [spec]"

let parse_prog s =
  match (parse_stmt << spaces) (LazyStream.of_string s) with
  | Some (x, Nil) -> x
  | Some (_, _) ->
    failwith "parse error [prog], remaining chars"
  | None ->
    failwith "parse error [prog]"