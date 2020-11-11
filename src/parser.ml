(**
   Functionnal parser for specifications and programs
*)

open Opal
open Notations

(** {2 parsing helpers} *)

(** Parse one or more [p]s *)
let some p = many p >>= (function
    | [] -> mzero
    | xs -> return xs)

(** Let notation for bindings *)
let (let*) = (>>=)

let parens p = between (exactly '(') (exactly ')') p

(** {2 Usefull parsers} *)

(** Parse an identifier *)
let parse_id = some letter => implode

(** Parse a (non negative) natural number *)
let parse_nat = some digit => (implode % int_of_string)

(** Parse a relative number *)
let parse_rel =
  (token "-" >> parse_nat => (fun x -> -x))
  <|> parse_nat

(** Parse an integer number *)
let parse_int = parse_rel => (fun x -> Logic.Int x)

(** Parse variable *)
let parse_var = parse_id => (fun x -> Logic.Var x)

(** Parse items separated by a comma *)
let parse_commasep p = sep_by (spaces >> p) (token ",")

(** Parse items separated by a comma and parenthesized *)
let parse_args p = parens (parse_commasep p)

(** {2 Parse arithmetic operators} *)

let op_add = (spaces >> exactly '+' << spaces) >> return add
let op_sub = (spaces >> exactly '-' << spaces) >> return sub
let op_mul = (spaces >> exactly '*' << spaces) >> return mul
let op_div = (spaces >> exactly '/' << spaces) >> return div

(** {2 Parsers for logic connectors} *)

let op_and = (token "and") >> return (fun x y -> Logic.And (x, y))
let op_or = (token "or") >> return (fun x y -> Logic.Or (x, y))
let op_impl = (token "->") >> return (fun x y -> Logic.Impl (x, y))

(** {2 Parsers for arithmetic expressions} *)

(** Parse an expression *)
let rec parse_expr inp =
  (spaces >> chainl1 parse_product (op_add <|> op_sub)) inp

(** Parse a product *)
and parse_product inp =
  (spaces >> chainl1 parse_factor (op_mul <|> op_div)) inp

(** Parse the negation of an expression *)
and parse_negation inp =
  (token "-" >> parse_expr => (fun x -> Logic.Fun ("-", [x]))) inp

(** Parse a factor.
    A factor is either a negation, a parenthesised expression,
    a variable or an integer *)
and parse_factor inp =
  begin
    parse_negation
    <|> (spaces >> parens parse_expr)
    <|> (spaces >> parse_var)
    <|> (spaces >> parse_int)
  end inp

(** {2 Parsers for arithmetic comparisons } *)

(** A list of parsers for comparison symbols (<=, >=, <, >, =) *)
let comparators = List.map token ["<="; ">="; "<"; ">"; "="]

(** Parse a comparison expression *)
let parse_comp =
  let* a = parse_expr in
  let* b = choice comparators in
  let* c = parse_expr in
  return (Logic.Pred (b, [a; c]))

(** {2 Parsers for logic expressions } *)

(** Parse a predicate *)
let parse_pred =
  let* p = parse_id in
  let* b = parse_args parse_expr in
  return (Logic.Pred (p, b))

(** Parse a function application *)
let parse_fun =
  let* p = parse_id in
  let* b = parse_args parse_expr in
  return (Logic.Fun (p, b))

(** Parse a logic formula *)
let rec parse_form inp =
  begin
    parse_forall
    <|> parse_exists
    <|> parse_nq_form
  end inp

(** Parse a formula starting with a universal quantificator *)
and parse_forall inp =
  begin
    let* _  = token "forall" << spaces in
    let* vs = parse_commasep parse_id in
    let* _  = token ":" in
    let* f  = parse_form in
    return (Logic.Forall (vs, f))
  end inp

(** Parse a formula starting with an existential quantificator *)
and parse_exists inp =
  begin
    let* _  = token "exists" << spaces in
    let* vs = parse_commasep parse_id in
    let* _  = token ":" in
    let* f  = parse_form in
    return (Logic.Exitsts (vs, f))
  end inp

(** Parse a formula which does'nt start with a qantificator *)
and parse_nq_form inp =
  (spaces >> chainr1 parse_disjunction op_impl) inp

(** Parse a disjunction *)
and parse_disjunction inp   =
  (spaces >> chainl1 parse_conjunction op_or) inp

(** Parse a conjunction *)
and parse_conjunction inp   =
  (spaces >> chainl1 parse_atom op_and) inp

(** Parse a negation *)
and parse_not inp   =
  (token "not" >> spaces >> parse_atom => (fun x -> Logic.Not x)) inp

(** Parse an atomic formula.
    An atomic formula is either a quantified formula, a comparison, a negation,
    a single predicate applied to arguments or a parenthesized formula *)
and parse_atom inp  =
  begin
    parse_forall
    <|> parse_exists
    <|> parse_comp
    <|> parse_not
    <|> parse_pred
    <|> parens (parse_form)
  end inp

(** {2 Parsers for IMP programs } *)

(** Parse a sequence of statements *)
let rec parse_seq inp =
  ((some parse_stmt) => Imp.seqc_of_list) inp

(** Parse a single statement *)
and parse_stmt inp =
  ((spaces >> parse_aff)
   <|> (spaces >> parse_ifElse)
   <|> (spaces >> parse_if)
   <|> (spaces >> parse_while)) inp

(** Parse an affectation *)
and parse_aff inp =
  begin
    let* dst = spaces >> parse_id in
    let* _   = token "=" in
    let* src = parse_expr in
    let* _ = token ";" << spaces in
    return (Imp.Aff (dst, src))
  end inp

(** Parse a conditional (withou Else branch) *)
and parse_if inp =
  begin
    let* _ = token "if" in
    let* cond = spaces >> parens parse_nq_form << spaces in
    let* body = between (exactly '{') (exactly '}') parse_seq in
    return (Imp.If (cond, body))
  end inp

(** Parse a conditional *)
and parse_ifElse inp =
  begin
    let* _ = token "if" in
    let* cond = spaces >> parens parse_nq_form << spaces in
    let* body1 = between (exactly '{') (exactly '}') parse_seq in
    let* _ = token "else" in
    let* body2 = spaces >> between (exactly '{') (exactly '}') parse_seq in
    return (Imp.IfElse (cond, body1, body2))
  end inp

(** Parse while loop *)
and parse_while inp =
  begin
    let* _    = token "inv:" in
    let* inv  = spaces >> parse_nq_form in
    let* _    = token "var:" in
    let* var  = spaces >> parse_expr in
    let* _    = token "while" << spaces in
    let* cond = spaces >> parens parse_nq_form << spaces in
    let* body = between (exactly '{') (exactly '}') parse_seq in
    return (Imp.While (inv, var, cond, body))
  end inp

let parse_anot_prog =
  let* _    = token "pre:" << spaces in
  let* pre  = parse_form in
  let* _    = token "post:" << spaces in
  let* post = parse_form in
  let* prog = spaces >> parse_seq in
  return (pre, post, prog)

(** {2 String parsers} *)

let form_err = "Parsing error while parsing a formula"
let prog_err = "Parsing error while parsing a program"

let parse_all err p s =
  match (p << spaces) s with
  | Some (x, Nil) -> x
  | _ -> prerr_endline err; exit 1

let form_of_string s =
  parse_all form_err parse_form (LazyStream.of_string s)

let prog_of_string s =
  parse_all prog_err parse_seq (LazyStream.of_string s)

let anot_prog_of_string s =
  parse_all prog_err parse_anot_prog (LazyStream.of_string s)

let parse_file ic =
  parse_all prog_err parse_anot_prog (LazyStream.of_channel ic)