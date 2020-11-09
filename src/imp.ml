open Logic

type expr = term

type cond = formula

type stmt =
  | Aff of string * expr
  | If of cond * stmt
  | IfElse of cond * stmt * stmt
  | Seqc of stmt * stmt
  (* TODO : WHILE loop *)

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

let (===) a b = eq a b
let (&&&) a b = And (a, b)
let (|||) a b = Or (a, b)
let (-->) a b = Or (a, b)
let (<--) a b = Aff (a, b)
let seq a b = Seqc (a, b)

let rec mk_seq l = 
  match l with
  | [] -> failwith "bad arg mk_seq"
  | [x] -> x
  | x::xs -> seq x (mk_seq xs)

let ifThen a b = If (a, b)

let ifElse a b c = IfElse (a, b, c)

let ex_prog_swap () =
  let va = Var "A" in
  let vb = Var "B" in
  let vx = Var "x" in
  let vy = Var "y" in
  let tmp = Var "tmp" in
  let pre = And (eq vx va , eq vy vb) in
  let post = And (eq vx vb , eq vy va) in
  let prog = mk_seq [
      "tmp" <-- vx;
      "x"   <-- vy;
      "y"   <-- tmp;
    ] in
  Impl (pre, wp prog post) |> str_of_form |> print_endline

let ex_prog_max () =
  let va = Var "a" in
  let vb = Var "b" in
  let vr = Var "r" in
  let pre = top in
  let post = And (ge vr va, ge vr vb) in
  let prog =
    ifElse (ge va vb)
      ("r" <-- vb)
      ("r" <-- va)
  in
  Impl (pre, wp prog post) |> str_of_form |> print_endline



