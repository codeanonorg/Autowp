open Imp
open Logic
open Notations

(**
   Generate preconditions
*)
let rec gen_pre (prog : stmt) (post : formula) =
  match prog with
  | Aff (name, expr) ->
    subst name expr post
  | Seqc (stmt1, stmt2) ->
    gen_pre stmt1 (gen_pre stmt2 post)
  | If (cond, stmt) ->
    let case1 = Impl (cond, gen_pre stmt post) in
    let case2 = Impl (Not cond, post) in
    And (case1, case2)
  | IfElse (cond, stmt1, stmt2) ->
    let case1 = Impl (cond, gen_pre stmt1 post) in
    let case2 = Impl (Not cond, gen_pre stmt2 post) in
    And (case1, case2)
  | While (inv, _, _, _) -> inv

(**
   Generate preconditions
*)
let rec gen_vc (prog : stmt) (post : formula) =
  match prog with
  | Aff (_, _)    -> []
  | Seqc (a, b)   -> gen_vc a (gen_pre b post) @ gen_vc b post
  | If (_, a)     -> gen_vc a post
  | IfElse (_, a, b)  -> gen_vc a post @ gen_vc b post
  | While (inv, _, cond, body) ->
    (gen_vc body inv) @
    [Forall (vars body, (cond &&& inv) --> gen_pre body inv)] @
    [Forall (vars body, (Not cond &&& inv) --> post)]

let gen_goals (pre : formula) (prog : stmt) (post : formula) =
  [pre --> gen_pre prog post] @ gen_vc prog post

(**
   [wp prog post] computes the weakest precondition of program [prog] with respect
   to a postcondition [post].
   [wp] computes preconditions for partial correction only.

   TODO : Add variants and terminations
*)
let rec wp (prog : stmt) (post : formula) =
  match prog with
  | Aff (name, expr) ->
    subst name expr post
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
    let aff_vars = vars stmt in
    let init = inv in
    let keep = Forall (aff_vars, Impl (And (cond, inv), wp stmt inv)) in
    let next = Forall (aff_vars, Impl (And (Not cond, inv), post)) in
    And (init, And (keep, next))

and make_decr inv var cond stmt =
  let invnc = And (inv, cond) in
  let init = Pred ("=", [Var "?var"; var]) in
  let min0 = Pred ("<=", [Int 0; Var "?var"]) in
  let next = Pred ("<=", [var; Var "?var"]) in
  let iter = wp stmt next in
  Impl (And (invnc, init), And (iter, min0))


let next_while ls =
  let rec step acc = function
    | [] -> (List.rev acc, [])
    | While _::_ as l -> List.rev acc, l
    | x::xs -> step (x::acc) xs
  in
  step [] ls