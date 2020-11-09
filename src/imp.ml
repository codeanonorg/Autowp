open Logic

type expr = term

type cond =
  | Eq of expr * expr
  | Gt of expr * expr
  | Lt of expr * expr
  | Ge of expr * expr
  | GT of expr * expr
  | Ne of expr * expr

type stmt =
  | Aff of string * expr
  | If of cond * stmt
  | IfElse of cond * stmt * stmt
  | Seq of stmt * stmt

let rec wp (s : stmt) (q : formula) =
  match s with
  | Aff (v, e) ->
    alpha v e q
  | Seq (s1, s2) ->
    wp s1 (wp s2 q)
  | _ -> assert false