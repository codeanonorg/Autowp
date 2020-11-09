type term =
  | Int of int
  | Var of string
  | Fun of string * term list

type formula = Pred of string * term list

let eq a b = Pred ("=", [a; b])

let gt a b = Pred (">", [a; b])

let ge a b = Pred (">=", [a; b])

let le a b = Pred ("<=", [a; b])

let lt a b = Pred ("<", [a; b])

let sum i j t = Fun ("Sum", [i; j; t])

let plus a b = Fun ("+", [a; b])

let rec alpha_term v e = function
  | Var v' -> if v = v' then e else Var v'
  | Fun (s, ts) -> Fun (s, List.map (alpha_term v e) ts)
  | Int i -> Int i

let alpha v e (Pred (p, ts)) = Pred (p, List.map (alpha_term v e) ts)
