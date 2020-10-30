open Re

(*
 * Simplify implements a number of the rules laid out by Ullman et al. in
 * 'Introduction to Automata Theory' -- namely section 3.4.2 and 3.4.5.
 *)

let rec simplify exp =
  match exp with
  | Concatenation (e1, e2) -> simplify_concatenation e1 e2
  | Closure e -> simplify_closure e
  | e -> e

and simplify_concatenation e1 e2 =
  match (e1, e2) with
  | Atom Null, e -> Atom Null
  | e, Atom Null -> Atom Null
  | Atom Epsilon, e -> e
  | e, Atom Epsilon -> e
  | e1, e2 -> Concatenation (e1, e2)

and simplify_closure e =
  match e with Atom Null | Atom Epsilon -> Atom Epsilon | e -> Closure e
