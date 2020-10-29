(*
 * re represents a regular expression as described by Ullman et al.
 * in 'Introduction to Automata Theory'.
 *)
type re =
  | Atom of atom
  | Union of re * re
  | Concatenation of re * re
  | Closure of re
  | Group of re

and atom = Epsilon | Null | Char of char

type re_snapshot_key = { i : int; j : int; k : int }

let pretty_print exp =
  let rec to_string exp =
    let atom_to_string a =
      match a with
      | Epsilon -> "ε"
      | Null -> "∅"
      | Char c -> Printf.sprintf "%c" c
    in
    match exp with
    | Atom a -> atom_to_string a
    | Union (e1, e2) -> Printf.sprintf "(%s + %s)" (to_string e1) (to_string e2)
    | Concatenation (e1, e2) ->
        Printf.sprintf "%s%s" (to_string e1) (to_string e2)
    | Closure e -> Printf.sprintf "(%s)*" (to_string e)
    | Group e -> Printf.sprintf "(%s)" (to_string e)
  in
  Printf.printf "%s\n" (to_string exp)
