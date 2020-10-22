open Parser

let transition_table = Hashtbl.create 53
exception UndefinedState of string

let rec store_transitions ast =
  List.iter (fun (_, features) -> List.iter store features) ast
and store feature =
  match feature with
    | Transition (from, to_) -> Hashtbl.add transition_table from to_
    | Entry (_, _) | Terminal (_, _) | NonTerminal (_, _) -> ()

let rec check_transitions ast =
  List.iter (fun (_, features) -> List.iter check_feature features) ast
and check_feature feature =
  match feature with
    | Transition _ -> ()
    | Entry (_, states)
      | Terminal (_, states)
      | NonTerminal (_, states) -> List.iter check states
and check (from, _) =
  if Hashtbl.mem transition_table from then ()
  else raise (UndefinedState (from))
