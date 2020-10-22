open Parser

let transition_table = Hashtbl.create 53
(* Really just a hash set but I can't find a type that works like that in the
 * stdlib
 *)
let state_table = Hashtbl.create 53
exception UndefinedState of string
exception UndefinedTransition of string

let rec store_transitions ast =
  List.iter (fun (_, features) -> List.iter store features) ast
and store feature =
  match feature with
    | Transition (from, to_) -> Hashtbl.add transition_table from to_
    | Entry (_, _) | Terminal (_, _) | NonTerminal (_, _) -> ()

let rec store_states ast =
  List.iter (fun (_, features) -> List.iter store features) ast
and store feature =
  match feature with
    | Transition _ -> ()
    | Entry (iden, _) | Terminal (iden, _) | NonTerminal (iden, _) -> begin
      Hashtbl.add state_table iden ()
    end

let rec check_transitions ast =
  List.iter (fun (_, features) -> List.iter check_feature features) ast
and check_feature feature =
  match feature with
    | Transition _ -> ()
    | Entry (_, states)
      | Terminal (_, states)
      | NonTerminal (_, states) -> List.iter check states
and check (from, to_) =
  if not (Hashtbl.mem transition_table from) then raise (UndefinedTransition (from))
  else if not (Hashtbl.mem state_table to_) then raise (UndefinedState (to_))
  else ()
