open Parser

let transition_table = Hashtbl.create 53

let state_table = Hashtbl.create 53

exception UndefinedState of string

exception UndefinedTransition of string

let run_semantic_analysis ast =
  let rec store_transitions ast =
    List.iter (fun (_, features) -> List.iter store features) ast
  and store feature =
    match feature with
    | Transition (from, to_) -> Hashtbl.add transition_table from to_
    | Entry _ | Terminal _ | NonTerminal _ -> ()
  in

  let rec store_states ast =
    List.iter (fun (_, features) -> List.iter store features) ast
  and store feature =
    match feature with
    | Transition _ -> ()
    | Entry (iden, _) | Terminal (iden, _) | NonTerminal (iden, _) ->
        Hashtbl.add state_table iden ()
  in

  let rec check_transitions ast =
    List.iter (fun (_, features) -> List.iter check_feature features) ast
  and check_feature feature =
    match feature with
    | Transition _ -> ()
    | Entry (_, states) | Terminal (_, states) | NonTerminal (_, states) ->
        List.iter check states
  and check (from, to_) =
    if not (Hashtbl.mem transition_table from) then
      raise (UndefinedTransition from)
    else if not (Hashtbl.mem state_table to_) then raise (UndefinedState to_)
    else ()
  in
  store_transitions ast;
  store_states ast;
  check_transitions ast

(* TODO *)
let into_ir ast =
  let context =
    match ast with
    | [] -> "UNK"
    | hd :: _ ->
        let iden, _ = hd in
        iden
  in
  let state_interface =
    Hashtbl.fold (fun k _ acc -> k :: acc) transition_table []
  in
  let states = Hashtbl.fold (fun k _ acc -> k :: acc) state_table [] in

  (context, state_interface, states)
