open Parser

(* 
 * Me is the middle end of myrnac. Me performs semantic analysis, etc.
 *)

let transition_table = Hashtbl.create 53
exception UndefinedState of string


(*
 * Records transitions specified by the user in our transition table. This
 * routine is destructive to the transition_table.
 *)
let rec store_transitions ast =
  match ast with
    | [] -> ()
    | hd::_ -> store_transitions_machine hd
and store_transitions_machine machine=
  match machine with
    | Machine (_, feature_list) -> store_transitions_feature_list feature_list
and store_transitions_feature_list feature_list =
  match feature_list with
    | [] -> ()
    | hd::tl -> begin
      store_transitions_machine_feature hd;
      store_transitions_feature_list tl
    end
and store_transitions_machine_feature machine_feature =
  match machine_feature with
    | Transition (iden, c) -> Hashtbl.add transition_table iden c
    | _ -> ()

let rec check_transitions ast =
  match ast with
    | [] -> ()
    | hd::_ -> check_transitions_machine hd
and check_transitions_machine machine=
  match machine with
    | Machine (_, feature_list) -> check_transitions_feature_list feature_list
and check_transitions_feature_list feature_list =
  match feature_list with
    | [] -> ()
    | hd::tl -> begin
      check_transitions_machine_feature hd;
      check_transitions_feature_list tl
    end
and check_transitions_machine_feature machine_feature =
  match machine_feature with
    | Terminal (_, state_list)
      | NonTerminal (_, state_list)
      | Entry (_, state_list) -> check_state_list state_list
    | _ -> ()
and check_state_list state_list =
  match state_list with
    | [] -> ()
    | hd::_ -> begin
      let from_state, to_state = hd in
        if Option.is_none (Hashtbl.find_opt transition_table from_state) then
          raise (UndefinedState (from_state))
        else if Option.is_none (Hashtbl.find_opt transition_table to_state) then
          raise (UndefinedState (to_state))
        else
        ()
      end
