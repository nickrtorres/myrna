open Parser
open Printf

let rec print_ast ast_list =
  match ast_list with
    | [] -> ()
    | hd::tl -> begin
      print_machine hd; print_ast tl
    end
and print_machine (iden, feature_list) = begin
      printf "MACHINE: %s\n" iden;
      print_feature_list feature_list
end
and print_feature_list fl =
  match fl with
    | [] -> ()
    | hd::tl -> begin
      print_feature hd;
      print_feature_list tl
    end
and print_feature f =
  match f with
    | Transition (iden, s) -> printf "\tTRANSITION %s -> %s\n" iden s
    | Entry (iden, s) -> begin
      printf "\tENTRY: %s\n" iden;
      print_state_transitions s
    end
    | Terminal (iden, s) -> begin
      printf "\tTERMINAL %s\n" iden;
      print_state_transitions s
    end
    | NonTerminal (iden, s) -> begin
      printf "\tNONTERMINAL %s\n" iden;
      print_state_transitions s
    end
and print_state_transitions transitions =
  match transitions with
    | [] -> ()
    | hd::tl -> begin
      print_transition hd;
      print_state_transitions tl
    end
and print_transition s =
  let from_state, to_state = s in
  printf "\t\t%s -> %s\n" from_state to_state

let dump_ast ast = 
  try
      print_ast ast;
      flush stdout
  with Lexer.Eof -> ()

