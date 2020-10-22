open Parser
open Printf

let rec print_ast ast_list =
  match ast_list with
  | [] -> ()
  | hd :: tl ->
      print_machine hd;
      print_ast tl

and print_machine (iden, feature_list) =
  printf "MACHINE: %s\n" iden;
  print_feature_list feature_list

and print_feature_list fl =
  match fl with
  | [] -> ()
  | hd :: tl ->
      print_feature hd;
      print_feature_list tl

and print_feature f =
  match f with
  | Transition (iden, s) -> printf "\tTRANSITION %s -> %s\n" iden s
  | Entry (iden, s) ->
      printf "\tENTRY: %s\n" iden;
      print_state_transitions s
  | Terminal (iden, s) ->
      printf "\tTERMINAL %s\n" iden;
      print_state_transitions s
  | NonTerminal (iden, s) ->
      printf "\tNONTERMINAL %s\n" iden;
      print_state_transitions s

and print_state_transitions transitions =
  match transitions with
  | [] -> ()
  | hd :: tl ->
      print_transition hd;
      print_state_transitions tl

and print_transition s =
  let from_state, to_state = s in
  printf "\t\t%s -> %s\n" from_state to_state

let dump_ast ast =
  try
    print_ast ast;
    flush stdout
  with Lexer.Eof -> ()
