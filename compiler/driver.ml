open Parser
open Printf

let rec print_ast ast_list =
  match ast_list with
    | [] -> ()
    | x::xs -> begin
      print_machine x; print_ast xs
    end
and print_machine m =
  match m with
    | Machine (iden, feature_list) -> begin
      printf "MACHINE: %s\n" iden;
      print_feature_list feature_list
    end
and print_feature_list fl =
  match fl with
    | [] -> ()
    | f::fs -> begin
      print_feature f;
      print_feature_list fs
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
    | x::xs -> begin
      print_transition x;
      print_state_transitions xs
    end
and print_transition s =
  let from_state, to_state = s in
  printf "\t\t%s -> %s\n" from_state to_state

let dump_ast () = 
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.program Lexer.token lexbuf in
      print_ast result;
      flush stdout
    done
  with Lexer.Eof -> ()

let do_dump_ast = ref false

let main () = begin
  let args = [("-d", Arg.Set do_dump_ast, "Dump a myrna AST to STDOUT")] in
  let usage = "myrnac: [-d] FILE" in
  Arg.parse args print_endline usage;
  if !do_dump_ast then dump_ast () else ()
end

let _ = main ()
