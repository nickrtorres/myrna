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

let print_token token =
  match token with
    | MACHINE -> printf "MACHINE"
    | TRANSITION -> printf "TRANSITION"
    | LBRACE -> printf "LBRACE"
    | RBRACE -> printf "RBRACE"
    | EQ -> printf "EQ"
    | STRING (s) -> printf "STRING %s" s
    | IDENT (s) -> printf "IDENT %s" s

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
