open Dump
open Fe

let do_dump_ast = ref false

let main () =
  let args = [("-d", Arg.Set do_dump_ast, "Dump a myrna AST to STDOUT")] in
  let usage = "myrnac: [-d] FILE" in
  let ast = ast () in begin
    Arg.parse args print_endline usage;
    if !do_dump_ast then dump_ast ast else ();
    try
      Me.into_ir ast;
    with
      | Me.UndefinedState (s) ->
        Printf.printf "myrnac: error: encountered undefined state -> %s\n" s
      | Me.UndefinedTransition (s) ->
        Printf.printf "myrnac: error: encountered undefined transition -> %s\n" s
  end

let _ = main ()
