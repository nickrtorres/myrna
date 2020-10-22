open Dump
open Fe

let do_dump_ast = ref false

let main () =
  let args = [ ("-d", Arg.Set do_dump_ast, "Dump a myrna AST to STDOUT") ] in
  let usage = "myrnac: [-d] FILE" in
  let ast = ast () in
  Arg.parse args print_endline usage;
  if !do_dump_ast then dump_ast ast else ();
  try
    let name, _, _ = Me.into_ir ast in
    let _ = Be.context_struct name in
    let _ = Be.context_impl_from name "Entry" in
    let l = [ ("A", "a"); ("B", "b") ] in
    let _ = Be.context_impl_accepts l in
    Be.state_declarations [ "Entry"; "Terminal1" ];
    Be.state_interface [ "A"; "B"; "C" ];
    Be.interface_implementations [ ("S1", ("A", "S2")); ("S2", ("B", "S3")) ];
    ()
  with
  | Me.UndefinedState s ->
      Printf.printf "myrnac: error: encountered undefined state -> %s\n" s
  | Me.UndefinedTransition s ->
      Printf.printf "myrnac: error: encountered undefined transition -> %s\n" s

let _ = main ()
