open Dump
open Fe

let do_dump_ast = ref false

let main () =
  let args = [ ("-d", Arg.Set do_dump_ast, "Dump a myrna AST to STDOUT") ] in
  let usage = "myrnac: [-d] FILE" in
  let ast = ast () in
  Arg.parse args print_endline usage;
  if !do_dump_ast then dump_ast ast else ();
  ()

let () = main ()
