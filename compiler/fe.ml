let ast () =
  let lexbuf = Lexing.from_channel stdin in
  let result = Parser.program Lexer.token lexbuf in
  result
