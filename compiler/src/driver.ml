open Token;; 
let rec to_string token =
  let t = match token with 
    | ACCEPTS       -> "ACCEPTS"
    | ARROW         -> "ARROW" 
    | MACHINE       -> "MACHINE"
    | TRANSITION    -> "TRANSITION"
    | ENTRY         -> "ENTRY"
    | TERMINAL      -> "TERMINAL"
    | NONTERMINAL   -> "NONTERMINAL"
    | EOL           -> "EOL"
    | LBRACE        -> "LBRACE"
    | RBRACE        -> "RBRACE"
    | COMMA         -> "COMMA"
    | EQ            -> "EQ"
    | STRING (s)    -> "STRING " ^ "( " ^ s ^ " )"
    | IDENT (s)     -> "IDENT " ^ "( " ^ s ^ " )"
  in
    t
;;

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      print_string (to_string (Lexer.token lexbuf));
      print_newline()
    done
  with Lexer.Eof ->
    exit 0
