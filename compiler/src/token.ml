type token =
  | ACCEPTS
  | ARROW 
  | MACHINE
  | TRANSITION
  | ENTRY
  | TERMINAL
  | NONTERMINAL
  | EOL
  | LBRACE
  | RBRACE
  | COMMA
  | EQ
  | STRING of string
  | IDENT of string
;;
