{ 
  open Parser

  exception Eof
  exception SyntaxError of string

  let keyword_table = Hashtbl.create 53

  let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
                      [ "machine", MACHINE;
                        "transition", TRANSITION; ]
}

rule token = parse
    [' ' '\t' '\n']      { token lexbuf }
  | ['A' - 'Z' 'a' - 'z'] ['A' - 'Z'  'a' - 'z' '0' - '9' '_'] * as id
    { try
        Hashtbl.find keyword_table id
      with Not_found ->
        IDENT id }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | '='             { EQ }
  | '"'             { read_string (Buffer.create 17) lexbuf }
  | eof             { raise Eof }

(*
 * Taken from "Real World Ocaml":
 * https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html
 *)
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }


