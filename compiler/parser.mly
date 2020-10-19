%{
type identifier = string

type machine =
  | Machine of identifier * machine_feature list
and machine_feature =
  | Transition of identifier * string
and state_desc = string list

type myrna_program = machine list
%}

%token MACHINE TRANSITION LBRACE RBRACE EQ
%token <string> STRING IDENT
%start program
%type  <myrna_program> program

%%
program : machine                                       { $1 :: [] }
  ;

machine : MACHINE IDENT LBRACE machine_contents RBRACE  { Machine($2, $4) }
  ;

machine_contents : transition                           { $1 }
  ;

transition : transition_entry                           { $1 }
  ;

transition_entry : transition_desc                      { $1 :: [] }
                 | transition_entry transition_desc     { $2 :: $1 }
  ;

transition_desc : TRANSITION IDENT EQ STRING            { Transition($2, $4) }
  ;

%%
