%{
type identifier = string

type machine =
  | Machine of identifier * machine_feature list
and machine_feature =
  | Transition of identifier * string
  | Entry of identifier * state_desc list
  | Terminal of identifier * state_desc list
and state_desc =
  identifier * identifier

type myrna_program = machine list
%}

%token MACHINE TRANSITION LBRACE RBRACE EQ ARROW COMMA ENTRY TERMINAL
%token <string> STRING IDENT 
%start program
%type  <myrna_program> program

%%
program : machine                                       { $1 :: [] }
  ;

machine : MACHINE IDENT LBRACE machine_contents RBRACE  { Machine($2, $4) }
  ;

machine_contents : transition                           { $1 }
                 | entry                                { $1 }
  ;

transition : transition_entry                           { $1 }
  ;

transition_entry : transition_desc                      { $1 :: [] }
                 | transition_entry transition_desc     { $2 :: $1 }
  ;

transition_desc : TRANSITION IDENT EQ STRING            { Transition($2, $4) }
  ;

entry : ENTRY IDENT LBRACE state_desc_list RBRACE       { Entry($2, $4) :: [] }
  ;

state_desc_list : state_desc                            { $1 :: [] }
                | state_desc_list state_desc            { $2 :: $1 }
  ;

state_desc : IDENT ARROW IDENT COMMA                    { ($1, $3) }
  ;

%%
