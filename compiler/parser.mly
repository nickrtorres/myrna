%{
type identifier = string

(*
 * This structure does not strictly match the definition of a state machine.
 * A state machine has a set of states, S, and an accepting state A that exists
 * S -- that is, the accepting state is just another state in S. To make
 * analysis simpler Accepting and State are separate constructors in the
 * machine_feature data structure.
 * 
 *)
type machine = identifier * machine_feature list
and machine_feature =
  | Transition of identifier * string
  | Entry of identifier * state_desc list
  | Accepting of identifier * state_desc list
  | State of identifier * state_desc list
and state_desc =
  identifier * identifier

type myrna_program = machine
%}

%token MACHINE TRANSITION LBRACE RBRACE EQ ARROW COMMA ENTRY ACCEPTING STATE
%token <string> STRING IDENT 
%start program
%type  <myrna_program> program

%%
program : machine                                              { $1 }
  ;

machine : MACHINE IDENT LBRACE machine_feature_list RBRACE     { ($2, $4) }
  ;

machine_feature_list : machine_feature                         { $1 :: [] }
                     | machine_feature_list machine_feature    { $2 :: $1 }
  ;

machine_feature : transition                                   { $1 }
                | entry                                        { $1 }
                | accepting                                    { $1 }
                | state                                        { $1 }
  ;

transition : TRANSITION IDENT EQ STRING                       { Transition($2, $4) }
  ;

state : STATE IDENT LBRACE state_desc_list RBRACE             { State($2, $4) }
  ;

accepting : ACCEPTING IDENT LBRACE state_desc_list RBRACE     { Accepting($2, $4) }
  ;

entry : ENTRY IDENT LBRACE state_desc_list RBRACE             { Entry($2, $4) }
  ;

state_desc_list : state_desc                                  { $1 :: [] }
                | state_desc_list state_desc                  { $2 :: $1 }
  ;

state_desc : IDENT ARROW IDENT COMMA                          { ($1, $3) }
  ;

%%
