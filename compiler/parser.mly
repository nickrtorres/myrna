%{
type identifier = string

type machine = identifier * machine_feature list
and machine_feature =
  | Transition of identifier * string
  | Entry of identifier * state_desc list
  | Terminal of identifier * state_desc list
  | NonTerminal of identifier * state_desc list
and state_desc =
  identifier * identifier

type myrna_program = machine list
%}

%token MACHINE TRANSITION LBRACE RBRACE EQ ARROW COMMA ENTRY TERMINAL NONTERMINAL
%token <string> STRING IDENT 
%start program
%type  <myrna_program> program

%%
program : machine                                              { $1 :: [] }
  ;

machine : MACHINE IDENT LBRACE machine_feature_list RBRACE     { ($2, $4) }
  ;

machine_feature_list : machine_feature                         { $1 :: [] }
                     | machine_feature_list machine_feature    { $2 :: $1 }
  ;

machine_feature : transition                                   { $1 }
                | entry                                        { $1 }
                | terminal                                     { $1 }
                | nonterminal                                  { $1 }
  ;

transition : TRANSITION IDENT EQ STRING                       { Transition($2, $4) }
  ;

nonterminal : NONTERMINAL IDENT LBRACE state_desc_list RBRACE { NonTerminal($2, $4) }
  ;

terminal : TERMINAL IDENT LBRACE state_desc_list RBRACE       { Terminal($2, $4) }
  ;

entry : ENTRY IDENT LBRACE state_desc_list RBRACE             { Entry($2, $4) }
  ;

state_desc_list : state_desc                                  { $1 :: [] }
                | state_desc_list state_desc                  { $2 :: $1 }
  ;

state_desc : IDENT ARROW IDENT COMMA                          { ($1, $3) }
  ;

%%
