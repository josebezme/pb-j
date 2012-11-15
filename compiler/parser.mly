(* parser.mly *)
%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token MAP ARRAY
%token STRING
%token <string> ID
%token EOF

